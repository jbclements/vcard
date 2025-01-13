#lang racket

(require sugar
         csv-writing
         scramble/regexp
         #;"address-hints-private.rkt"
         "name-hints-private.rkt")

;; a vcard is a hash from strings to lists of lists of strings.
;; the outer list is because there can be multiple instances of the
;; same key; the inner one is because fields can be multiple lines.

(define christmas-vcf-path (build-path "/Users/clements/git-clements/addresses/christmas-2024.vcf"))

(define (card-name c) (first (hash-ref c "N")))

(define (file->unique-named-cards filename)
  (define lines (file->lines filename))

  ;; lists of lines where the first one is not indented
  (define non-indenteds
    (slicef-at lines (λ (s) (regexp-match #px"^[^ ]" s))))

  (define vcard-line-bundles
    (slicef-at non-indenteds (λ (b)
                               (equal? b '("BEGIN:VCARD")))))

  (define vcards
    (for/list ([v (in-list vcard-line-bundles)])
      (match v
        [(list (list "BEGIN:VCARD")
               (list "VERSION:3.0")
               (list (regexp #px"^PRODID:-//Apple Inc\\.//macOS [.0-9]+//EN$"))
               contents ...
               (list "END:VCARD"))
         (define initial-parse
           (for/list ([b (in-list contents)])
             (match (first b)
               [(regexp #px"^([^:]+):(.*)$" (list _ label first-line-rest))
                (list label first-line-rest (rest b))])))
         ;; yep, field names can be duplicated....
         #;(define duplicated-name (check-duplicates (map first initial-parse)))
         #;(when duplicated-name
             (error 'parse-vcard "duplicated field name: ~e in card ~e" duplicated-name
                    initial-parse))

         (make-immutable-hash
          (map (λ (g)
                 (cons (first (first g))
                       (map (λ (elt) (cons (second elt) (third elt))) g)))
               (group-by first initial-parse)))]
        [other
         (error 'parse-vcards "non")])))

  (printf "this file appears to contain ~v vcards.\n" (length vcards))

  ;; everyone has an "N" field. It's the only common one.

  ;; sigh... cards with empty names...
  (define-values (no-name-cards named-cards)
    (partition (λ (card) (equal? (hash-ref card "N") '((";;;;")))) vcards))

  (when (not (empty? no-name-cards))
    (printf "ignoring these ~v cards that have no N field, all businesses I hope?\n"
            (length no-name-cards))
    (pretty-display no-name-cards))
  
  

  (when (ormap (λ (c) (not (= 1 (length (hash-ref c "N"))))) named-cards)
    (error 'not-all-single-named "more info here"))

  ;; now all cards have exactly one name

  
  ;; duplicates?

  (define duplicated-names
    (map first
         (filter (λ (g) (< 1 (length g)))
                 (group-by (λ (x) x) (map card-name named-cards)))))

  (unless (subset? duplicated-names '(("Clements;John;;;")))
    (error 'zzz "unexpected duplicated names: ~v"
           duplicated-names))

  (define unique-named-cards
    (filter (λ (c) (not (equal? (card-name c) '("Clements;John;;;")))) named-cards))

  unique-named-cards)

;(define all-cards (file->unique-named-cards "/tmp/all-cards.vcf"))
(define cc-cards (file->unique-named-cards christmas-vcf-path))



;(define all-card-names (map card-name all-cards))
(define cc-card-names (map card-name cc-cards))

(define-RE dot (inject "."))

(define (find-main-address card)
  (define possible-key-pairs
    (filter-map
     (λ (key)
       (define split (regexp-split (px ";") key))
       (define f (first split))
       (and (or (equal? f "ADR")
                (regexp-match (px ".ADR" $) f))
            (list key
                  (map (λ (f)
                         (match f
                           [(regexp (px ^ "type=" (report (* dot)) $) (list _ ty))
                            ty]
                           [other (error 'BADPARSE)]))
                       (rest split)))))
     (hash-keys card)))
  (define home-keys
    (filter (λ (pr)
              (and (subset? '("HOME") (second pr))))
            possible-key-pairs))
  (match home-keys
    [(list) (list 'NOHOME possible-key-pairs)]
    [(list (list key _)) (hash-ref card key)]
    [other (list '2HOMES other)]))

(map (λ (card)
       (list (card-name card)
             (find-main-address card)))
     cc-cards)


(define (undo-string-quotes s)
  (regexp-replace* #px"\\\\," s ","))

(define (find-address card)
  (define main-address (find-main-address card))
  (match main-address
    [(cons 'NOHOME _) #f]
    [(list (list (? string? s)))
     (undo-string-quotes s)]))

(define (address-line-split addr)
  (match (regexp-split #px";" addr)
    [(list "") '(empty)]
    [(list "" "" street city state zip country)
     (define country-str
       (match country
         [(or "" "USA" "United States") ""]
         ["Canada" ",  Canada"]))
     (list street
           (~a city ", " state "  " zip country-str))]))

(define (name-render fn)
  (match (assoc fn rename-hints)
    [#f (error 'name-lookup "missing rename for name: ~e" fn)]
    [(list _ (? string? s)) s]
    [(list _ 'skip) 'skip]
    [(list _ 'no-address) (printf "hand-deliver to person with no address: ~e\n"
                                  fn)
                          'skip]
    [(list (list (list n)) 'same) n]
    [(list (list (list n)) (? symbol? s))
     (error 'name-lookup "unrecognized behavior symbol ~e for name ~e"
            s fn)]
    [other (error "badly formatted : ~e" other)]))

(call-with-output-file "/tmp/christmas.csv"
  #:exists 'truncate
  (λ (port)
    (display-table
     (cons (list "name" "street" "rest")
     (filter
      (λ (row) (string? (first row)))
      (map
       (λ (card)
         (define addr (find-address card))
         (cons (name-render (hash-ref card "FN"))
               (and addr (address-line-split addr))))
       cc-cards)))
     port)))

#;(unless (subset? cc-card-names all-card-names)
  (error 'zzz "problem1143"))


(define cc-card-names-set (list->set cc-card-names))

#;(call-with-output-file "/tmp/is-christmas.csv"
  (λ (port)
    (display-table
     (for/list ([n (sort all-card-names #:key first string<?)])
       (list (first n) (cond [(set-member? cc-card-names-set n) "TRUE"]
                             [else ""])))
     port)))


#;(call-with-output-file "christmas.csv"
  (λ (port)
    (display-table
     (for/list ([n (sort all-card-names #:key first string<?)])
       (list (first n) (cond [(set-member? cc-card-names-set n) "TRUE"]
                             [else ""])))
     port)))


#;(check-duplicates (map (λ (vcard) (hash-ref vcard "N")) vcards))


;; checking
#;(unless (andmap (λ (vcard) (<= 3 (length vcard))) vcards)
  (error 'problem "moreinfo1"))



;; page printing instructions:
;; - open microsoft word
;; - Mailings > Start Mail Merge > Labels ...
;; - Avery 5160, click ok
;; select recipients > use existing list
;; select csv file
;; Use Insert Merge Field 3x to insert the 3 rows
;; Click "Update Labels"
;; Click "preview"
;; Finish & Merge > Print Documents ...
;; (figure out how to load paper correctly)