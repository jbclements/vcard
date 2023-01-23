#lang racket

(require sugar
         csv-writing)

;; a vcard is a hash from strings to lists of lists of strings.
;; the outer list is because there can be multiple instances of the
;; same key; the inner one is because fields can be multiple lines.

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
               ;; this will break for newer versions of the OS, I'm just being conservative
               ;; and exploring
               (list "PRODID:-//Apple Inc.//macOS 12.6//EN")
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
         (error 'zzz "info")])))

  (printf "this file appears to contain ~v vcards.\n" (length vcards))

  ;; everyone has an "N" field. It's the only common one.

  ;; sigh... cards with empty names...
  (define-values (no-name-cards named-cards)
    (partition (λ (card) (equal? (hash-ref card "N") '((";;;;")))) vcards))

  (printf "ignoring these ~v cards that have no N field, all businesses I hope?\n"
          (length no-name-cards))
  (pretty-display no-name-cards)

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

(define all-cards (file->unique-named-cards "/tmp/all-cards.vcf"))
(define cc-cards (file->unique-named-cards "/tmp/christmas.vcf"))

(define all-card-names (map card-name all-cards))
(define cc-card-names (map card-name cc-cards))

(unless (subset? cc-card-names all-card-names)
  (error 'zzz "problem1143"))

(for ([n (in-list all-card-names)])
  (when (not (= (length n) 1))
    (error 'zz2 "tuhhoth")))

(define cc-card-names-set (list->set cc-card-names))

(call-with-output-file "/tmp/is-christmas.csv"
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



