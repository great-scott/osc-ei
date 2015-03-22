;
; decode.scm
;

(use s48-modules)
(include-relative "util.scm")

(define (decode-address normalized-input)
  (let* ((l (map integer->char normalized-input))
         (final (split-address l)))
    (list->string
      (filter
        (lambda (x) (not (char=? #\null x)))
        final))))

(define (split-address items)
  (if (null? items)
    '()
    (if (and (char? (car items)) (char=? (car items) #\,))
      '()
      (cons (car items)
            (split-address (cdr items))))))

