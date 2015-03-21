;
; util.scm
;

(use srfi-1 srfi-4)


(define (str->int-list str)
  (map char->integer (string->list str)))


(define (list->s8->blob stuff)
  (s8vector->blob (list->s8vector stuff)))


(define (pad buffer)
  (let ((extra (- 4 (modulo (length buffer) 4))))
   (if (< extra 4)
     (append buffer (make-list extra 0))
     buffer)))


(define (pack-bytes bytes vec->blob list->vec)
  (reverse
    (u8vector->list
      (blob->u8vector
        (vec->blob (list->vec (list bytes)))))))


(define (flatten lst)
  (cond
    ((null? lst) '())
    ((pair? lst) (append (flatten (car lst)) (flatten (cdr lst))))
    (else (list lst))))


(define (get-type-str arg)
  (cond
    ((string? arg) #\s)
    ((flonum? arg) #\f)
    ((integer? arg) #\i)))

