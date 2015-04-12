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


(define (slice items first last)
  (let ((split-items (list-tail items first)))
   (let remove-end ((remaining-items split-items)
                    (start-index first))
    (if (or (null? remaining-items) (= start-index last))
      '()
      (cons
        (car remaining-items)
        (remove-end (cdr remaining-items) (+ start-index 1)))))))


(define (split items token)
  (let ((reversed-list (reverse items)))
   (reverse
     (let inner-loop ((rl reversed-list))
      (let ((check-against (car rl)))
       (if (or (null? rl) (equal? check-against token))
         '()
         (cons check-against (inner-loop (cdr rl)))))))))

