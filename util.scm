;
; util.scm
;

(use srfi-1 srfi-4)


(define (str->int-list str)
  (map char->integer (string->list str)))


(define (list->s8->blob stuff)
  (s8vector->blob (list->s8vector stuff)))


(define (get-padding-amount buffer)
  (- 4 (modulo (length buffer) 4)))


(define (pad buffer)
  (let ((extra (get-padding-amount buffer)))
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


(define (slice items begin end)
  (take (drop items begin) (- end begin)))


(define (split items token)
  (let ((index (list-index (lambda (item) (equal? item token)) items)))
   (list (take items index) (drop items index))))


(define (split-string-preserve-alignment items token)
  (let* ((raw-split (split items token))
         (str (car raw-split))
         (padding (get-padding-amount str))
         (rest (slice items (+ (length str) padding) (length items))))
    (list str rest)))


(define (message? possible-message-list)
  (string? (car possible-message-list)))
