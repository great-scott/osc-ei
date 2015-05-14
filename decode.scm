;
; decode.scm
;

(use s48-modules)
(include-relative "util.scm")

(define (decode-packet normalized-input)
  (let* ((s (split normalized-input 44))
         (address (decode-str (car s)))
         (types (decode-type normalized-input))
         (padding (get-padding-amount (append (list ",") types)))
         (message (decode-message
                    (slice (cadr s)
                           (+ 1 (length types) padding)
                           (length (cadr s)))
                    types)))

      (append (list address) message)))


(define (decode-address normalized-input)
  (decode-str normalized-input))


(define (decode-type normalized-input)
  (let* ((l (map integer->char normalized-input))
         (rest (cdr (cadr (split l #\,)))))
    (let find-types ((types rest))
     (if (char=? (car types) #\null)
       '()
       (cons (car types) (find-types (cdr types)))))))


(define (decode-int normalized-input)
  (car
   (s32vector->list
    (blob->s32vector
      (u8vector->blob
        (list->u8vector (reverse normalized-input)))))))


(define (decode-float normalized-input)
  (car
    (f32vector->list
      (blob->f32vector
      (u8vector->blob
        (list->u8vector (reverse normalized-input)))))))


(define (decode-str normalized-input)
   (list->string
    (let collect-char ((input normalized-input))
     (if (null? input)
       '()
       (let ((current (car input)))
        (if (equal? current 0)
          '()
          (cons (integer->char current) (collect-char (cdr input)))))))))


(define (decode-message normalized-input types)
  (if (or (null? normalized-input) (null? types))
    '()
    (let* ((type (car types))
           (decode-fn (get-type-decode-fn type))
           (to-decode
             (if (equal? type #\s)
               (split-string-preserve-alignment normalized-input 0)
               ((lambda (i)
                  (list (slice i 0 4)
                        (slice i 4 (length i))))
                normalized-input))))
      (cons
        (decode-fn (car to-decode))
        (decode-message (cadr to-decode) (cdr types))))))


(define (get-type-decode-fn arg)
  (cond
    ((equal? arg #\i) decode-int)
    ((equal? arg #\s) decode-str)
    ((equal? arg #\f) decode-float)))

