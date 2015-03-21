;
; encode.scm
;

(use s48-modules)
(include-relative "util.scm")

(define (encode-type args)
  (encode-str
    (list->string (append (list #\,) (map get-type-str args)))))


(define (encode-float fl)
  (pack-bytes fl f32vector->blob list->f32vector))


(define (encode-int int)
  (pack-bytes int s32vector->blob list->s32vector))


(define (encode-str str)
  (let ((converted (if (string? str)
                     (str->int-list str)
                     (map char->integer str))))
    (pad (append converted '(0)))))


(define (get-type-enc-fn arg)
  (cond
    ((string? arg) encode-str)
    ((flonum? arg) encode-float)
    ((integer? arg) encode-int)))


(define (collect-messages args)
  (let ((r (map (lambda (x) ((get-type-enc-fn x) x)) args)))
   (flatten r)))
