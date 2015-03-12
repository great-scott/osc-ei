;
; osc.scm
;

(module 
  osc 
  ; declarations
  (encode-int 
   encode-str
   encode-float
   encode-type
   osc-connect
   osc-send
   osc-close
   server
   )

  (import chicken scheme)
  (use srfi-1 srfi-4 udp6)

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

  (define (get-type-enc-fn arg)
    (cond 
      ((string? arg) encode-str)
      ((flonum? arg) encode-float)
      ((integer? arg) encode-int)))

  (define (flatten lst)
    (cond
      ((null? lst) '())
      ((pair? lst) (append (flatten (car lst)) (flatten (cdr lst))))
      (else (list lst))))

  (define (collect-messages args)
    (let ((r (map (lambda (x) ((get-type-enc-fn x) x)) args)))
     (flatten r)))

  (define (get-type-str arg)
    (cond 
      ((string? arg) #\s)
      ((flonum? arg) #\f)
      ((integer? arg) #\i)))

  ;------------------------------------------------------------------
  
  (define socket '())
  
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

  (define (osc-connect port)
    (if (not (null? socket))
      (udp-close-socket socket))

    (set! socket (udp-open-socket))
    (udp-connect! socket "localhost" port)
    (print "Connected..."))

  (define (osc-send . body)
    (let ((address (car body))
          (message (cdr body)))
      
      (let* ((encoded-address (encode-str address))
             (encoded-message (collect-messages message))
             (encoded-type (encode-type message))
             (encoded 
               (append encoded-address encoded-type encoded-message)))
        (udp-send socket (list->s8->blob encoded)))))

  (define close
    (lambda ()
      (udp-close-socket socket)
      (set! socket '())
      (print "Closing socket...")))

  )
