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
   )

  (import chicken scheme)
  (use udp6)

  (include "encode.scm")

  (define socket '())


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


  (define osc-close
    (lambda ()
      (udp-close-socket socket)
      (set! socket '())
      (print "Closing socket...")))

  )
