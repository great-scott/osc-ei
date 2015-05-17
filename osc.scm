;
; osc.scm
;


(module
  osc
  ; declarations
  (osc-connect
   osc-send
   osc-close
   )

  (import chicken scheme)
  (use udp6 s48-modules)

  (include-relative "encode.scm")


  (define (osc-connect port)
    (let ((socket (udp-open-socket)))
     (udp-connect! socket "localhost" port)
     (print "Connected..." (udp-bound-port socket))

    socket))

  (define (osc-send socket . body)
    (let ((address (car body))
          (message (cdr body)))

      (let* ((encoded-address (encode-str address))
             (encoded-message (collect-messages message))
             (encoded-type (encode-type message))
             (encoded
               (append encoded-address encoded-type encoded-message)))
        (udp-send socket (list->s8->blob encoded)))))


  (define osc-close
    (lambda (socket)
      (udp-close-socket socket)
      (print "Closing socket...")))

  )
