;
; osc.scm
;


(module
  osc
  ; declarations
  (osc-connect
   osc-send
   osc-close
   osc-receive
   )

  (import chicken scheme)
  (use udp6 s48-modules srfi-18)

  (include-relative "encode.scm")
  (include-relative "decode.scm")


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
             (encoded (list->s8->blob (append encoded-address encoded-type encoded-message))))

        (udp-send socket encoded))))


  (define osc-close
    (lambda (socket)
      (udp-close-socket socket)
      (print "Closing socket...")))


  (define (osc-receive socket)
    (thread-start!
      (lambda ()
        (let loop ()
         (let ((received (udp-recv socket 1024)))
          (print (decode-packet (map char->integer (string->list received)))))
         (loop)))))

  )
