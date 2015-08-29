;
; osc.scm
;


(module
  osc
  ; declarations
  (osc-connect
   osc-server
   osc-send
   osc-close
   osc-listen
   )

  (import chicken scheme)
  (use udp6 socket s48-modules srfi-18)

  (include-relative "encode.scm")
  (include-relative "decode.scm")


  (define (osc-connect port)
    (let ((socket (udp-open-socket)))
     (udp-connect! socket "localhost" port)
     (print "Connected..." (udp-bound-port socket))

     socket))


  (define (osc-server port)
    (let ((socket (udp-open-socket)))
     (udp-bind! socket "localhost" port)

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


  (define (osc-listen socket)
    (thread-start!
      (lambda ()
        (let loop ()
         (if (socket-receive-ready? socket)
             (let* ((received (udp-recv socket 1024))
                    (decoded (decode-packet (map char->integer (string->list received)))))
               (print decoded)
             (thread-sleep! 0.05))
         (loop)))))

  )
