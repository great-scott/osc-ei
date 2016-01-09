;;
;; osc.scm
;;


(module
  osc
  ;; declarations
  (osc-connect
   osc-server
   osc-send
   osc-close
   osc-listen
   osc-listen-and-call
   register-listener
   )

  (import chicken scheme)
  (use udp6 socket s48-modules srfi-18 srfi-69 records)

  (include-relative "encode.scm")
  (include-relative "decode.scm")
  (include-relative "util.scm")

  ;; alias thread for our listener and associate it with socket
  (define-record osc-listener-manager socket table thread)

  ;; main listener table
  (define listener-table (make-hash-table))

  (define (osc-connect port)
    (let ((socket (udp-open-socket)))
     (udp-connect! socket "localhost" port)
     (print "Connected..." (udp-bound-port socket))

     socket))


  (define (osc-server port)
    (let ((socket (udp-open-socket)))
     (udp-bind! socket "localhost" port)

     socket))


  (define (encode-and-send socket body)
    (let ((address (car body))
          (message (cdr body)))

      (let* ((encoded-address (encode-str address))
             (encoded-message (collect-messages message))
             (encoded-type (encode-type message))
             (encoded (list->s8->blob (append encoded-address encoded-type encoded-message))))
        (udp-send socket encoded))))


  (define (osc-send socket messages)
    (if (message? messages)
        (encode-and-send socket messages)
        (for-each (lambda (message) (encode-and-send socket message)) messages)))


  (define osc-close
    (lambda (socket)
      (udp-close-socket socket)
      (print "Closing socket...")))


  (define (evaluate-listener table input)
    (let* ((pattern (car input))
           (args (cdr input))
           (fn  (lambda (pattern)
                  (if (hash-table-exists? table pattern)
                    (hash-table-ref table pattern)
                    '()))))
      (if (not (null? (fn pattern)))
          (if (procedure? (fn pattern))
              (apply (fn pattern) (list args))
              ((eval (fn pattern)) args)))))


  (define (osc-listen socket)
    (osc-listen-and-call socket (lambda (arg) arg)))


  (define (osc-listen-and-call socket proc)
    (let ((table listener-table))
      (make-osc-listener-manager
       socket
       table
       (thread-start!
        (lambda ()
          (let loop ()
            (if (and (udp-bound? socket) (socket-receive-ready? socket))
                (let* ((received (udp-recv socket 1024))
                       (decoded (decode-packet-unnormalized received)))
                  (evaluate-listener table decoded)
                  (proc decoded)
                  (loop)))
            (thread-sleep! 0.05)
            (loop)))))))


  (define (register-listener manager pattern callback)
    (hash-table-set! (osc-listener-manager-table manager) pattern callback)
    manager)

)
