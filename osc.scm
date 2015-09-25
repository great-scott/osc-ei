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
  (use udp6 socket s48-modules srfi-18 srfi-69)

  (include-relative "encode.scm")
  (include-relative "decode.scm")


  ; main listener table
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
    (osc-listen-and-call socket (lambda (arg) arg)))


  (define (osc-listen-and-call socket proc)
    (thread-start!
     (lambda ()
       (let loop ()
        (thread-sleep! 0.05)
         (if (socket-receive-ready? socket)
             (let* ((received (udp-recv socket 1024))
                    (decoded (decode-packet-unnormalized received)))
               (evaluate-listener decoded)
               (proc decoded)))
         (loop)))))


  (define register-listener (make-register-listener-fn listener-table))

  ;;--------------------------------------------

  (define (make-register-listener-fn table)
    (lambda (pattern fn)
      (hash-table-set! table pattern fn)))

  (define evaluate-listener (make-evaluate-listener-fn listener-table))

  (define (make-evaluate-listener-fn table)
    (lambda (input)
        (evaluate-listener-with-table input table)))

  (define (evaluate-listener-with-table input table)
    (let* ((pattern (car input))
           (args (cdr input))
           (fn  (lambda (pattern)
                  (if (hash-table-exists? table pattern)
                    (hash-table-ref table pattern)
                    '()))))
      (if (not (null? (fn pattern)))
          (if (procedure? (fn pattern))
              (apply (fn pattern) args)
              ((eval (fn pattern)) args)))))
)
