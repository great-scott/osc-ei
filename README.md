# osc-ei
Open Sound Control library for Chicken Scheme

### Building

```sh
csc -s ./osc.setup
chicken-install
```

### api
This is a work in progress, so it's possible the api will change.

```
; returns socket object that's necessary for other procedure calls
[procedure] (osc-connect port)

; send osc formatted message (message-body) to the specified socket
[procedure] (osc-send socket . message-body)

; spawns a thread to listen to messages sent to socket's internal port
; the socket's port is not the port used in (osc-connect port)
[procedure] (osc-listen socket)

; similar to the bare osc-listen procedure, but always calls the
; input procedure
[procedure] (osc-listen-and-call socket procedure)

; registers an osc-address-pattern with a callback procedure
; the procedure can be a reference to a procedure or a quoted
; procedure
[procedure] (register-listener osc-address-pattern procedure)

; closes socket
[procedure] (osc-close socket)
```

### example
```scheme
(use osc)

(define client-socket (osc-connect 8000))
(define server-socket (osc-server 8000))

(osc-listen-and-call server-socket (lambda (msg) (print msg))

(osc-send client-socket "/freq" 440)

; output
> (/freq 440)
```

### listener example
```scheme
(use osc)

(define client-socket (osc-connect 8005))
(define server-socket (osc-server 8005))
(osc-listen server-socket)

(define (freq-callback freq) (print "wooo " freq))

; uses quoted version of the procedure, but passing in just
; freq-callback also works
(register-listener "/freq" 'freq-callback)

(osc-send client-socket "/freq" 440)

; output
wooo (440)
```
