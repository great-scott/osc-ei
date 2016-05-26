;
; encode.scm
;

(use test)
(load-relative "../encode.scm")

(define (last-byte buf)
  (list-ref buf (- (length buf) 1)))

(define ENC-STRING (encode-str ",s"))
(define ENC-INT (encode-str ",i"))
(define ENC-FLOAT (encode-str ",f"))

(test-begin "encode-tests")

(test "encode basic string"
      '(104 97 108 108 111 0 0 0)
      (encode-str "hallo"))

(test "check that string ends with zero"
      0
      (last-byte (encode-str "morgen")))

(test "length is a multiple of four"
      0
      (modulo (length (encode-str "somethingfoobar")) 4))

(test "string type length is multiple of four"
      0
      (modulo (length (encode-str ",s")) 4))

(test "check basic encoding of int32"
      '(255 254 188 24)
      (encode-int -82920))

(test "interpret one string argument"
      ENC-STRING
      (encode-type '("gutenmorgen")))

(test "interpret one int argument"
      ENC-INT
      (encode-type '(-372)))

(test "interpret one flaot argument"
      ENC-FLOAT
      (encode-type '(84.19)))

(test "interpret many arguments"
      (encode-str ",siif")
      (encode-type '("hi" 1 28 -0.53)))

(test-end "encode-tests")
