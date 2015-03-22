;
; decode.scm
;

(use test s48-modules)
(include-relative "../decode.scm")

(test-begin "decode-tests")

;    /   f   r   e   q         ,  i         584
;  '(47 102 114 101 113 0 0 0 44 105 0 0 0 0 2 72)

(test "decode basic address"
      "/freq"
      (decode-address '(47 102 114 101 113 0 0 0 44 105 0 0 0 0 2 72)))

(test "decode longer address"
      "/freq/osc"
      (decode-address '(47 102 114 101 113 47 111 115 99 0 0 0 44 105 0 0 0 0 2 72)))

(test-end "decode-tests")
