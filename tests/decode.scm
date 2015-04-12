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

(test "decode single type"
      (list #\i)
      (decode-type '(47 102 114 101 113 0 0 0 44 105 0 0 0 0 2 72)))

(test "decode int and string"
      (list #\i #\s)
      (decode-type
        '(47 102 114 101 113 0 0 0 44 105 115 0 0 0 2 72 0 0 104 97 108 108 111 0 0 0)))

(test-end "decode-tests")
