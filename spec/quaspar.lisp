(defpackage :quaspar.spec
  (:import-from :quaspar #:morton-cord #:bit-separate #:linear-index)
  (:use :cl :jingoh :quaspar))
(in-package :quaspar.spec)
(setup :quaspar)

(requirements-about MORTON-CORD :doc-type function)

;;;; Description:

#+syntax (MORTON-CORD x y w h &optional (depth *depth*)) ; => result

#?(MORTON-CORD 48 80 100 100 3)
:values (3 6)

;;;; Arguments and Values:

; x := unsigned-byte, otherwise signals condition depending on implementation.
#?(morton-cord -1 0 10 10 3) :signals condition

; y := unsigned-byte, otherwise signals condition depending on implementation.
#?(morton-cord 0 -1 10 10 3) :signals condition

; w := unsigned-byte, otherwise signals condition depending on implementation.
#?(morton-cord 0 0 -1 10 3) :signals condition

; h := unsigned-byte, otherwise signals condition depending on implementation.
#?(morton-cord 0 0 10 -1 3) :signals condition

; depth := unsigned-byte, otherwise signals condition depending on implementation.
#?(morton-cord 0 0 10 10 -3) :signals condition

; result 1 := unsigned-byte

; result 2 := unsigned-byte

;;;; Affected By:
; *DEPTH*
#?(morton-cord 48 80 100 100 0) :values (0 0) ; (array * (1))
#?(morton-cord 48 80 100 100 1) :values (0 1) ; (array * (2 2))
#?(morton-cord 48 80 100 100 2) :values (1 3) ; (array * (4 4))
#?(morton-cord 48 80 100 100 3) :values (3 6) ; (array * (8 8))

;;;; Side-Effects:
; None

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about BIT-SEPARATE :doc-type function)

;;;; Description:

#+syntax (BIT-SEPARATE integer) ; => result

;;;; Arguments and Values:

; integer := (unsigned-byte 8)

; result := (unsigned-byte 16)

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

;;;; TEST
#?(bit-separate #B111) => #B010101
#?(bit-separate #B1111) => #B01010101
#?(bit-separate #xFF) => #B0101010101010101
#?(bit-separate #xFFFF) => #B01010101010101010101010101010101
#?(bit-separate  0) => 0
#?(bit-separate  1) => 1
#?(bit-separate  #B10) => #B0100
#?(bit-separate -1) :signals condition

(requirements-about LINEAR-INDEX :doc-type function)

;;;; Description:
; Convert morton cordinates to linear morton index.

#+syntax (LINEAR-INDEX x y) ; => result

;; Level 0 or more
#?(linear-index 0 0) => 0
;; Level 1.
#?(uiop:while-collecting (acc)
    (dotimes (x 2)
      (dotimes (y 2)
        (acc (linear-index x y)))))
:satisfies (lambda (x)
             (null
               (set-difference x (loop :for i :below (expt 4 1) :collect i))))
;; Level 2.
#?(uiop:while-collecting (acc)
    (dotimes (x 4)
      (dotimes (y 4)
        (acc (linear-index x y)))))
:satisfies (lambda (x)
             (null
               (set-difference x (loop :for i :below (expt 4 2) :collect i))))
;; Level 3.
#?(uiop:while-collecting (acc)
    (dotimes (x 8)
      (dotimes (y 8)
        (acc (linear-index x y)))))
:satisfies (lambda (x)
             (null
               (set-difference x (loop :for i :below (expt 4 3) :collect i))))

;;;; Arguments and Values:

; x := (unsigned-byte 16)

; y := (unsigned-byte 16)

; result := (unsigned-byte 32)

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

