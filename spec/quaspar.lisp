(defpackage :quaspar.spec
  (:import-from :quaspar #:morton-cord)
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

;;;; Side-Effects:
; None

;;;; Notes:

;;;; Exceptional-Situations:

