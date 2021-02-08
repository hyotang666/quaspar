(defpackage :quaspar.spec
  (:shadowing-import-from :quaspar space delete)
  (:import-from :quaspar #:morton-cord #:bit-separate #:smallest-space-index #:depth
                #:space-local-index)
  (:use :cl :jingoh :quaspar))
(in-package :quaspar.spec)
(setup :quaspar)

(requirements-about MORTON-CORD :doc-type function)

;;;; Description:

#+syntax (MORTON-CORD x y w h &optional (depth *depth*)) ; => result

#?(MORTON-CORD 48 80 100 100 3)
:values (3 6)
#?(morton-cord 55 15 80 80 3) :values (5 1)
#?(morton-cord 75 35 80 80 3) :values (7 3)

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
(requirements-about smallest-space-index :doc-type function)

;;;; Description:
; Convert morton cordinates to linear local morton space index.

#+syntax (smallest-space-index x y) ; => result

;;;; Arguments and Values:

; x := (unsigned-byte 16)

; y := (unsigned-byte 16)

; result := (unsigned-byte 32)

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

#?(loop :for h :below 8
        :collect (loop :for w :below 8
                       :collect (smallest-space-index w h)))
=> (( 0  1  4  5 16 17 20 21)
    ( 2  3  6  7 18 19 22 23)
    ( 8  9 12 13 24 25 28 29)
    (10 11 14 15 26 27 30 31)
    (32 33 36 37 48 49 52 53)
    (34 35 38 39 50 51 54 55)
    (40 41 44 45 56 57 60 61)
    (42 43 46 47 58 59 62 63))
,:test equal

#?(uiop:while-collecting (acc)
    (dotimes (h 8)
      (dotimes (w 8)
        (acc (smallest-space-index w h)))))
:satisfies (lambda (x)
             (null (set-difference x (loop :for i :below (expt 8 2)
                                           :collect i))))

(requirements-about DEPTH :doc-type function)

;;;; Description:

#+syntax (DEPTH left-top right-bottom) ; => result

; When left-top and right-bottom have same index, it ocupy smallest space.
#?(depth 0 0) => 0

; Secondary smallest box.
#?(depth 0 1) => 1
#?(depth 0 2) => 1
#?(depth 0 3) => 1
#?(depth 2 3) => 1

#?(depth 0 12) => 2
#?(depth 0 16) => 3
#?(depth 5 16) => 3

;;;; Arguments and Values:

; left-top := 

; right-bottom := 

; result := 

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about SPACE-LOCAL-INDEX :doc-type function)

;;;; Description:

#+syntax (SPACE-LOCAL-INDEX left-top ocupied-space-depth) ; => result

; When depth is smallest (i.e. zero), left-top is returned.
#?(space-local-index 0 0) => 0
#?(space-local-index 10 0) => 10

; Secandary smallest space index.
#?(space-local-index 1 1) => 0
#?(space-local-index 2 1) => 0
#?(space-local-index 3 1) => 0
#?(space-local-index 4 1) => 1
#?(space-local-index 4 2) => 0
;;;; Arguments and Values:

; left-top := 

; ocupied-space-depth := 

; result := 

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about OUT-OF-SPACE :doc-type TYPE)

;;;; Description:
; Signaled when RECT over LQTREE max range.
;;;; Class Precedence List: (case in SBCL)
; out-of-space cell-error error serious-condition condition slot-object t

;;;; Effective Slots:

; POINT [Type] T
; [READER] point

; RANGE [Type] T
; [READER] range

; MAX [Type] T
; [READER] max-of

; NAME [Type] T
; [READER] cell-error-name

;;;; Notes:

(requirements-about RECT :doc-type TYPE)

;;;; Description:
; The default rect object for LQTREE-STORABLE.
;;;; Class Precedence List: (case in SBCL)
; rect standard-object slot-object t

;;;; Effective Slots:

; X [Type] (INTEGER 0 *)
; [ACCESSOR] x

; Y [Type] (INTEGER 0 *)
; [ACCESSOR] y

; W [Type] (INTEGER 0 *)
; [READER] w

; H [Type] (INTEGER 0 *)
; [READER] h

;;;; Notes:

(requirements-about MAKE-RECT :doc-type function)

;;;; Description:
; The default rect-constructor for LQTREE-STORABLE make-instance &key parameter :rect-constructor.  

#+syntax (MAKE-RECT &key (x 0) (y 0) (w 0) (h 0)) ; => result

#?(make-rect) :be-the rect

;;;; Arguments and Values:

; x := unsigned-byte

; y := unsigned-byte

; w := unsigned-byte

; h := unsigned-byte

; result := rect

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about LQTREE-STORABLE :doc-type TYPE)

;;;; Description:
; Inherit this to store object for lqtree.
;;;; Class Precedence List: (case in SBCL)
; lqtree-storable standard-object slot-object t

;;;; Effective Slots:

; INDEX [Type] T
; [ACCESSOR] index
; Morton space index

; RECT [Type] T
; [READER] rect

; PREV [Type] (OR NULL LQTREE-STORABLE)
; [ACCESSOR] prev
; Previous object of the list.

; NEXT [Type] (OR NULL LQTREE-STORABLE)
; [ACCESSOR] next
; Next object of the list.

;;;; Notes:
; To constract it, you must specify MAX-W and MAX-H as keyword parameter for MAKE-INSTANCE.
