(defpackage :quaspar.spec
  (:shadowing-import-from :quaspar space delete)
  (:import-from :quaspar #:morton-cord #:bit-separate #:smallest-space-index #:depth
                #:space-local-index #:linear-quad-length)
  (:use :cl :jingoh :quaspar))
(in-package :quaspar.spec)
(setup :quaspar)

(requirements-about LINEAR-QUAD-LENGTH :doc-type function)

;;;; Description:
; Compute backend vector length.

#+syntax (LINEAR-QUAD-LENGTH depth) ; => result

#?(linear-quad-length 0) => 1
#?(linear-quad-length 1) => 5
#?(linear-quad-length 2) => 21
;;;; Arguments and Values:

; depth := (integer 0 *)

; result := (integer 1 *)

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

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

;;;; Effective Slots:

; RECT [Type] T
; [READER] rect

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

(requirements-about STORE :doc-type function)

;;;; Description:
; Store lqtree-storable object to internal space data object.

#+syntax (STORE storable space) ; => result

#?(store (make-instance 'lqtree-storable :max-w 100 :max-h 100)
         (quaspar::make-space))
:satisfies (lambda (o)
             (& (typep o 'lqtree-storable)
                (= 21 (index o))
                (typep (rect o) 'rect)
                (= 0 (x (rect o)))
                (= 0 (y (rect o)))
                (= 0 (w (rect o)))
                (= 0 (h (rect o)))
                (null (next o))
                (null (prev o))))

; Stored object is pushed last.
; Objects are linked.
#?(let ((space (quaspar::make-space)))
    (store (make-instance 'lqtree-storable :max-w 100 :max-h 100)
           space)
    (store (make-instance 'lqtree-storable :x 1 :max-w 100 :max-h 100)
           space)
    space)
:satisfies (lambda (o)
             (& (typep o 'quaspar::space)
                (typep (quaspar::space-contents o) 'lqtree-storable)
                (null (prev (quaspar::space-contents o)))
                (typep (next (quaspar::space-contents o)) 'lqtree-storable)
                (eq (prev (next (quaspar::space-contents o)))
                    (quaspar::space-contents o))
                (null (next (next (quaspar::space-contents o))))
                (= 0 (x (rect (quaspar::space-contents o))))
                (= 1 (x (rect (next (quaspar::space-contents o)))))))

;;;; Arguments and Values:

; storable := lqtree-storable, otherwise signals condition depends on implementation.
#?(store :not-storable-object (quaspar::make-space)) :signals condition

; space := (or (cons null null) (cons lqtree-storable lqtree-storable))
; This internal object is automatically constructed by LQTREE.

; result := lqtree-storable

;;;; Affected By:
; none

;;;; Side-Effects:
; Modify storable object and space object.

;;;; Notes:
; STORE just do storing, never care anything else.

;;;; Exceptional-Situations:

(requirements-about DELETE-FROM-SPACE :doc-type function
                    :around (let ((space (quaspar::make-space))
                                  first between last)
                              (declare (ignorable first between last))
                              (store (setf first (make-instance 'lqtree-storable :max-w 100 :max-h 100))
                                     space)
                              (store (setf between (make-instance 'lqtree-storable :x 1 :max-w 100 :max-h 100))
                                     space)
                              (store (setf last (make-instance 'lqtree-storable :x 2 :max-w 100 :max-h 100))
                                     space)
                              (call-body)))

;;;; Description:

#+syntax (DELETE-FROM-SPACE storable space) ; => result

; Case first.
#?(progn (delete-from-space first space)
         space)
:satisfies (lambda (o)
             (& (typep o 'quaspar::space)
                (= 1 (x (rect (quaspar::space-contents o))))
                (null (prev (quaspar::space-contents o)))
                (= 2 (x (rect (next (quaspar::space-contents o)))))))

; Case betwee.
#?(progn (delete-from-space between space)
         space)
:satisfies (lambda (o)
             (& (typep o 'quaspar::space)
                (= 0 (x (rect (quaspar::space-contents o))))
                (= 2 (x (rect (next (quaspar::space-contents o)))))
                (= 0 (x (rect (prev (next (quaspar::space-contents o))))))))

; Case last
#?(progn (delete-from-space last space)
         space)
:satisfies (lambda (o)
             (& (typep o 'quaspar::space)
                (= 0 (x (rect (quaspar::space-contents o))))
                (= 1 (x (rect (next (quaspar::space-contents o)))))
                (null (next (next (quaspar::space-contents o))))
                (eq (quaspar::space-last o)
                    (next (quaspar::space-contents o)))))

; Case only one element.
#?(let ((space (quaspar::make-space)) a)
    (store (setf a (make-instance 'lqtree-storable :max-w 10 :max-h 10)) space)
    (delete-from-space a space)
    space)
:satisfies (lambda (o)
             (& (typep o 'space)
                (equal o '(nil . nil))))
;;;; Arguments and Values:

; storable := lqtree-storable

; space := (or (cons null null) (cons lqtree-storable lqtree-storable))

; result := (values)

;;;; Affected By:
; none

;;;; Side-Effects:
; Modify object in the SPACE.

;;;; Notes:
; STORABLE is not modified.

;;;; Exceptional-Situations:

(requirements-about DO-STORED :doc-type function)

;;;; Description:

#+syntax (DO-STORED (var space) &body body) ; => result

;;;; Arguments and Values:

; var := SYMBOL, not evaluated.
#?(do-stored ("not-symbol" (quaspar::make-space)) :dummy)
:signals condition
#?(do-stored ((intern "NOT evaluated") (quaspar::make-space)) :dummy)
:signals condition
; Bound by lqtree-storable.
#?(let ((space (quaspar::make-space)))
    (store (make-instance 'lqtree-storable :max-w 100 :max-h 100) space)
    (store (make-instance 'lqtree-storable :x 1 :max-w 100 :max-h 100) space)
    (store (make-instance 'lqtree-storable :x 2 :max-w 100 :max-h 100) space)
    (do-stored (v space)
      (unless (typep v 'lqtree-storable)
        (error "Not lqtree-storable object."))))
=> NIL

; Iterate over lqtree-storable in the space.
#?(let ((space (quaspar::make-space)))
    (store (make-instance 'lqtree-storable :max-w 100 :max-h 100) space)
    (store (make-instance 'lqtree-storable :x 1 :max-w 100 :max-h 100) space)
    (store (make-instance 'lqtree-storable :x 2 :max-w 100 :max-h 100) space)
    (do-stored (v space)
      (prin1 (x (rect v)))))
:outputs "012"

; space := Form which generate quaspar::space object.
#?(do-stored (v :not-generate-space) (print v))
:signals condition

; body := implicit-progn

; result := null

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about DO-UNIQUE-PAIR :doc-type function)

;;;; Description:
; Iterate over unique pairs in the SPACE.

#+syntax (DO-UNIQUE-PAIR ((a b) space) &body body) ; => result

#?(let ((space (quaspar::make-space)))
    (store (make-instance 'lqtree-storable :max-w 100 :max-h 100) space)
    (store (make-instance 'lqtree-storable :x 1 :max-w 100 :max-h 100) space)
    (store (make-instance 'lqtree-storable :x 2 :max-w 100 :max-h 100) space)
    (do-unique-pair ((a b) space)
      (format t "~A:~A~%" (x (rect a)) (x (rect b)))))
:outputs "0:1
0:2
1:2
"

;;;; Arguments and Values:

; a := symbol, not evaluated.

; b := symbol, not evaluated.

; space := Form which generates SPACE object.

; body := implicit progn.

; result := NULL

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about LQTREE :doc-type TYPE)

;;;; Description:

;;;; Effective Slots:

; W [Type] (INTEGER 0 *)
; [READER] w
; Max width of the root space.

; H [Type] (INTEGER 0 *)
; [READER] h
; Max height of the root space.

; VECTOR [Type] VECTOR
; [READER] lqtree-vector

; DEPTH [Type] DEPTH
; [READER] lqtree-depth

;;;; Notes:

(requirements-about MAKE-LQTREE :doc-type function)

;;;; Description:

#+syntax (MAKE-LQTREE w h d) ; => result

#?(make-lqtree 10 10 3) :be-the lqtree
;;;; Arguments and Values:

; w := unsigned-byte
; Max-width of the tree's root space.

; h := unsigned-byte
; Max-height of the tree's root space.

; d := (unsigned-byte 4)

; result := lqtree

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

