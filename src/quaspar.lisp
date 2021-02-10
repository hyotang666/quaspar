(in-package :cl-user)

(defpackage :quaspar
  (:shadow delete space)
  (:use :cl)
  (:export ;;; Condition
           #:out-of-space
           ;;; LQTREE
           #:lqtree ; type-name
           #:make-lqtree ; constructor
           #:space ; node reader
           #:add
           #:delete
           #:move
           #:do-lqtree ; iterate macro.
           #:traverse ; interate function.
           ;;; RECT
           #:rect ; class
           #:make-rect ; constructor
           ;; Slot-accessors
           #:x
           #:y
           #:w
           #:h
           ;;; LQTREE-STORABLE
           #:lqtree-storable ; class
           #| #:rect ; reader |#
           ;;; Underlying functions for heavy user.
           #:prev ; accessor
           #:next ; accessor
           #:index ; accessor
           #:linear-index
           #:delete-from-space
           #:store
           #:do-stored ; iterate macro.
           #:do-unique-pair ; iterate macro.
           #:lqtree-vector ; reader
           #:lqtree-depth ; reader
           ))

(in-package :quaspar)

;;;; RECT
;; In order to update cordinates at once, we needs RECT object.

(defclass rect ()
  ((x :initarg :x :initform 0 :accessor x :type (integer 0 *))
   (y :initarg :y :initform 0 :accessor y :type (integer 0 *))
   (w :initarg :w :initform 0 :reader w :type (integer 0 *))
   (h :initarg :h :initform 0 :reader h :type (integer 0 *)))
  (:documentation "The default rect object for LQTREE-STORABLE."))

(defmethod print-object ((o rect) stream)
  (print-unreadable-object (o stream :type t)
    (format stream "x ~S y ~S w ~S h ~S" (x o) (y o) (w o) (h o))))

(declaim
 (ftype (function
         (&key (:x (integer 0 *)) (:y (integer 0 *)) (:w (integer 0 *))
          (:h (integer 0 *)))
         (values rect &optional))
        make-rect))

(defun make-rect (&key (x 0) (y 0) (w 0) (h 0))
  (make-instance 'rect :x x :y y :w w :h h))

;;;; CONDITION

(define-condition out-of-space (cell-error)
  ((rect :initarg :rect :reader rect)
   (max :initarg :max :reader max-of))
  (:report
   (lambda (condition stream)
     (format stream "~S is out of ~S in range ~S." (rect condition)
             (max-of condition) (cell-error-name condition)))))

;;;; MORON-NUMBER-INDEX

(declaim
 (ftype (function ((integer -1 *)) (values (integer 0 *) &optional))
        linear-quad-length))

(defun linear-quad-length (depth) (/ (1- (expt 4 (1+ depth))) 3))

(deftype depth ()
  ;; Currently supported depth is below.
  ;; This is depends on BIT-SEPARATE algorithm.
  #+(or) ; How to compute above max value is.
  (loop :for i :upfrom 0
        :if (< #xFFFFFFFF ; <--- (unsigned-byte 32)
               (linear-quad-length i))
          :return (1- i))
  '(integer 0 15))

(declaim
 (ftype (function
         ((integer 0 *) (integer 0 *) (integer 0 *) (integer 0 *) depth)
         (values (integer 0 *) (integer 0 *) &optional))
        morton-cord))

(defun morton-cord (x y max-w max-h depth)
  "Convert cordinates to morton spaces cordinates."
  (let ((expt (expt 2 depth)))
    (values (floor x (/ (float max-w) expt)) (floor y (/ (float max-h) expt)))))

(declaim
 (ftype (function ((unsigned-byte 16)) (values (unsigned-byte 32) &optional))
        bit-separate))

(defun bit-separate (integer)
  (let* ((n (logand (logior (ash integer 8) integer) #xFF00FF))
         (n (logand (logior (ash n 4) n) #xF0F0F0F))
         (n (logand (logior (ash n 2) n) #x33333333))
         (n (logand (logior (ash n 1) n) #x55555555)))
    n))

(declaim
 (ftype (function ((unsigned-byte 16) (unsigned-byte 16))
         (values (unsigned-byte 32) &optional))
        smallest-space-index))

(defun smallest-space-index (x y)
  "Convert morton cordinates to linear max depth morton space index."
  (logior (bit-separate x) (ash (bit-separate y) 1)))

(defun depth (left-top right-bottom)
  "Compute ocupied space's depth. 0 is the smallest."
  (loop :for n = (logxor left-top right-bottom) :then (ash n -2)
        :for i :upfrom 0
        :if (zerop n)
          :return i))

(defun space-local-index (left-top ocupied-space-depth)
  "Compute linear index of an ocupied local space."
  (ash left-top (- (* 2 ocupied-space-depth))))

(defun linear-index (rect max-w max-h depth)
  "Compute index of background linear quad tree's vector."
  (let ((vert (+ (x rect) (w rect))) (hor (+ (y rect) (h rect))))
    (assert (< -1 vert max-w) () 'out-of-space :name 'x :rect rect :max max-w)
    (assert (< -1 hor max-h) () 'out-of-space :name 'y :rect rect :max max-h)
    (let* ((left-top
            (multiple-value-call #'smallest-space-index
              (morton-cord (x rect) (y rect) max-w max-h depth)))
           (right-bottom
            (multiple-value-call #'smallest-space-index
              (morton-cord vert hor max-w max-h depth)))
           (space-depth (depth left-top right-bottom)))
      (+ (linear-quad-length (- depth space-depth 1))
         (space-local-index left-top space-depth)))))

;;;; SPACE

(deftype space () '(or (cons null null) (cons lqtree-storable lqtree-storable)))

(defun make-space (&optional first-content) (cons first-content first-content))

(defun empty-space-p (space) (null (car space)))

(defun space-contents (space) (car space))

(defun (setf space-contents) (new space) (setf (car space) new))

(defun space-last (space) (cdr space))

(defun (setf space-last) (new space) (setf (cdr space) new))

;;;; LQTREE-STORABLE

(defclass lqtree-storable ()
  ((index :initarg :index :accessor index :documentation "Morton space index")
   (rect :initarg :rect :reader rect)
   (prev :initform nil :initarg :prev :accessor prev
         ;; NIL means top of the list.
         :type (or null lqtree-storable)
         :documentation "Previous object of the list.")
   (next :initform nil :initarg :next :accessor next
         ;; NIL means last of the list.
         :type (or null lqtree-storable)
         :documentation "Next object of the list."))
  (:default-initargs :x 0 :y 0 :w 0 :h 0 :depth 4 :rect-constructor 'make-rect)
  (:documentation "Inherit this to store object for lqtree."))

(defmethod initialize-instance
           ((o lqtree-storable)
            &rest args
            &key x y w h (max-w (error "MAX-W is required."))
            (max-h (error "MAX-H is reqrured.")) depth rect-constructor)
  (let ((rect (funcall rect-constructor :x x :y y :w w :h h)))
    (apply #'call-next-method o :index (linear-index rect max-w max-h depth)
           :rect rect args)))

(declaim
 (ftype (function (lqtree-storable space) (values &optional))
        delete-from-space))

(defun delete-from-space (storable space)
  (if (prev storable)
      (if (next storable)
          ;; Between two storables.
          (setf (next (prev storable)) (next storable)
                (prev (next storable)) (prev storable))
          ;; Last in space
          (setf (next (prev storable)) nil
                (space-last space) (prev storable)))
      (if (next storable)
          ;; First in space
          (setf (prev (next storable)) nil
                (space-contents space) (next storable))
          ;; Only one storable in space.
          (setf (space-contents space) nil
                (space-last space) nil)))
  ;; Cleanup.
  (setf (prev storable) nil
        (next storable) nil)
  (slot-makunbound storable 'index)
  (values))

(declaim
 (ftype (function (lqtree-storable space) (values lqtree-storable &optional))
        store))

(defun store (storable space)
  (if (empty-space-p space)
      (setf (space-contents space) storable
            (space-last space) storable)
      (setf (prev storable) (space-last space)
            (next (space-last space)) storable
            (space-last space) storable)))

(defmacro do-stored ((var space) &body body)
  (let ((s (gensym "SPACE")))
    `(let ((,s (the space ,space)))
       (unless (empty-space-p ,s)
         (do ((,var (space-contents ,s) (next ,var)))
             (nil)
          ,@body
           (when (null (next ,var))
             (return)))))))

(defmacro do-unique-pair (((a b) space) &body body)
  `(do-stored (,a ,space)
     (when (next ,a)
       (do-stored (,b (make-space (next ,a)))
         ,@body))))

;;;; LQTREE

(defclass lqtree ()
  ((w :initarg :w
      :type (integer 0 *)
      :reader w
      :documentation "Max width of the root space.")
   (h :initarg :h
      :type (integer 0 *)
      :reader h
      :documentation "Max height of the root space.")
   (vector :reader lqtree-vector :type vector)
   (depth :initarg :depth :type depth :reader lqtree-depth))
  (:default-initargs :depth 4)
  (:documentation "Linear Quad Tree."))

(defmethod initialize-instance :after ((o lqtree) &key depth)
  (setf (slot-value o 'vector)
          (let* ((length (linear-quad-length depth))
                 (vector (make-array length)))
            (dotimes (i length vector) (setf (aref vector i) (make-space))))))

(declaim
 (ftype (function ((integer 0 *) (integer 0 *) depth)
         (values lqtree &optional))
        make-lqtree))

(defun make-lqtree (w h d) (make-instance 'lqtree :w w :h h :depth d))

(declaim (ftype (function (t lqtree) (values space &optional)) space))

(defun space (rect lqtree)
  "Return SPACE object that is RECT should be in from LQTREE."
  (aref (lqtree-vector lqtree)
        (linear-index rect (w lqtree) (h lqtree) (lqtree-depth lqtree))))

(declaim
 (ftype (function (lqtree-storable lqtree) (values lqtree-storable &optional))
        add))

(defun add (storable lqtree)
  (store storable
         (aref (lqtree-vector lqtree)
               (linear-index (rect storable) (w lqtree) (h lqtree)
                             (lqtree-depth lqtree)))))

(defun traverse (lqtree call-back)
  (labels ((rec (index &optional seen)
             (when (array-in-bounds-p (lqtree-vector lqtree) index)
               (if (empty-space-p (aref (lqtree-vector lqtree) index))
                   (rec (1+ index))
                   (let ((seen
                          (cons
                            (space-contents
                              (aref (lqtree-vector lqtree) index))
                            seen)))
                     (funcall call-back seen)
                     (rec (1+ index) seen))))))
    (rec 0)))

(defmacro do-lqtree ((var lqtree) &body body)
  "Iterate over every objects in lqtree."
  (let ((space (gensym "SPACE")))
    `(loop :for ,space :across (lqtree-vector ,lqtree)
           :do (do-stored (,var ,space)
                 ,@body))))

(defun delete (storable lqtree)
  (delete-from-space storable (aref (lqtree-vector lqtree) (index storable))))

(defun move (storable x y lqtree)
  (setf (x (rect storable)) x
        (y (rect storable)) y)
  (let ((new-space
         (linear-index (rect storable) (w lqtree) (h lqtree)
                       (lqtree-depth lqtree))))
    (if (= new-space (index storable))
        storable
        (progn
         (delete storable lqtree)
         (setf (index storable) new-space)
         (store storable (aref (lqtree-vector lqtree) new-space))))))
