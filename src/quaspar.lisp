(in-package :cl-user)

(defpackage :quaspar
  (:shadow delete space)
  (:use :cl)
  (:export))

(in-package :quaspar)

;;;; CONDITION

(define-condition out-of-space (cell-error)
  ((point :initarg :point :reader point)
   (range :initarg :range :reader range)
   (max :initarg :max :reader max-of))
  (:report
   (lambda (condition stream)
     (format stream
             "Out of range ~A: Point ~S + Range ~S = ~S must smaller than ~S."
             (cell-error-name condition) (point condition) (range condition)
             (+ (point condition) (range condition)) (max-of condition)))))

;;;; MORTON-ORDER-SPACES

(defparameter *depth* 4)

;; Currently supported *DEPTH* is declaimed below.
;; This is depends on BIT-SEPARATE algorithm.

(declaim
 (type
  (integer 0 15
   #+(or) ; How to compute above max value is.
   (loop :for i :upfrom 0
         :if (< #xFFFFFFFF ; <--- (unsigned-byte 32)
                (linear-quad-length i))
           :return (1- i)))
  *depth*))

(declaim
 (ftype (function
         ((integer 0 *) (integer 0 *) (integer 0 *) (integer 0 *) &optional
          (integer 0 *))
         (values (integer 0 *) (integer 0 *) &optional))
        morton-cord))

(defun morton-cord (x y max-w max-h &optional (depth *depth*))
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

(defun linear-index (x y w h max-w max-h &optional (*depth* *depth*))
  "Compute index of background linear quad tree's vector."
  (assert (< 0 (+ x w) max-w) ()
    'out-of-space :name 'x
                  :point x
                  :range w
                  :max max-w)
  (assert (< 0 (+ y h) max-h) ()
    'out-of-space :name 'y
                  :point y
                  :range h
                  :max max-h)
  (let* ((left-top
          (multiple-value-call #'smallest-space-index
            (morton-cord x y max-w max-h *depth*)))
         (right-bottom
          (multiple-value-call #'smallest-space-index
            (morton-cord (+ x w) (+ y h) max-w max-h *depth*)))
         (depth (depth left-top right-bottom)))
    (+ (/ (1- (expt 4 depth)) *depth*) (space-local-index left-top depth))))

;;;; RECT
;; In order to update cordinates at once, we needs RECT object.

(defclass rect ()
  ((x :initarg :x :initform 0 :accessor x)
   (y :initarg :y :initform 0 :accessor y)
   (w :initarg :w :initform 0 :reader w)
   (h :initarg :h :initform 0 :reader h))
  (:documentation "The default rect object for LQTREE-STORABLE."))

(defmethod print-object ((o rect) stream)
  (print-unreadable-object (o stream :type t)
    (format stream "x ~S y ~S w ~S h ~S" (x o) (y o) (w o) (h o))))

(defun make-rect (&key (x 0) (y 0) (w 0) (h 0))
  (make-instance 'rect :x x :y y :w w :h h))

(defparameter *rect-constructor* 'make-rect)

;;;; SPACE

(deftype space () '(or (cons null null) (cons lqtree-storable lqtree-storable)))

(defun make-space () (list nil))

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
  (:default-initargs :x 0 :y 0 :w 0 :h 0 :depth *depth*)
  (:documentation "Inherit this to store object for lqtree."))

(defmethod initialize-instance
           ((o lqtree-storable)
            &rest args
            &key x y w h (max-w (error "MAX-W is required."))
            (max-h (error "MAX-H is reqrured.")) depth index rect)
  (multiple-value-call #'call-next-method
    (values :index (or index (linear-index x y w h max-w max-h depth)))
    (values :rect (or rect (funcall *rect-constructor* :x x :y y :w w :h h)))
    (values-list args)))

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
  (values))

(defun store (storable space)
  (if (empty-space-p space)
      (setf (space-contents space) storable
            (space-last space) storable)
      (setf (prev storable) (space-last space)
            (next (space-last space)) storable
            (space-last space) storable)))

(defmacro do-stored ((var space) &body body)
  `(do ((,var (space-contents ,space) (next ,var)))
       (nil)
    ,@body
     (when (null (next ,var))
       (return))))

;;;; LQTREE

(defun linear-quad-length (depth)
  (loop :for i :upto depth
        :sum (expt 4 i)))

(defstruct (lqtree (:constructor make-lqtree
                    (depth &aux
                           (vector
                             (let* ((length (linear-quad-length depth))
                                    (vector (make-array length)))
                               (dotimes (i length vector)
                                 (setf (aref vector i) (make-space))))))))
  (vector #() :type vector :read-only t)
  (depth *depth* :type (integer 0 15) :read-only t))

(defmethod print-object ((o lqtree) stream)
  (print-unreadable-object (o stream :type t)))

(defun space (lqtree x y w h max-w max-h &optional (*depth* *depth*))
  (aref (lqtree-vector lqtree) (linear-index x y w h max-w max-h *depth*)))

(defun (setf space)
       (new lqtree x y w h max-w max-h &optional (*depth* *depth*))
  (store new (space lqtree x y w h max-w max-h *depth*)))

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

(defun move (storable x y max-w max-h lqtree)
  (setf (x (rect storable)) x
        (y (rect storable)) y)
  (let ((new-space
         (linear-index x y (w storable) (h storable) max-w max-h *depth*)))
    (if (= new-space (index storable))
        storable
        (progn
         (delete storable lqtree)
         (store storable (aref (lqtree-vector lqtree) new-space))))))