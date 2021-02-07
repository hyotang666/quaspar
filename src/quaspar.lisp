(in-package :cl-user)

(defpackage :quaspar
  (:shadow delete)
  (:use :cl)
  (:export))

(in-package :quaspar)

;;;; MORTON-ORDER-SPACES

(defparameter *depth* 4)

(declaim (type (integer 1 *) *depth*))

(declaim
 (ftype (function
         ((integer 0 *) (integer 0 *) (integer 0 *) (integer 0 *) &optional
          (integer 0 *))
         (values (integer 0 *) (integer 0 *) &optional))
        morton-cord))

(defun morton-cord (x y w h &optional (depth *depth*))
  "Convert cordinates to morton spaces cordinates."
  (let ((expt (expt 2 depth)))
    (values (floor x (/ (float w) expt)) (floor y (/ (float h) expt)))))

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
  "Convert morton cordinates to linear local morton space index."
  (logior (bit-separate x) (ash (bit-separate y) 1)))

(defun depth (left-top right-bottom)
  "Compute ocupied space's depth. 0 is the smallest."
  (loop :for n = (logxor left-top right-bottom) :then (ash n -2)
        :for i :upfrom 0
        :if (zerop n)
          :return i))

(defun space-local-index (left-top ocupied-space-depth)
  (ash left-top (- (* 2 ocupied-space-depth))))

(defun index-as-root (n)
  (do* ((n n (ash n -2))
        (i 0 (1+ i))
        (byte (ldb (byte 2 0) n) (ldb (byte 2 0) n))
        result)
       ((zerop n) (values result i))
    (unless (zerop byte)
      (setf result byte))))

(defun morton-space-index (x y w h max-w max-h &optional (*depth* *depth*))
  (multiple-value-bind (index depth)
      (index-as-root
        (logand
          (multiple-value-call #'linear-index (morton-cord x y max-w max-h))
          (multiple-value-call #'linear-index
            (morton-cord (+ x w) (+ y h) max-w max-h))))
    (+ index (/ (expt 4 depth) *depth*))))

;;;; DOUBLE-LINKED-LIST

(defstruct dcons prev next content)

(defmethod print-object ((o dcons) stream)
  (print-unreadable-object (o stream :type t :identity t)
    (pprint-logical-block (stream nil)
      (do ((cons o (dcons-next cons)))
          (nil)
        (write (dcons-content cons) :stream stream)
        (when (null (dcons-next cons))
          (return))
        (write-char #\Space stream)
        (pprint-newline :fill stream)))))

(defun dlist (content &rest rest-contents)
  (do* ((first (make-dcons :content content))
        (rest rest-contents (cdr rest))
        (cdr first))
       ((null rest) first)
    (setf cdr
            (let ((dcons (make-dcons :prev cdr :content (car rest))))
              (setf (dcons-next cdr) dcons)
              dcons))))

(defun delete (dcons)
  (setf (dcons-next (dcons-prev dcons)) (dcons-next dcons))
  t)

(defmacro do-dlist ((var dlist) &body body)
  (let ((v (gensym "DLIST")))
    `(do* ((,v ,dlist (dcons-next ,v))
           (,var (dcons-content ,v) (dcons-content ,v)))
          (nil)
      ,@body
       (when (null (dcons-next ,v))
         (return)))))

;;;; LQTREE

(defstruct cell last first)

(defun empty-cell-p (cell) (null (cell-first cell)))

(defun insert (object cell)
  (let ((cons (make-dcons :content object :prev (cell-last cell))))
    (setf (cell-last cell) (setf (dcons-next (cell-last cell)) cons))
    cell))

(defstruct (lqtree (:constructor make-lqtree
                    (depth &aux
                     (vector
                       (make-array
                         (loop :for i :upto depth
                               :sum (expt 4 i))
                         :initial-element (make-cell))))))
  (vector #() :type vector :read-only t)
  (depth (integer 1 *) :type (integer 1 *) :read-only t))

(defmethod print-object ((o lqtree) stream)
  (print-unreadable-object (o stream :type t)))

(defun traverse (lqtree call-back)
  (labels ((rec (index &optional seen)
             (when (array-in-bounds-p lqtree index)
               (if (empty-cell-p (aref lqtree index))
                   (rec (1+ index))
                   (let ((seen (cons (cell-first (aref lqtree index)) seen)))
                     (funcall call-back seen)
                     (rec (1+ index) seen))))))
    (rec 0)))

(defmacro do-lqtree ((var lqtree) &body body)
  (let ((cell (gensym "CELL")))
    `(loop :for ,cell :across (lqtree-vector ,lqtree)
           :for ,var = (cell-content ,cell)
           :do (progn ,@body))))

(defun ref (lqtree x y w h max-w max-h)
  (aref (lqtree-vector lqtree) (morton-space-index x y w h max-w max-h)))

(defun (setf ref) (new lqtree x y w h max-w max-h)
  (setf (aref (lqtree-vector lqtree) (morton-space-index x y w h max-w max-h))
          new))
