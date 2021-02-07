(in-package :cl-user)

(defpackage :quaspar
  (:shadow delete)
  (:use :cl)
  (:export))

(in-package :quaspar)

;;;; MORTON-ORDER-SPACES

(defparameter *depth* 4)

(declaim (type (integer 1 *) *depth*))

(defun morton-cord (x y w h &optional (depth *depth*))
  (let ((expt (expt 2 depth)))
    (values (floor x (/ (float w) expt)) (floor y (/ (float h) expt)))))

(defun bit-separate (integer)
  (let* ((n (logand (logior (ash integer 8) integer) #xFF00FF))
         (n (logand (logior (ash n 4) n) #xF0F0F0F))
         (n (logand (logior (ash n 2) n) #x33333333))
         (n (logand (logior (ash n 1) n) #x55555555)))
    n))

(defun morton-index (x y) (logior (bit-separate x) (ash (bit-separate y) 1)))

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
          (multiple-value-call #'morton-index (morton-cord x y max-w max-h))
          (multiple-value-call #'morton-index
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

;;;; LQTREE

(defstruct cell last first)

(defun empty-cell-p (cell) (null (cell-first cell)))

(defun insert (object cell)
  (let ((cons (make-dcons :content object :prev (cell-last cell))))
    (setf (cell-last cell) (setf (dcons-next (cell-last cell)) cons))
    cell))

(defun make-lqtree (depth)
  (make-array
    (loop :for i :upto depth
          :sum (expt 4 i))
    :initial-element (make-cell)))

(defun traverse (lqtree call-back)
  (labels ((rec (index &optional seen)
             (when (array-in-bounds-p lqtree index)
               (if (empty-cell-p (aref lqtree index))
                   (rec (1+ index))
                   (let ((seen (cons (cell-first (aref lqtree index)) seen)))
                     (funcall call-back seen)
                     (rec (1+ index) seen))))))
    (rec 0)))
