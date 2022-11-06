(in-package #:compiler)


(defclass symbol-table ()
  ((class-table
    :initform (make-hash-table :test 'equal)
    :accessor class-table)
   (subroutine-table
    :initform (make-hash-table :test 'equal)
    :accessor subroutine-table)
   (index-of-static
    :initform 0
    :accessor index-of-static)
   (index-of-field
    :initform 0
    :accessor index-of-field)
   (index-of-arg
    :initform 0
    :accessor index-of-arg)
   (index-of-var
    :initform 0
    :accessor index-of-var)))

(defun make-symbol-table ()
  (make-instance 'symbol-table))

(defgeneric start-subroutine (table)
  (:documentation "Starts a new subroutine scope.")
  (:method (table)
    (setf (subroutine-table table) (make-hash-table :test 'equal))
    (setf (index-of-arg table) 0)
    (setf (index-of-var table) 0)))

(defgeneric define (table name type kind)
  (:documentation "Define a new identifier of a given name, type and kind and assigns it a running index."))

(defgeneric var-count (table kind)
  (:documentation "Returns the number of variables of the given kind already defined in table."))

(defgeneric kind-of (table name)
  (:documentation "Returns the kind of the named identifier in the current scope. If the identifier is unknown in the current scope, returns nil."))

(defgeneric type-of* (table name)
  (:documentation "Returns the type of the named identifier in the current scope."))

(defgeneric index-of (table name)
  (:documentation "Returns the index assigned to the named identifier."))

(defgeneric index-find-and-inc (table kind)
  (:method (table kind)
    (let ((symbol-kind (read-from-string kind)))
     (cond ((eql 'static symbol-kind)
	    (incf (index-of-static table))
	    (1- (index-of-static table)))
	   ((eql 'field symbol-kind)
	    (incf (index-of-field table))
	    (1- (index-of-field table)))
	   ((eql 'arg symbol-kind)
	    (incf (index-of-arg table))
	    (1- (index-of-arg table)))
	   ((eql 'var symbol-kind)
	    (incf (index-of-var table))
	    (1- (index-of-var table)))
	   (t (error "Kind not recognized"))))))

(defmethod define (table name type kind)
  (cond ((member (read-from-string kind) '(static field))
	 (let ((h (class-table table)))
	   (setf (gethash name h) (list type kind (index-find-and-inc table kind)))))
	((member (read-from-string kind) '(var arg))
	 (let ((h (subroutine-table table)))
	   (setf (gethash name h) (list type kind (index-find-and-inc table kind)))))
	(t (error "Define error: Kind unrecognized."))))

(defmethod var-count (table kind)
  (case (read-from-string kind)
    (static (index-of-static table))
    (field (index-of-field table))
    (arg (index-of-arg table))
    (var (index-of-var table))
    (otherwise (error "Kind not recognized when finding the index count."))))

(defmethod kind-of (table name)
  (let ((c-table (class-table table))
	(s-table (subroutine-table table)))
    (cond ((gethash name s-table)
	   (second (gethash name s-table)))
	  ((gethash name c-table)
	   (second (gethash name c-table)))
	  (t nil))))

(defmethod type-of* (table name)
  (let ((c-table (class-table table))
	(s-table (subroutine-table table)))
    (cond ((gethash name s-table)
	   (first (gethash name s-table)))
	  ((gethash name c-table)
	   (first (gethash name c-table)))
	  (t nil))))

(defmethod index-of (table name)
  (let ((c-table (class-table table))
	(s-table (subroutine-table table)))
    (cond ((gethash name s-table)
	   (third (gethash name s-table)))
	  ((gethash name c-table)
	   (third (gethash name c-table)))
	  (t nil))))
