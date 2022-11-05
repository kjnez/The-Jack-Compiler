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
    (cond ((eql 'static kind)
	   (incf (index-of-static table))
	   (index-of-static table))
	  ((eql 'field kind)
	   (incf (index-of-field table))
	   (index-of-field table))
	  ((eql 'arg kind)
	   (incf (index-of-arg table))
	   (index-of-arg table))
	  ((eql 'var kind)
	   (incf (index-of-var table))
	   (index-of-var table))
	  (t (error "Kind not recognized")))))

(defmethod define (table name type kind)
  (cond ((member kind '(static field))
	 (let ((h (class-table table)))
	   (setf (gethash name h) (list type kind (index-find-and-inc table kind)))))
	((member kind '(var arg))
	 (let ((h (subroutine-table table)))
	   (setf (gethash name h) (list type kind (index-find-and-inc table kind)))))
	(t (error "Define error: Kind unrecognized."))))

(defmethod var-count (table kind)
  (case kind
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
