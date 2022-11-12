(in-package #:compiler)


(defstruct tables
  (class-table (make-hash-table :test 'equal))
  (subroutine-table (make-hash-table :test 'equal))
  (index-of-static 0)
  (index-of-field 0)
  (index-of-arg 0)
  (index-of-var 0))

(defun start-subroutine (table)
  "Starts a new subroutine scope."
  (setf (tables-subroutine-table table) (make-hash-table :test 'equal))
  (setf (tables-index-of-arg table) 0)
  (setf (tables-index-of-var table) 0))

(defun index-find-and-inc (table kind)
  (let ((symbol-kind (read-from-string kind)))
    (cond ((eql 'static symbol-kind)
	   (incf (tables-index-of-static table))
	   (1- (tables-index-of-static table)))
	  ((eql 'field symbol-kind)
	   (incf (tables-index-of-field table))
	   (1- (tables-index-of-field table)))
	  ((eql 'arg symbol-kind)
	   (incf (tables-index-of-arg table))
	   (1- (tables-index-of-arg table)))
	  ((eql 'var symbol-kind)
	   (incf (tables-index-of-var table))
	   (1- (tables-index-of-var table)))
	  (t (error "Kind not recognized")))))

(defun define (table name type kind)
  "Define a new identifier of a given name, type and kind and assigns it a running index."
  (cond ((member (read-from-string kind) '(static field))
	 (let ((h (tables-class-table table)))
	   (setf (gethash name h) (list type kind (index-find-and-inc table kind)))))
	((member (read-from-string kind) '(var arg))
	 (let ((h (tables-subroutine-table table)))
	   (setf (gethash name h) (list type kind (index-find-and-inc table kind)))))
	(t (error "Define error: kind unrecognized."))))

(defun var-count (table kind)
  "Returns the number of variables of the given kind already defined in table."
  (case (read-from-string kind)
    (static (tables-index-of-static table))
    (field (tables-index-of-field table))
    (arg (tables-index-of-arg table))
    (var (tables-index-of-var table))
    (otherwise (error "Kind not recognized when finding the index count."))))

(defun kind-of (table name)
  "Returns the kind of the named identifier in the current scope. If the identifier is unknown in the current scope, returns nil."
  (let ((c-table (tables-class-table table))
	(s-table (tables-subroutine-table table)))
    (cond ((gethash name s-table)
	   (second (gethash name s-table)))
	  ((gethash name c-table)
	   (second (gethash name c-table)))
	  (t nil))))

(defun type-of* (table name)
  "Returns the type of the named identifier in the current scope."
  (let ((c-table (tables-class-table table))
	(s-table (tables-subroutine-table table)))
    (cond ((gethash name s-table)
	   (first (gethash name s-table)))
	  ((gethash name c-table)
	   (first (gethash name c-table)))
	  (t nil))))

(defun index-of (table name)
  "Returns the index assigned to the named identifier."
  (let ((c-table (tables-class-table table))
	(s-table (tables-subroutine-table table)))
    (cond ((gethash name s-table)
	   (third (gethash name s-table)))
	  ((gethash name c-table)
	   (third (gethash name c-table)))
	  (t nil))))
