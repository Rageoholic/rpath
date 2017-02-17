(defpackage :rpath
  (:use :cl)
  (:export directory-pathname-p
	   pathname-as-directory
	   directory-wildcard
	   list-directory
	   file-exists-p
	   pathname-as-file
	   walk-directory))

(in-package :rpath)

(defun component-present-p (value)
  (and value (not (eql value :unspecific))))

(defun directory-pathname-p (p)
  (and (not (component-present-p (pathname-name p)))
       (not (component-present-p (pathname-type p)))
       p))

(defun pathname-as-directory (name)
  (let ((pathname (pathname name)))
    (when (wild-pathname-p pathname)
      (error "Can't convert wild pathnames reliably"))
    (if (not (directory-pathname-p name))
	(make-pathname
	 :directory (append (or (pathname-directory pathname) (list :relative))
			    (list (file-namestring pathname)))
	 :name nil
	 :type nil
	 :defaults pathname)
	pathname)))

(defun directory-wildcard (dirname)
  (make-pathname
   :name :wild
   :type :wild
   :defaults (pathname-as-directory dirname)))

(defun list-directory (dirname)
	 (when (wild-pathname-p dirname)
	   (error "Can't use wild pathnames"))
	 (directory (directory-wildcard dirname)))

(defun file-exists-p (path)
  (probe-file path))

(defun pathname-as-file (name)
  (let ((pathname (pathname name)))
    (when (wild-pathname-p pathname)
      (error "rpath does not accept wild pathnames"))
    (if (directory-pathname-p name)
	(let* ((directory (pathname-directory pathname))
	       (name-and-type (pathname (first (last directory)))))
	  (make-pathname
	   :directory (butlast directory)
	   :name (pathname-name name-and-type)
	   :type (pathname-type name-and-type)
	   :defaults pathname))
	pathname)))

(defun walk-directory (dirname fn &key directories (test (constantly t)))
  (labels
      ((walk (name)
	 (cond
	   ((directory-pathname-p name)
	    (when (and directories (funcall test name))
	      (funcall fn name))
	    (dolist (x (list-directory name)) (walk x)))
	   ((funcall test name (funcall fn name))))))
    (walk (pathname-as-directory dirname))))
