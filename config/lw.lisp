(in-package #:ctype)

(declaim (inline ratiop))
(defun ratiop (object) (lispworks:ratiop object))

(define-constant +floats+
    '((single-float . hcl:single-float-p)
      (double-float . hcl:double-float-p))
  :test #'equal)

(define-constant +standard-charset+ '((10 . 10) (32 . 126)) :test #'equal)
(define-constant +base-charset+ '((0 . 127)) :test #'equal)

(define-constant +string-uaets+ '(nil base-char character) :test #'equal)

(define-constant +complex-arrays-exist-p+ t)

(declaim (inline simple-array-p))
(defun simple-array-p (object) (hcl:simple-array-p object))

(define-constant +class-aliases+ () :test #'equal)

(declaim (inline subclassp))
(defun subclassp (sub super) (member super (hcl:class-precedence-list sub)))

(declaim (inline typexpand))
(defun typexpand (type-specifier environment)
  (declare (ignore environment))
  ;; The two-argument version is for compatibility; environment is ignored.
  (type:expand-user-type type-specifier #+(or) environment))

(defmacro complex-ucptp (objectf ucpt)
  `(ecase ,ucpt
     ((*) t)
     #+(or)
     ((single-float) (sb-kernel:complex-single-float-p ,objectf))
     #+(or)
     ((double-float) (sb-kernel:complex-double-float-p ,objectf))
     #+(or)
     ((rational) (sb-kernel:complex-rational-p ,objectf))))
