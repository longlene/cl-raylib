(in-package #:cl-raylib-util)

(defmethod translate-name-from-foreign ((spec string) (package (eql (find-package 'cl-raylib))) &optional varp)
 (let ((name (translate-camelcase-name spec :upper-initial-p t :special-words '("2D" "3D" "FPS" "HSV" "POT" "RES" "TTF" "BRDF" "URL" "UTF8"))))
  (if varp (intern (format nil "*~a" name)) name)))

(defmethod translate-name-to-foreign ((spec symbol) (package (eql (find-package 'cl-raylib))) &optional varp)
 (let ((name (translate-camelcase-name spec :upper-initial-p t :special-words '("2D" "3D" "FPS" "HSV" "POT" "RES" "TTF" "BRDF" "URL" "UTF8"))))
  (if varp (subseq name 1 (1- (length name))) name)))

(defmacro define-conversion-into-foreign-memory (lambda-list &body body)
  (let ((unquoted (mapcar (lambda (x)
                            (etypecase x
                              (symbol x)
                              (list (car x))))
                          (list (first lambda-list) (third lambda-list)))))
    (labels ((walk-and-quote (form)
               "A simple code walker that works fine without symbol shadowing"
               (typecase form
                 (list (cond
                         ((eql (first form) 'quote) `(quote ,form))
                         ((eql form body) `(list 'progn . ,(mapcar #'walk-and-quote form)))
                         (t `(list . ,(mapcar #'walk-and-quote form)))))
                 (t (if (member form unquoted) form `(quote ,form))))))
      `(progn
         (eval-when (:compile-toplevel :load-toplevel :execute)
           (defmethod expand-into-foreign-memory ,lambda-list
             ,(walk-and-quote body)))
         (defmethod translate-into-foreign-memory ,lambda-list
           ,@body)))))

(defmacro define-conversion-from-foreign (lambda-list &body body)
  (let ((unquoted (let ((arg1 (first lambda-list)))
                    (etypecase arg1
                      (symbol arg1)
                      (list arg1)))))
    (labels ((walk-and-quote (form)
               "A simple code walker that works fine without symbol shadowing"
               (typecase form
                 (list (cond
                         ((eql (first form) 'quote) `(quote ,form))
                         ((eql form body) `(list 'progn . ,(mapcar #'walk-and-quote form)))
                         (t `(list . ,(mapcar #'walk-and-quote form)))))
                 (t (if (eql form unquoted) form `(quote ,form))))))
      `(progn
         (eval-when (:compile-toplevel :load-toplevel :execute)
           (defmethod expand-from-foreign ,lambda-list
             ,(walk-and-quote body)))
         (defmethod translate-from-foreign ,lambda-list
           ,@body)))))
