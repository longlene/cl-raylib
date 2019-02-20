(in-package #:cl-raylib)

(defmethod translate-name-from-foreign ((spec string) (package (eql *package*)) &optional varp)
 (let ((name (translate-camelcase-name spec :upper-initial-p t :special-words '("FPS" "POT" "RES" "TTF" "BRDF"))))
  (if varp (intern (format nil "*~a" name)) name)))

(defmethod translate-name-to-foreign ((spec symbol) (package (eql *package*)) &optional varp)
 (let ((name (translate-camelcase-name spec :upper-initial-p t :special-words '("FPS" "POT" "RES" "TTF" "BRDF"))))
  (if varp (subseq name 1 (1- (length name))) name)))

