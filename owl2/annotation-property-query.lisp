(in-package :cl-user)

(defun annotation-property-parents (annotation-property kb &aux them)
  (each-axiom kb
      (lambda(ax)
        (axiom-typecase ax
          (:subannotationpropertyof
           (let ((sub (make-uri (#"toString" (#"getIRI" (#"getSubProperty" ax)))))
                 (super  (make-uri (#"toString" (#"getIRI" (#"getSuperProperty" ax))))))
             (if (eq annotation-property sub) (push super them))))))))

(defun annotation-property-children (annotation-property kb &aux them)
  (each-axiom kb
      (lambda(ax)
        (axiom-typecase ax
          (:subannotationpropertyof
           (let ((sub (make-uri (#"toString" (#"getIRI" (#"getSubProperty" ax)))))
                 (super  (make-uri (#"toString" (#"getIRI" (#"getSuperProperty" ax))))))
             (if (eq annotation-property super) (push sub them)))))))
  them)

(defun annotation-property-ancestors (annotation-property kb &aux them)
  (each-axiom kb
      (lambda(ax)
        (axiom-typecase ax
          (:subannotationpropertyof
           (let ((sub (make-uri (#"toString" (#"getIRI" (#"getSubProperty" ax)))))
                 (super  (make-uri (#"toString" (#"getIRI" (#"getSuperProperty" ax))))))
             (when (eq annotation-property sub)
               (push super them)
               (setq them (append (annotation-property-ancestors super kb) them))
               ))))))
  (remove-duplicates them))

(defun annotation-property-descendants (annotation-property kb &aux them)
  (each-axiom kb
      (lambda(ax)
        (axiom-typecase ax
          (:subannotationpropertyof
           (let ((sub (make-uri (#"toString" (#"getIRI" (#"getSubProperty" ax)))))
                 (super  (make-uri (#"toString" (#"getIRI" (#"getSuperProperty" ax))))))
             (when (eq annotation-property super)
               (push sub them)
               (setq them (append (annotation-property-descendants sub kb) them))
               ))))))
  (remove-duplicates them))


