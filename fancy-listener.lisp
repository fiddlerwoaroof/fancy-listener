(defpackage :fancy-listener
  (:use :cl )
  (:export :row :column :button :stream-disp :fancy-listener *interface-layout*))

(defpackage :fl-user
  (:use :cl :fancy-listener))

(in-package :fl-user)
(declaim (special fancy-listener:*interface-layout*))

(defmacro with-pp ((pane) &body body)
  `(capi:apply-in-pane-process ,pane
                               (lambda ()
                                 ,@body)))

(defun push-widget (widget)
  (when *interface-layout*
    (with-pp (*interface-layout*)
      (when (capi:layout-description *interface-layout*)
        (push :divider (capi:layout-description *interface-layout*)))
      (push widget (capi:layout-description *interface-layout*)))))

(defun pop-widget ()
  (when *interface-layout*
    (with-pp (*interface-layout*)
      (pop (capi:layout-description *interface-layout*))
      (when (eql (first (capi:layout-description *interface-layout*))
                 :divider)
        (pop (capi:layout-description *interface-layout*))))))

(in-package :fancy-listener)
(defvar *interface-layout* nil)

(defun row (&rest components)
  (make-instance 'capi:row-layout :children components))

(defun column (&rest components)
  (make-instance 'capi:column-layout :children components))

(defun button (cb &optional (text "Go!"))
  (make-instance 'capi:push-button
                 :text text
                 :callback cb))

(defun stream-disp ()
  (let ((out-pane (make-instance 'capi:collector-pane)))
    (values out-pane
            (capi:collector-pane-stream out-pane))))
 
(defun listener-pane ()
  (make-instance 'capi:listener-pane))


(defun interface ()
  (setf *interface-layout* (column))
  (let* ((listener-pane (listener-pane))
         (result (make-instance 'capi:interface
                                :layout (row listener-pane
                                             :divider
                                             *interface-layout*))))
    (capi:interactive-pane-execute-command listener-pane "(in-package :fl-user)")
    result))

(defun fancy-listener ()
  (capi:display (interface)))
