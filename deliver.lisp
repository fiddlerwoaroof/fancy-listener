(in-package :cl-user)
(setf *default-pathname-defaults*
 (make-pathname :directory (pathname-directory *load-pathname*)))
(format t "~&CURDIR: ~a~%" (truename "."))
(load-all-patches)
(compile-file "fancy-listener.lisp" :load t)

(deliver 'fancy-listener:fancy-listener
         #+:cocoa
         (create-macos-application-bundle
          "~/desktop/FancyListener.app"
          :document-types nil
          :identifier "fwoar.FancyListener")
         #-:cocoa "~/fancy-listener"
         0
         :interface :capi
         :keep-modules t) 