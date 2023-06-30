(in-package #:webgpu.examples.core)

(defmacro continuable (&body body)
  `(restart-case
       (progn ,@body)
     (continue () :report "Continue")))

(defun setup-repl ()
  #+(and slynk linux)
  (slynk-mrepl:send-prompt))

(defun handle-repl-events ()
  #+(and slynk linux)
  (slynk:process-requests t))

;; * GLFW native functions

;; These are not defined in cl-glfw3 currently

#+darwin
(cffi:defcfun (glfw-get-cocoa-window "glfwGetCocoaWindow") :pointer
  (window %glfw::window))

#+linux
(cffi:defcfun (glfw-get-x11-display "glfwGetX11Display") :pointer)

#+linux
(cffi:defcfun (glfw-get-x11-window  "glfwGetX11Window") :uint32
  (window %glfw::window))
