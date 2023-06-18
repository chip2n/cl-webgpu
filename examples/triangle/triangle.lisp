(in-package #:webgpu.examples.triangle)

;; NOTE: These are missing from cl-glfw3
(cffi:defcfun (glfw-get-x11-display "glfwGetX11Display") :pointer)
(cffi:defcfun (glfw-get-x11-window  "glfwGetX11Window") :uint32
  (window %glfw::window))

(defun setup-repl ()
  #+slynk
  (slynk-mrepl:send-prompt))

(defun handle-repl-events ()
  #+slynk
  (slynk:process-requests t))

(defun run ()
  (setup-repl)
  (glfw:with-init-window (:title "Window test" :width 800 :height 600)
    (let ((instance (w:create-instance)))
      (loop until (glfw:window-should-close-p)
            do (handle-repl-events)
               (glfw:poll-events)
               (glfw:swap-buffers))
      (w:drop-instance instance))))
