(in-package #:webgpu.examples.triangle)

(cffi:define-foreign-library metalwrapper
  (:darwin "libmetalwrapper.dylib"))

(cffi:use-foreign-library metalwrapper)

(cffi:defcfun (init-metal "clwInitMetal") :pointer
  (window :pointer))

(defvar *instance* nil)
(defvar *surface* nil)

(defun run ()
  (setup-repl)
  (w:with-engine
    (glfw:with-init-window (:title "Window test" :width 800 :height 600 :client-api :no-api)
      (let* ((instance (w::create-instance))
             (metal-layer (init-metal (glfw-get-cocoa-window glfw:*window*)))
             (surface (w::create-metal-surface instance metal-layer)))
        (setf *instance* instance)
        (setf *surface* surface)
        (loop until (glfw:window-should-close-p)
              do (continuable
                   (handle-repl-events)
                   (glfw:poll-events)
                   ;; Should let wgpu swap buffers I guess
                   ;; (glfw:swap-buffers)
                   (sleep 0.016)))
        (w:drop-instance *instance*)
        (setf *instance* nil)
        (setf *surface* nil)))))
