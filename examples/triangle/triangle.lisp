(in-package #:webgpu.examples.triangle)

(defvar *instance* nil)
(defvar *surface* nil)

(defun run ()
  (setup-repl)
  (w:with-engine
    (glfw:with-init-window (:title "Window test" :width 800 :height 600 :client-api :no-api)
      (unwind-protect
           (w:with-instance (instance)
             (let* ((x11-display (glfw-get-x11-display))
                    (x11-window (glfw-get-x11-window glfw:*window*))
                    (surface (w::create-x11-surface instance x11-display x11-window)))
               (setf *instance* instance)
               (setf *surface* surface)
               (loop until (glfw:window-should-close-p)
                     do (continuable
                          (handle-repl-events)
                          (glfw:poll-events)
                          ;; Should let wgpu swap buffers I guess
                          ;; (glfw:swap-buffers)
                          (sleep 0.016))))
             (cleanup))))))

(defun cleanup ()
  (setf *instance* nil)
  (setf *surface* nil))
