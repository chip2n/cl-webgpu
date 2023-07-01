(in-package #:webgpu.examples.triangle)

(defvar *instance* nil)
(defvar *surface* nil)
(defvar *adapter* nil)

(defun run ()
  (setup-repl)
  (w:with-engine
    (glfw:with-init-window (:title "Window test" :width 800 :height 600 :client-api :no-api)
      (unwind-protect
           (w:with-instance (instance)
             (let* ((xdisplay (glfw-get-x11-display))
                    (xwindow (glfw-get-x11-window glfw:*window*)))
               (w:with-surface (surface instance :xdisplay xdisplay :xwindow xwindow)
                 (setf *instance* instance)
                 (setf *surface* surface)

                 (let ((adapter (w:instance-request-adapter *instance* *surface*)))
                   (setf *adapter* adapter)
                   (loop until (glfw:window-should-close-p)
                         do (continuable
                              (handle-repl-events)
                              (glfw:poll-events)
                              ;; Should let wgpu swap buffers I guess
                              ;; (glfw:swap-buffers)
                              (sleep 0.016))))))
             (cleanup))))))

(defun cleanup ()
  (setf *instance* nil)
  (setf *surface* nil)
  (setf *adapter* nil))
