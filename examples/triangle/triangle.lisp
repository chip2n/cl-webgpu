(in-package #:webgpu.examples.triangle)

(defvar *instance* nil)
(defvar *surface* nil)
(defvar *adapter* nil)
(defvar *device* nil)
(defvar *shader* nil)

(defvar *shader-src* "
@vertex
fn vs_main(@builtin(vertex_index) in_vertex_index: u32) -> @builtin(position) vec4<f32> {
    let x = f32(i32(in_vertex_index) - 1);
    let y = f32(i32(in_vertex_index & 1u) * 2 - 1);
    return vec4<f32>(x, y, 0.0, 1.0);
}

@fragment
fn fs_main() -> @location(0) vec4<f32> {
    return vec4<f32>(1.0, 0.0, 0.0, 1.0);
}")

(defun run ()
  (setup-repl)
  (w:with-engine
    (glfw:with-init-window (:title "Window test" :width 800 :height 600 :client-api :no-api)
      (unwind-protect
           (w:with-instance (instance)
             (setf *instance* instance)
             (let* ((xdisplay (glfw-get-x11-display))
                    (xwindow (glfw-get-x11-window glfw:*window*)))
               (w:with-surface (surface instance :xdisplay xdisplay :xwindow xwindow)
                 (setf *surface* surface)
                 (w:with-adapter (adapter instance surface)
                   (setf *adapter* adapter)
                   (w:with-device (device adapter)
                     (setf *device* device)
                     (w:device-set-uncaptured-error-callback device (lambda (type msg) (format t "Uncaptured device error (~A): ~A~%" type msg)))
                     (w:device-set-device-lost-callback device (lambda (reason msg) (format t "Device lost (~A): ~A~%" reason msg)))

                     (setf *shader* (w:create-shader-module device *shader-src* :label "Main shader"))

                     (loop until (glfw:window-should-close-p)
                           do (continuable
                                (handle-repl-events)
                                (glfw:poll-events)
                                ;; Should let wgpu swap buffers I guess
                                ;; (glfw:swap-buffers)
                                (sleep 0.016)))))))
             (cleanup))))))

(defun cleanup ()
  (setf *instance* nil)
  (setf *surface* nil)
  (setf *adapter* nil)
  (setf *device* nil)
  (setf *shader* nil))
