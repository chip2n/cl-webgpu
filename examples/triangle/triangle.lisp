(in-package #:webgpu.examples.triangle)

(defvar *instance* nil)
(defvar *surface* nil)
(defvar *adapter* nil)
(defvar *device* nil)
(defvar *queue* nil)
(defvar *shader* nil)
(defvar *pipeline* nil)
(defvar *swap-chain* nil)

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

(defun render ()
  (format t "RENDER ~A~%" *swap-chain*)
  (finish-output)
  (glfw:poll-events)
  (let* ((texture (w:get-current-texture-view *swap-chain*))
         (command-encoder (w:create-command-encoder *device*))
         (render-pass-encoder (w:begin-render-pass command-encoder texture)))
    (w:render-pass-encoder-set-pipeline render-pass-encoder *pipeline*)
    (w:render-pass-encoder-draw render-pass-encoder 3 1 0 0)
    (w:render-pass-encoder-end render-pass-encoder)
    (w:texture-view-drop texture)
    ;; (let ((command-buffer (w:command-encoder-finish command-encoder)))
    ;;   (w:queue-submit *queue* command-buffer))
    )
  (w:swap-chain-present *swap-chain*)
  (sleep 0.016))

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

                     (setf *queue* (w:get-queue device))
                     (setf *shader* (w:create-shader-module device *shader-src* :label "Main shader"))
                     (let ((color-format (w:surface-get-preferred-format *surface* *adapter*)))
                       (setf *pipeline* (w:create-render-pipeline *device* *shader* color-format))
                       (setf *swap-chain* (w:create-swap-chain device surface color-format 800 600))

                       (loop until (glfw:window-should-close-p)
                             do (continuable
                                  (handle-repl-events)
                                  (render))))))))
             (cleanup))))))

(defun cleanup ()
  (setf *instance* nil)
  (setf *surface* nil)
  (setf *adapter* nil)
  (setf *device* nil)
  (setf *queue* nil)
  (setf *shader* nil)
  (setf *pipeline* nil)
  (setf *swap-chain* nil))
