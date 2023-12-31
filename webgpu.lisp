(in-package #:webgpu)

;; NOTE: These are super unfinished. The goal is to expose functions to create
;; various descriptors (on the stack and on the heap). Currently we hard code a
;; bunch of stuff.

(defun setup-thread ()
  #+sbcl
  (sb-int:set-floating-point-modes :traps nil))

;; TODO shouldn't move to main thread on Linux
(defmacro with-engine (&body body)
  `(trivial-main-thread:with-body-in-main-thread (:blocking nil)
     (handler-bind ((error #'invoke-debugger))
       (progn
         (setup-thread)
         ,@body))))

;; * Initialization

(defmacro with-instance ((instance) &body body)
  (check-type instance symbol)
  `(let ((,instance (create-instance)))
     (unwind-protect ,@body
       (drop-instance ,instance))))

;; NOTE This is a workaround for this issue: https://github.com/cffi/cffi/issues/262
(defmethod cffi::translate-aggregate-to-foreign (ptr value (type cffi::foreign-typedef))
  (cffi::translate-aggregate-to-foreign ptr value (cffi::follow-typedefs type)))

(define-condition webgpu-error (error) ())

(define-condition webgpu-init-error (webgpu-error) ()
  (:report (lambda (c stream)
             (declare (ignore c))
             (format stream "WebGPU failed to initialize.~&"))))

;; TODO Automatic conversion between lisp and foreign type?

(defclass webgpu-instance ()
  ((handle :initarg :handle)))

(defun create-instance ()
  #+linux (create-x11-instance)
  #+darwin (create-metal-instance))

(defmethod drop-instance ((instance webgpu-instance))
  (ffi::instance-drop (slot-value instance 'handle)))

;;; * MacOS Metal (WIP)

(defclass webgpu-metal-instance (webgpu-instance) ())

(defun create-metal-instance ()
  (with-foreign-object (desc 'ffi::instance-descriptor)
    (setf (foreign-slot-value desc 'ffi::instance-descriptor 'ffi::next-in-chain)
          (null-pointer))
    (let ((instance (ffi::create-instance desc)))
      (when (null-pointer-p instance)
        (error 'webgpu-init-error))
      (make-instance 'webgpu-metal-instance :handle instance))))

;;; * Linux X11

(defmacro with-surface ((surface instance &rest options) &body body)
  (check-type surface symbol)
  `(let ((,surface (create-surface ,instance ,@options)))
     (unwind-protect ,@body
       (drop-surface ,surface))))

(defclass webgpu-x11-instance (webgpu-instance) ())

(defun create-x11-instance ()
  (with-foreign-object (desc 'ffi::instance-descriptor)
    ;; (setf (foreign-slot-value desc 'ffi::instance-descriptor 'ffi::next-in-chain) extras)
    (setf (foreign-slot-value desc 'ffi::instance-descriptor 'ffi::next-in-chain) (null-pointer))
    (let ((instance (ffi::create-instance desc)))
      (when (null-pointer-p instance)
        (error 'webgpu-init-error))
      (make-instance 'webgpu-x11-instance :handle instance))))

;;; * Surface

(defmethod create-surface ((instance webgpu-x11-instance) &key xdisplay xwindow)
  (assert xdisplay)
  (assert xwindow)
  (with-foreign-objects ((type 'ffi::chained-struct)
                         (desc 'ffi::surface-descriptor)
                         (xlib-surface-desc 'ffi::surface-descriptor-from-xlib-window))
    (setf (foreign-slot-value type 'ffi::chained-struct 'ffi::next) (null-pointer))
    (setf (foreign-slot-value type 'ffi::chained-struct 'ffi::s-type) ffi::s-type-surface-descriptor-from-xlib-window)

    (setf (foreign-slot-value xlib-surface-desc 'ffi::surface-descriptor-from-xlib-window 'ffi::chain) type)
    (setf (foreign-slot-value xlib-surface-desc 'ffi::surface-descriptor-from-xlib-window 'ffi::display) xdisplay)
    (setf (foreign-slot-value xlib-surface-desc 'ffi::surface-descriptor-from-xlib-window 'ffi::window) xwindow)

    (setf (foreign-slot-value desc 'ffi::surface-descriptor 'ffi::next-in-chain) xlib-surface-desc)
    (setf (foreign-slot-value desc 'ffi::surface-descriptor 'ffi::label) (null-pointer))

    (ffi::instance-create-surface (slot-value instance 'handle) desc)))

(defmethod create-surface ((instance webgpu-metal-instance) &key metal-layer)
  (assert metal-layer)
  (with-foreign-objects ((type 'ffi::chained-struct)
                         (desc 'ffi::surface-descriptor)
                         (metal-surface-desc 'ffi::surface-descriptor-from-metal-layer))
    (setf (foreign-slot-value type 'ffi::chained-struct 'ffi::next) (null-pointer))
    (setf (foreign-slot-value type 'ffi::chained-struct 'ffi::s-type) ffi::s-type-surface-descriptor-from-metal-layer)

    (setf (foreign-slot-value metal-surface-desc 'ffi::surface-descriptor-from-metal-layer 'ffi::chain) type)
    (setf (foreign-slot-value metal-surface-desc 'ffi::surface-descriptor-from-metal-layer 'ffi::layer) metal-layer)

    (setf (foreign-slot-value desc 'ffi::surface-descriptor 'ffi::next-in-chain) metal-surface-desc)
    (setf (foreign-slot-value desc 'ffi::surface-descriptor 'ffi::label) (null-pointer))

    (ffi::instance-create-surface (slot-value instance 'handle) desc)))

(defun drop-surface (surface)
  (ffi::surface-drop surface))

(defun surface-get-preferred-format (surface adapter)
  (ffi::surface-get-preferred-format surface adapter))

;;; * Adapter

(defvar *request-adapter-callback* nil)

(defmacro with-adapter ((adapter instance surface) &body body)
  (check-type adapter symbol)
  `(let ((,adapter (instance-request-adapter ,instance ,surface)))
     (unwind-protect ,@body
       (drop-adapter ,adapter))))

(defcallback handle-request-adapter :void
    ((status ffi::request-adapter-status)
     (adapter ffi::adapter)
     (message :string)
     (userdata :pointer))
  (declare (ignore userdata))
  (a:when-let ((callback *request-adapter-callback*))
    (funcall callback status adapter message)))

(defun instance-request-adapter (instance surface &key callback)
  (with-foreign-object (options 'ffi::request-adapter-options)
    (with-foreign-slots (((next-in-chain ffi::next-in-chain)
                          (compatible-surface ffi::compatible-surface)
                          (power-preference ffi::power-preference)
                          (force-fallback-adapter ffi::force-fallback-adapter))
                         options
                         ffi::request-adapter-options)
      (setf next-in-chain (null-pointer))
      (setf compatible-surface surface)
      (setf power-preference ffi::power-preference-undefined)
      (setf force-fallback-adapter nil))
    (let* ((obtained-adapter nil)
           (*request-adapter-callback* (lambda (status adapter message)
                                         (setf obtained-adapter adapter)
                                         (when callback
                                           (funcall callback status adapter message)))))
      (ffi::instance-request-adapter (slot-value instance 'handle)
                                     options
                                     (callback handle-request-adapter)
                                     (null-pointer))
      obtained-adapter)))

(defun drop-adapter (adapter)
  (ffi::adapter-drop adapter))

;;; * Device

(defvar *request-device-callback* nil)

(defmacro with-device ((device adapter) &body body)
  (check-type device symbol)
  `(let ((,device (adapter-request-device ,adapter)))
     (unwind-protect ,@body
       (drop-device ,device))))

(defcallback handle-request-device :void
    ((status ffi::request-device-status)
     (device ffi::device)
     (message :string)
     (userdata :pointer))
  (declare (ignore userdata))
  ;; TODO handle status
  (a:when-let ((callback *request-device-callback*))
    (funcall callback status device message)))

(defun adapter-request-device (adapter &key callback)
  (with-foreign-object (desc 'ffi::device-descriptor)
    (with-foreign-slots (((next-in-chain ffi::next-in-chain)
                          (label ffi::label)
                          (required-features-count ffi::required-features-count)
                          (required-limits ffi::required-limits)
                          (default-queue ffi::default-queue))
                         desc
                         ffi::device-descriptor)
      (setf next-in-chain (null-pointer))
      (setf label "Main device")
      (setf required-features-count 0)
      (setf required-limits (null-pointer))
      (setf (foreign-slot-value default-queue 'ffi::queue-descriptor 'ffi::next-in-chain) (null-pointer))
      (setf (foreign-slot-value default-queue 'ffi::queue-descriptor 'ffi::label) "Default queue"))

    (let* ((obtained-device nil)
           (*request-device-callback* (lambda (status device message)
                                        (setf obtained-device device)
                                        (when callback
                                          (funcall callback status device message)))))
      (ffi::adapter-request-device adapter
                                   desc
                                   (callback handle-request-device)
                                   (null-pointer))
      obtained-device)))

(defun drop-device (device)
  (ffi::device-drop device))

(defvar *device-uncaptured-error-callback* nil)

(defcallback handle-uncaptured-error :void
    ((type ffi::error-type)
     (message :string)
     (userdata :pointer))
  (declare (ignore userdata))
  (a:when-let ((callback *device-uncaptured-error-callback*))
    (funcall callback type message)))

(defun device-set-uncaptured-error-callback (device callback)
  (let ((*device-uncaptured-error-callback* callback))
    (ffi::device-set-uncaptured-error-callback device (callback handle-uncaptured-error) (null-pointer))))

(defvar *device-lost-callback* nil)

(defcallback handle-device-lost :void
    ((reason ffi::device-lost-reason)
     (message :string)
     (userdata :pointer))
  (declare (ignore userdata))
  (a:when-let ((callback *device-lost-callback*))
    (funcall callback reason message)))

(defun device-set-device-lost-callback (device callback)
  (let ((*device-lost-callback* callback))
    (ffi::device-set-device-lost-callback device (callback handle-device-lost) (null-pointer))))

;;; * Shaders

(defun create-shader-module (device source &key label)
  (with-foreign-objects ((type 'ffi::chained-struct)
                         (desc 'ffi::shader-module-descriptor)
                         (wgsl-desc 'ffi::shader-module-wgsl-descriptor))
    (setf (foreign-slot-value type 'ffi::chained-struct 'ffi::next) (null-pointer))
    (setf (foreign-slot-value type 'ffi::chained-struct 'ffi::s-type) ffi::s-type-shader-module-wgsl-descriptor)

    (setf (foreign-slot-value wgsl-desc 'ffi::shader-module-wgsl-descriptor 'ffi::chain) type)
    (setf (foreign-slot-value wgsl-desc 'ffi::shader-module-wgsl-descriptor 'ffi::code) source)

    (setf (foreign-slot-value desc 'ffi::shader-module-descriptor 'ffi::next-in-chain) wgsl-desc)
    (setf (foreign-slot-value desc 'ffi::shader-module-descriptor 'ffi::label) (or label (null-pointer)))

    (ffi::device-create-shader-module device desc)))

(defvar *handle-compilation-info-callback* nil)

(defcallback handle-compilation-info :void
    ((status ffi::compilation-info-request-status)
     (compilation-info (:pointer ffi::compilation-info))
     (userdata :pointer))
  (declare (ignore userdata))
  (format t "Got the compilation info (~A)~%" status)
  (a:when-let ((callback *handle-compilation-info-callback*))
    (funcall callback status compilation-info)))

;; NOTE: This is not yet implemented in wgpu-native as of 2023-07-07
(defun shader-module-get-compilation-info (shader-module callback)
  (let ((*handle-compilation-info-callback* (lambda (status info)
                                              (format t "Bro~%")
                                              (funcall callback status info))))
    (ffi::shader-module-get-compilation-info shader-module (callback handle-compilation-info) (null-pointer))))

;;; * Pipelines

(defun create-render-pipeline (device shader-module color-format)
  (with-foreign-objects ((desc 'ffi::render-pipeline-descriptor)
                         (pipeline-layout-desc 'ffi::pipeline-layout-descriptor)
                         (color-target-state 'ffi::color-target-state)
                         (vertex-state 'ffi::vertex-state)
                         (fragment-state 'ffi::fragment-state)
                         (primitive-state 'ffi::primitive-state)
                         (multisample-state 'ffi::multisample-state))
    (setf (foreign-slot-value pipeline-layout-desc 'ffi::pipeline-layout-descriptor 'ffi::next-in-chain) (null-pointer))
    (setf (foreign-slot-value pipeline-layout-desc 'ffi::pipeline-layout-descriptor 'ffi::label) "Pipeline layout")
    (setf (foreign-slot-value pipeline-layout-desc 'ffi::pipeline-layout-descriptor 'ffi::bind-group-layout-count) 0)
    (setf (foreign-slot-value pipeline-layout-desc 'ffi::pipeline-layout-descriptor 'ffi::bind-group-layouts) (null-pointer))

    (let ((pipeline-layout (ffi::device-create-pipeline-layout device pipeline-layout-desc)))
      (setf (foreign-slot-value vertex-state 'ffi::vertex-state 'ffi::next-in-chain) (null-pointer))
      (setf (foreign-slot-value vertex-state 'ffi::vertex-state 'ffi::module) shader-module)
      (setf (foreign-slot-value vertex-state 'ffi::vertex-state 'ffi::entry-point) "vs_main")
      (setf (foreign-slot-value vertex-state 'ffi::vertex-state 'ffi::constant-count) 0)
      (setf (foreign-slot-value vertex-state 'ffi::vertex-state 'ffi::constants) (null-pointer))
      (setf (foreign-slot-value vertex-state 'ffi::vertex-state 'ffi::buffer-count) 0)
      (setf (foreign-slot-value vertex-state 'ffi::vertex-state 'ffi::buffers) (null-pointer))

      (setf (foreign-slot-value color-target-state 'ffi::color-target-state 'ffi::next-in-chain) (null-pointer))
      (set-enum-slot color-target-state 'ffi::color-target-state 'ffi::format color-format)
      (setf (foreign-slot-value color-target-state 'ffi::color-target-state 'ffi::blend) (null-pointer))
      (setf (foreign-slot-value color-target-state 'ffi::color-target-state 'ffi::write-mask) ffi::color-write-mask-all)

      (setf (foreign-slot-value fragment-state 'ffi::fragment-state 'ffi::next-in-chain) (null-pointer))
      (setf (foreign-slot-value fragment-state 'ffi::fragment-state 'ffi::module) shader-module)
      (setf (foreign-slot-value fragment-state 'ffi::fragment-state 'ffi::entry-point) "fs_main")
      (setf (foreign-slot-value fragment-state 'ffi::fragment-state 'ffi::constant-count) 0)
      (setf (foreign-slot-value fragment-state 'ffi::fragment-state 'ffi::constants) (null-pointer))
      (setf (foreign-slot-value fragment-state 'ffi::fragment-state 'ffi::target-count) 1)
      (setf (foreign-slot-value fragment-state 'ffi::fragment-state 'ffi::targets) color-target-state)

      (setf (foreign-slot-value primitive-state 'ffi::primitive-state 'ffi::next-in-chain) (null-pointer))
      (setf (foreign-slot-value primitive-state 'ffi::primitive-state 'ffi::topology) ffi::primitive-topology-triangle-list)
      (set-enum-slot primitive-state 'ffi::primitive-state 'ffi::strip-index-format 'ffi::index-format-undefined)
      (set-enum-slot primitive-state 'ffi::primitive-state 'ffi::front-face 'ffi::front-face-ccw)

      (setf (foreign-slot-value multisample-state 'ffi::multisample-state 'ffi::next-in-chain) (null-pointer))
      (setf (foreign-slot-value multisample-state 'ffi::multisample-state 'ffi::count) 1)
      (setf (foreign-slot-value multisample-state 'ffi::multisample-state 'ffi::mask) 4294967295)
      (setf (foreign-slot-value multisample-state 'ffi::multisample-state 'ffi::alpha-to-coverage-enabled) nil)

      (setf (foreign-slot-value desc 'ffi::render-pipeline-descriptor 'ffi::next-in-chain) (null-pointer))
      (setf (foreign-slot-value desc 'ffi::render-pipeline-descriptor 'ffi::label) "Main pipeline")
      (setf (foreign-slot-value desc 'ffi::render-pipeline-descriptor 'ffi::layout) pipeline-layout)
      (setf (foreign-slot-value desc 'ffi::render-pipeline-descriptor 'ffi::vertex) vertex-state)
      (setf (foreign-slot-value desc 'ffi::render-pipeline-descriptor 'ffi::primitive) primitive-state)
      (setf (foreign-slot-value desc 'ffi::render-pipeline-descriptor 'ffi::depth-stencil) (null-pointer))
      (setf (foreign-slot-value desc 'ffi::render-pipeline-descriptor 'ffi::multisample) multisample-state)
      (setf (foreign-slot-value desc 'ffi::render-pipeline-descriptor 'ffi::fragment) fragment-state)

      (ffi::device-create-render-pipeline device desc))))

;; * Swap chain

(defun create-swap-chain (device surface color-format width height &key label)
  (with-foreign-objects ((desc 'ffi::swap-chain-descriptor))
    (setf (foreign-slot-value desc 'ffi::swap-chain-descriptor 'ffi::next-in-chain) (null-pointer))
    (setf (foreign-slot-value desc 'ffi::swap-chain-descriptor 'ffi::label) (or label (null-pointer)))
    ;; (setf (foreign-slot-value desc 'ffi::swap-chain-descriptor 'ffi::usage) 16)
    ;; (setf (foreign-slot-value desc 'ffi::swap-chain-descriptor 'ffi::usage) ffi::texture-usage-render-attachment)
    (setf (foreign-slot-value desc 'ffi::swap-chain-descriptor 'ffi::usage) (foreign-enum-value 'ffi::texture-usage 'ffi::texture-usage-render-attachment))
    (set-enum-slot desc 'ffi::swap-chain-descriptor 'ffi::format color-format)
    (setf (foreign-slot-value desc 'ffi::swap-chain-descriptor 'ffi::width) width)
    (setf (foreign-slot-value desc 'ffi::swap-chain-descriptor 'ffi::height) height)
    (set-enum-slot desc 'ffi::swap-chain-descriptor 'ffi::present-mode 'ffi::present-mode-fifo)

    (ffi::device-create-swap-chain device surface desc)))

(declaim (inline get-current-texture-view))
(defun get-current-texture-view (swap-chain)
  (ffi::swap-chain-get-current-texture-view swap-chain))

(declaim (inline swap-chain-present))
(defun swap-chain-present (swap-chain)
  (ffi::swap-chain-present swap-chain))

;; * Command encoder

(defun create-command-encoder (device &key label)
  (with-foreign-objects ((desc 'ffi::command-encoder-descriptor))
    (setf (foreign-slot-value desc 'ffi::command-encoder-descriptor 'ffi::next-in-chain) (null-pointer))
    (setf (foreign-slot-value desc 'ffi::command-encoder-descriptor 'ffi::label) (or label (null-pointer)))
    (ffi::device-create-command-encoder device desc)))

(defun command-encoder-finish (encoder &key label)
  (with-foreign-objects ((desc 'ffi::command-buffer-descriptor))
    (setf (foreign-slot-value desc 'ffi::command-buffer-descriptor 'ffi::next-in-chain) (null-pointer))
    (setf (foreign-slot-value desc 'ffi::command-buffer-descriptor 'ffi::label) (or label (null-pointer)))
    (ffi::command-encoder-finish encoder desc)))

;; * Render pass

(defun begin-render-pass (encoder texture &key label)
  (with-foreign-objects ((desc 'ffi::render-pass-descriptor)
                         (color-attachment 'ffi::render-pass-color-attachment)
                         (clear-color 'ffi::color))
    (setf (foreign-slot-value clear-color 'ffi::color 'ffi::r) 0d0)
    (setf (foreign-slot-value clear-color 'ffi::color 'ffi::g) 1d0)
    (setf (foreign-slot-value clear-color 'ffi::color 'ffi::b) 0d0)
    (setf (foreign-slot-value clear-color 'ffi::color 'ffi::a) 1d0)

    (setf (foreign-slot-value color-attachment 'ffi::render-pass-color-attachment 'ffi::view) texture)
    (setf (foreign-slot-value color-attachment 'ffi::render-pass-color-attachment 'ffi::resolve-target) (null-pointer))
    (setf (foreign-slot-value color-attachment 'ffi::render-pass-color-attachment 'ffi::load-op) ffi::load-op-clear)
    (setf (foreign-slot-value color-attachment 'ffi::render-pass-color-attachment 'ffi::store-op) ffi::store-op-store)
    (setf (foreign-slot-value color-attachment 'ffi::render-pass-color-attachment 'ffi::clear-value) clear-color)

    (setf (foreign-slot-value desc 'ffi::render-pass-descriptor 'ffi::next-in-chain) (null-pointer))
    (setf (foreign-slot-value desc 'ffi::render-pass-descriptor 'ffi::label) (or label (null-pointer)))
    (setf (foreign-slot-value desc 'ffi::render-pass-descriptor 'ffi::color-attachment-count) 1)
    (setf (foreign-slot-value desc 'ffi::render-pass-descriptor 'ffi::color-attachments) color-attachment)
    (setf (foreign-slot-value desc 'ffi::render-pass-descriptor 'ffi::depth-stencil-attachment) (null-pointer))
    (setf (foreign-slot-value desc 'ffi::render-pass-descriptor 'ffi::occlusion-query-set) (null-pointer))
    (setf (foreign-slot-value desc 'ffi::render-pass-descriptor 'ffi::timestamp-write-count) 0)
    (setf (foreign-slot-value desc 'ffi::render-pass-descriptor 'ffi::timestamp-writes) (null-pointer))

    (ffi::command-encoder-begin-render-pass encoder desc)))

(declaim (inline render-pass-encoder-set-pipeline))
(defun render-pass-encoder-set-pipeline (encoder pipeline)
  (ffi::render-pass-encoder-set-pipeline encoder pipeline))

(declaim (inline render-pass-encoder-draw))
(defun render-pass-encoder-draw (encoder vertex-count instance-count first-vertex first-instance)
  (ffi::render-pass-encoder-draw encoder vertex-count instance-count first-vertex first-instance))

(declaim (inline render-pass-encoder-end))
(defun render-pass-encoder-end (encoder)
  (ffi::render-pass-encoder-end encoder))

;; * Queue

(declaim (inline get-queue))
(defun get-queue (device)
  (ffi::device-get-queue device))

(declaim (inline queue-submit))
(defun queue-submit (queue command-buffer)
  (ffi::queue-submit queue 1 command-buffer))

;; * Texture

(declaim (inline texture-view-drop))
(defun texture-view-drop (texture-view)
  (ffi::texture-view-drop texture-view))

;; * Utils

;; Due to generated typedefs by C2FFI (and what I assume is a bug), we can't
;; SETF through FOREIGN-SLOT-VALUE directly. So we workaround this by using the
;; backing types for the enum.
(defun set-enum-slot (ptr type slot-name value)
  (let* ((slot-info (cffi::get-slot-info type slot-name))
         (offset (cffi::slot-offset slot-info))
         (slot-type (cffi::parse-type (cffi::slot-type slot-info))))
    (setf (mem-ref ptr (cffi::canonicalize slot-type) offset)
          (cffi:foreign-enum-value slot-type value))))
