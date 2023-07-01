(in-package #:webgpu)

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
