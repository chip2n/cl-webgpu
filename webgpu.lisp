(in-package #:webgpu)

;; * Entrypoint

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

(defmacro with-instance ((instance) &body body)
  (check-type instance symbol)
  `(let ((,instance (create-instance)))
     (unwind-protect ,@body
       (drop-instance ,instance))))

;; * Initialization

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

(defun create-x11-surface (instance x11-display x11-window)
  (with-foreign-objects ((type 'ffi::chained-struct)
                         (desc 'ffi::surface-descriptor)
                         (xlib-surface-desc 'ffi::surface-descriptor-from-xlib-window))
    (setf (foreign-slot-value type 'ffi::chained-struct 'ffi::next) (null-pointer))
    (setf (foreign-slot-value type 'ffi::chained-struct 'ffi::s-type) ffi::s-type-surface-descriptor-from-xlib-window)

    (setf (foreign-slot-value xlib-surface-desc 'ffi::surface-descriptor-from-xlib-window 'ffi::chain) type)
    (setf (foreign-slot-value xlib-surface-desc 'ffi::surface-descriptor-from-xlib-window 'ffi::display) x11-display)
    (setf (foreign-slot-value xlib-surface-desc 'ffi::surface-descriptor-from-xlib-window 'ffi::window) x11-window)

    (setf (foreign-slot-value desc 'ffi::surface-descriptor 'ffi::next-in-chain) xlib-surface-desc)
    (setf (foreign-slot-value desc 'ffi::surface-descriptor 'ffi::label) (null-pointer))

    (ffi::instance-create-surface (slot-value instance 'handle) desc)))

(defun create-metal-surface (instance metal-layer)
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

(defun surface-release (surface)
  (ffi::surface-release surface))

;;; * Adapter

(defvar *request-adapter-callback* nil)

(defcallback handle-request-adapter :void
    ((status ffi::request-adapter-status)
     (adapter ffi::adapter)
     (message :string)
     (userdata :pointer))
  (declare (ignore adapter userdata))
  (format t "STATUS: ~A (~A)~%" status message)
  (a:when-let ((callback *request-adapter-callback*))
    (format t "calling callback~%")
    (funcall callback)))

(defmethod instance-request-adapter ((instance webgpu-instance) surface &key callback)
  (with-foreign-object (options 'ffi::request-adapter-options)
    (with-foreign-slots (((next-in-chain ffi::next-in-chain)
                          (compatible-surface ffi::compatible-surface)
                          (power-preference ffi::power-preference)
                          (force-fallback-adapter ffi::force-fallback-adapter))
                         options
                         ffi::request-adapter-options)
      (setf next-in-chain (null-pointer))
      (setf compatible-surface (null-pointer))
      (setf power-preference ffi::power-preference-undefined)
      (setf force-fallback-adapter nil))
    (let ((*request-adapter-callback* callback))
      (ffi::instance-request-adapter (slot-value instance 'handle)
                                     options
                                     (callback handle-request-adapter)
                                     (null-pointer)))))
