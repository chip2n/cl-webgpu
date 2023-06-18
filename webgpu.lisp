(in-package #:webgpu)

(define-condition webgpu-error (error) ())

(define-condition webgpu-init-error (webgpu-error) ()
  (:report (lambda (c stream)
             (declare (ignore c))
             (format stream "WebGPU failed to initialize.~&"))))

(defclass webgpu-instance ()
  ((handle :initarg :handle)))

(defclass webgpu-x11-instance (webgpu-instance) ())

(defun create-instance ()
  (cffi:with-foreign-object (desc 'ffi::instance-descriptor)
    (setf (cffi:foreign-slot-value desc 'ffi::instance-descriptor 'ffi::next-in-chain)
          (cffi:null-pointer))
    (let ((instance (ffi::create-instance desc)))
      (when (cffi:null-pointer-p instance)
        (error 'webgpu-init-error))
      ;; TODO support other platforms
      (make-instance 'webgpu-x11-instance :handle instance))))

(defmethod drop-instance ((instance webgpu-instance))
  (ffi::instance-drop (slot-value instance 'handle)))

(defmethod create-surface ((instance webgpu-x11-instance))
  (cffi:with-foreign-objects ((type 'ffi::chained-struct)
                              (desc 'ffi::surface-descriptor)
                              (xlib-surface-desc 'ffi::surface-descriptor-from-xlib-window))
    ;; TODO do we need to care about the `next` field here?
    (setf (cffi:foreign-slot-value type 'ffi::chained-struct 'ffi::s-type) ffi::s-type-surface-descriptor-from-xlib-window)
    (setf (cffi:foreign-slot-value xlib-surface-desc 'ffi::surface-descriptor-from-xlib-window 'ffi::chain) type
          (cffi:foreign-slot-value xlib-surface-desc 'ffi::surface-descriptor-from-xlib-window 'ffi::display) display
          (cffi:foreign-slot-value xlib-surface-desc 'ffi::surface-descriptor-from-xlib-window 'ffi::window) window)
    (setf (cffi:foreign-slot-value desc 'ffi::surface-descriptor 'ffi::next-in-chain) xlib-surface-desc))
  (webgpu.ffi::instance-create-surface (slot-value instance 'handle) desc))
