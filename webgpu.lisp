(in-package #:webgpu)

;; * Entrypoint

(defun setup-thread ()
  #+sbcl
  (sb-int:set-floating-point-modes :traps nil))

;; TODO shouldn't move to main thread on Linux
(defmacro with-engine (&body body)
  `(trivial-main-thread:with-body-in-main-thread (:blocking nil)
     (handler-case
         (progn
           (setup-thread)
           ,@body)
       (error (c)
         ;; We invoke the debugger manually here, because trivial-main-thread
         ;; only marks the task created as :errored
         (invoke-debugger c)))))

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

;; TODO basically same as X11, reuse code?
(defun create-metal-instance ()
  (cffi:with-foreign-object (desc 'ffi::instance-descriptor)
    (setf (cffi:foreign-slot-value desc 'ffi::instance-descriptor 'ffi::next-in-chain)
          (cffi:null-pointer))
    (let ((instance (ffi::create-instance desc)))
      (when (cffi:null-pointer-p instance)
        (error 'webgpu-init-error))
      (make-instance 'webgpu-metal-instance :handle instance))))

;; TODO should share code with X11 (requires specific args)
(defmethod create-metal-surface ((instance webgpu-metal-instance) metal-layer)
  ;; TODO
  ;; (cffi:with-foreign-objects ((type '(:struct ffi::chained-struct))
  ;;                             (desc '(:struct ffi::surface-descriptor))
  ;;                             (xlib-surface-desc '(:struct ffi::surface-descriptor-from-xlib-window))))

  (let ((type (cffi:foreign-alloc '(:struct ffi::chained-struct)))
        (desc (cffi:foreign-alloc '(:struct ffi::surface-descriptor)))
        (metal-surface-desc (cffi:foreign-alloc '(:struct ffi::surface-descriptor-from-metal-layer))))

    ;; TODO not used
    (setf (cffi:foreign-slot-value type '(:struct ffi::chained-struct) 'ffi::next) (cffi:null-pointer))
    (setf (cffi:foreign-slot-value type '(:struct ffi::chained-struct) 'ffi::s-type) ffi::s-type-surface-descriptor-from-metal-layer)

    ;; -----------------------

    (setf (cffi:foreign-slot-value
           (cffi:foreign-slot-value metal-surface-desc '(:struct ffi::surface-descriptor-from-metal-layer) 'ffi::chain)
           '(:struct ffi::chained-struct)
           'ffi::next)
          (cffi:null-pointer))
    (setf (cffi:foreign-slot-value
           (cffi:foreign-slot-value metal-surface-desc '(:struct ffi::surface-descriptor-from-metal-layer) 'ffi::chain)
           '(:struct ffi::chained-struct)
           'ffi::s-type)
          ffi::s-type-surface-descriptor-from-metal-layer)
    (setf (cffi:foreign-slot-value metal-surface-desc '(:struct ffi::surface-descriptor-from-metal-layer) 'ffi::layer) metal-layer)

    ;; -----------------------

    (setf (cffi:foreign-slot-value desc '(:struct ffi::surface-descriptor) 'ffi::next-in-chain) metal-surface-desc)
    (setf (cffi:foreign-slot-value desc '(:struct ffi::surface-descriptor) 'ffi::label) (cffi:null-pointer))

    ;; (values
    ;;  desc
    ;;  type
    ;;  metal-surface-desc
    ;;  (cffi:foreign-slot-value desc '(:struct ffi::surface-descriptor) 'ffi::next-in-chain)
    ;;  )
    (webgpu.ffi::instance-create-surface (slot-value instance 'handle) desc)
    ;; desc
    ))

;;; * Linux X11

(defclass webgpu-x11-instance (webgpu-instance) ())

;; TODO This currently creates X11 instance, but should create an appropriate
;; instance for current platform
(defun create-x11-instance ()
  (cffi:with-foreign-object (desc 'ffi::instance-descriptor)
    ;; (setf (cffi:foreign-slot-value desc 'ffi::instance-descriptor 'ffi::next-in-chain) extras)
    (setf (cffi:foreign-slot-value desc 'ffi::instance-descriptor 'ffi::next-in-chain) (cffi:null-pointer))
    (let ((instance (ffi::create-instance desc)))
      (when (cffi:null-pointer-p instance)
        (error 'webgpu-init-error))
      ;; TODO support other platforms
      (make-instance 'webgpu-x11-instance :handle instance))))

(defmethod create-surface ((instance webgpu-x11-instance) x11-display x11-window)
  ;; (cffi:with-foreign-objects ((type '(:struct ffi::chained-struct))
  ;;                             (desc '(:struct ffi::surface-descriptor))
  ;;                             (xlib-surface-desc '(:struct ffi::surface-descriptor-from-xlib-window))))

  (let ((type (cffi:foreign-alloc '(:struct ffi::chained-struct)))
        (desc (cffi:foreign-alloc '(:struct ffi::surface-descriptor)))
        (xlib-surface-desc (cffi:foreign-alloc '(:struct ffi::surface-descriptor-from-xlib-window))))

    ;; (CFFI:DEFCSTRUCT (CHAINED-STRUCT :SIZE 16)
    ;;   (NEXT (:POINTER :VOID) :OFFSET 0)
    ;;   (S-TYPE STYPE :OFFSET 8))
    (setf (cffi:foreign-slot-value type '(:struct ffi::chained-struct) 'ffi::next) (cffi:null-pointer))
    (setf (cffi:foreign-slot-value type '(:struct ffi::chained-struct) 'ffi::s-type) ffi::s-type-surface-descriptor-from-xlib-window)

    ;; -----------------------

    ;; (CFFI:DEFCSTRUCT (SURFACE-DESCRIPTOR-FROM-XLIB-WINDOW :SIZE 32)
    ;;   (CHAIN CHAINED-STRUCT :OFFSET 0)
    ;;   (DISPLAY (:POINTER :VOID) :OFFSET 16)
    ;;   (WINDOW UINT32_T :OFFSET 24))
    ;; (setf (cffi:foreign-slot-value xlib-surface-desc '(:struct ffi::surface-descriptor-from-xlib-window) 'ffi::chain) type)
    (setf (cffi:foreign-slot-value
           (cffi:foreign-slot-value xlib-surface-desc '(:struct ffi::surface-descriptor-from-xlib-window) 'ffi::chain)
           '(:struct ffi::chained-struct)
           'ffi::next)
          (cffi:null-pointer))
    (setf (cffi:foreign-slot-value
           (cffi:foreign-slot-value xlib-surface-desc '(:struct ffi::surface-descriptor-from-xlib-window) 'ffi::chain)
           '(:struct ffi::chained-struct)
           'ffi::s-type)
          ffi::s-type-surface-descriptor-from-xlib-window)
    (setf (cffi:foreign-slot-value xlib-surface-desc '(:struct ffi::surface-descriptor-from-xlib-window) 'ffi::display) x11-display)
    (setf (cffi:foreign-slot-value xlib-surface-desc '(:struct ffi::surface-descriptor-from-xlib-window) 'ffi::window) x11-window)

    ;; -----------------------

    ;; (CFFI:DEFCSTRUCT (SURFACE-DESCRIPTOR :SIZE 16)
    ;;   (NEXT-IN-CHAIN (:POINTER CHAINED-STRUCT) :OFFSET 0)
    ;;   (LABEL :STRING :OFFSET 8))
    (setf (cffi:foreign-slot-value desc '(:struct ffi::surface-descriptor) 'ffi::next-in-chain) xlib-surface-desc)
    (setf (cffi:foreign-slot-value desc '(:struct ffi::surface-descriptor) 'ffi::label) (cffi:null-pointer))

    ;; (values
    ;;  desc
    ;;  type
    ;;  xlib-surface-desc
    ;;  (cffi:foreign-slot-value desc '(:struct ffi::surface-descriptor) 'ffi::next-in-chain)
    ;;  )
    (ffi::instance-create-surface (slot-value instance 'handle) desc)
    ;; desc
    ))

;;; * Adapter

(cffi:defcallback handle-request-adapter :void
    ((status ffi::request-adapter-status)
     (adapter ffi::adapter)
     (message :string)
     (userdata :pointer))
  (declare (ignore adapter userdata))
  (format t "STATUS: ~A (~A)~%" status message))

(defmethod instance-request-adapter ((instance webgpu-instance) surface)
  (cffi:with-foreign-object (options 'ffi::request-adapter-options)
    (cffi:with-foreign-slots (((next-in-chain ffi::next-in-chain)
                               (compatible-surface ffi::compatible-surface)
                               (power-preference ffi::power-preference)
                               (force-fallback-adapter ffi::force-fallback-adapter))
                              options
                              ffi::request-adapter-options)
      (setf next-in-chain (cffi:null-pointer))
      (setf compatible-surface (cffi:null-pointer))
      (setf power-preference ffi::power-preference-undefined)
      (setf force-fallback-adapter nil))
    (ffi::instance-request-adapter (slot-value instance 'handle)
                                   options
                                   (cffi:callback handle-request-adapter)
                                   (cffi:null-pointer))))
