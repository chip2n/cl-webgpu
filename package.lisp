(defpackage #:webgpu.prelude
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria)))

(defpackage #:webgpu.ffi
  (:use #:cl))

(defpackage #:webgpu
  (:use #:cl #:cffi)
  (:local-nicknames (#:a #:alexandria)
                    (#:ffi #:webgpu.ffi))
  (:export
   #:create-instance
   #:drop-instance
   #:create-surface
   #:drop-surface
   #:webgpu-x11-instance
   #:webgpu-instance
   #:webgpu-error
   #:webgpu-init-error
   #:with-engine
   #:with-instance
   #:with-surface
   #:instance-request-adapter
   #:drop-adapter
   #:with-adapter
   #:adapter-request-device
   #:drop-device
   #:with-device
   #:device-set-uncaptured-error-callback
   #:device-set-device-lost-callback))
