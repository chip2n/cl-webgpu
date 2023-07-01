(defpackage #:webgpu.prelude
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria)))

(defpackage #:webgpu.ffi
  (:use #:cl))

(defpackage #:webgpu
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria)
                    (#:ffi #:webgpu.ffi))
  (:export
   #:create-instance
   #:drop-instance
   #:create-surface
   #:webgpu-x11-instance
   #:webgpu-instance
   #:webgpu-error
   #:webgpu-init-error
   #:with-engine))
