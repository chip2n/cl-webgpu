(defpackage #:webgpu.prelude
  (:use #:cl #:arrow-macros)
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
   #:device-set-device-lost-callback
   #:create-shader-module
   #:surface-get-preferred-format
   #:create-render-pipeline
   #:shader-module-get-compilation-info
   #:create-swap-chain
   #:get-current-texture-view
   #:create-command-encoder
   #:begin-render-pass
   #:command-encoder-finish
   #:render-pass-encoder-set-pipeline
   #:render-pass-encoder-end
   #:render-pass-encoder-draw
   #:queue-submit
   #:texture-view-drop
   #:swap-chain-present
   #:get-queue))
