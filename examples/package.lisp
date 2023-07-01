(defpackage #:webgpu.examples.core
  (:use #:cl #:webgpu)
  (:local-nicknames (#:a #:alexandria)
                    (#:w #:webgpu))
  (:export
   #:continuable
   #:setup-repl
   #:handle-repl-events
   #:glfw-get-cocoa-window
   #:glfw-get-x11-display
   #:glfw-get-x11-window))

(defpackage #:webgpu.examples.triangle
  (:use #:cl #:webgpu #:webgpu.examples.core)
  (:local-nicknames (#:a #:alexandria)
                    (#:w #:webgpu)))
