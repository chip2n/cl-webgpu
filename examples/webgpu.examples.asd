(asdf:defsystem #:webgpu.examples
  :description "Examples for cl-webgpu"
  :author "Andreas Arvidsson <andreas@arvidsson.io>"
  :license "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:alexandria #:cl-glfw3 #:webgpu)
  :components ((:file "package")
               (:file "triangle/triangle")))
