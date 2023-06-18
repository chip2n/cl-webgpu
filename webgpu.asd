(asdf:defsystem #:webgpu
  :description "Bindings to WebGPU (via wgpu-native)"
  :author "Andreas Arvidsson <andreas@arvidsson.io>"
  :license "MIT"
  :version "0.0.1"
  :serial t
  :defsystem-depends-on (:cffi/c2ffi)
  :depends-on (#:alexandria #:cffi #:cffi/c2ffi #:cffi-libffi)
  :components ((:file "package")
               (:file "prelude")
               (:module "spec"
                :components
                ((:cffi/c2ffi-file
                  "webgpu-plus-wgpu.h"
                  :c2ffi-executable "/home/chip/dev/c2ffi/build/bin/c2ffi"
                  :package #:webgpu.ffi
                  :foreign-library-name "webgpu.ffi::libwebgpu"
                  :foreign-library-spec ((:unix "libwgpu_native.so"))
                  :ffi-name-transformer "webgpu.prelude::ffi-name-transformer"
                  :include-sources ()
                  :exclude-sources ("/usr/lib/clang/([^/]*)/include/(?!stddef.h)"))))
               (:file "webgpu")))
