(in-package #:webgpu.prelude)

(defun ffi-name-transformer (name kind &key &allow-other-keys)
  (declare (ignore kind))
  ;; chop off prefixes
  (cond
    ((alexandria:starts-with-subseq "WGPUSType_" name)
     (setf name (concatenate 'string "SType" (subseq name 10))))
    ((alexandria:starts-with-subseq "wgpu" name)
     (setf name (subseq name 4)))
    ((alexandria:starts-with-subseq "WGPU" name)
     (setf name (subseq name 4))))
  (string-upcase (cffi/c2ffi:maybe-camelcase-to-dash-separated name)))
