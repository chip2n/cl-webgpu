(in-package #:webgpu.prelude)

(defun ffi-name-transformer (name kind &key &allow-other-keys)
  (declare (ignore kind))
  (cond
    ((alexandria:starts-with-subseq "wgpu" name)
     (setf name (subseq name 4)))
    ((alexandria:starts-with-subseq "WGPU" name)
     (setf name (remove #\_ (subseq name 4)))))
  (string-upcase (cffi/c2ffi:maybe-camelcase-to-dash-separated name)))

#+nil
(progn
  (ffi-name-transformer "WGPUPowerPreference_Undefined" nil)
  (ffi-name-transformer "WGPUSType_SurfaceDescriptorFromMetalLayer" nil)
  )
