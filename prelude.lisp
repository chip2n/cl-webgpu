(in-package #:webgpu.prelude)

(defun ffi-name-transformer (name kind &key &allow-other-keys)
  (declare (ignore kind))
  (cond
    ((alexandria:starts-with-subseq "wgpu" name)
     (setf name (subseq name 4)))
    ((alexandria:starts-with-subseq "WGPU" name)
     (setf name (remove #\_ (subseq name 4)))))
  (->> name
    (str:replace-all "FrontFaceCCW" "FrontFaceCcw")
    (str:replace-all "FrontFaceCW" "FrontFaceCw")
    (cffi/c2ffi:maybe-camelcase-to-dash-separated)
    (string-upcase)
    (str:replace-all "W-G-S-L" "WGSL")
    (str:replace-all "S-P-I-R-V" "SPIRV")))

#+nil
(progn
  (ffi-name-transformer "WGPUPowerPreference_Undefined" nil)
  (ffi-name-transformer "WGPUSType_SurfaceDescriptorFromMetalLayer" nil)
  (ffi-name-transformer "WGPUShaderModuleWGSLDescriptor" nil)
  (ffi-name-transformer "WGPUSType_ShaderModuleSPIRVDescriptor" nil)
  (ffi-name-transformer "WGPUFrontFace_CCW" nil)
  )
