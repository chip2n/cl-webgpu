(defun webgpu/sly-start ()
  "Start SLY with example system loaded."
  (interactive)
  (sly-start :program inferior-lisp-program
             :init-function (lambda ()
                              (sly-eval `(quicklisp:quickload "webgpu.examples"))
                              (sly-switch-package "#:webgpu.examples.triangle"))))

(provide 'cl-webgpu)
