;; Code from https://emacs.stackexchange.com/questions/28390/quickly-adjusting-text-to-dpi-changes

(defun my-dpi ()
  (let* ((attrs (car (display-monitor-attributes-list)))
         (size (assoc 'mm-size attrs))
         (sizex (cadr size))
         (res (cdr (assoc 'geometry attrs)))
         (resx (- (caddr res) (car res)))
         dpi)
    (catch 'exit
      ;; in terminal
      (unless sizex
        (throw 'exit 10))
      ;; on big screen
      (when (> sizex 1000)
        (throw 'exit 10))
      ;; DPI
      (* (/ (float resx) sizex) 25.4))))

(defun scale-font-size (size)
  (let ( (dpi (my-dpi)) )
  (cond
    ((> dpi 192) (* size 2))
    (t size))))
