;; https://emacs.stackexchange.com/questions/28390/quickly-adjusting-text-to-dpi-changes
(defun screen-dpi ()
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

(defun scale-font-size-to-dpi (size)
  (let ( (dpi (screen-dpi)) )
  (cond
    ((> dpi 192) (* size 2))
    (t size))))

(defun fix-font-scale (font)
  (setq fixed-size (scale-font-size-to-dpi (spacemacs/mplist-get-value font :size)))
  (setq new-font (copy-tree font))
  (plist-put (cdr new-font) :size fixed-size)
  new-font
  )

;; https://github.com/syl20bnr/spacemacs/issues/6197#issuecomment-224248780
(defun reset-default-font ()
  (setq-default
   dotspacemacs-default-font (fix-font-scale my-default-font))
  (unless (spacemacs/set-default-font dotspacemacs-default-font)
    (spacemacs-buffer/warning
     "Cannot find any of the specified fonts (%s)! Font settings may not be correct."
     (mapconcat 'car dotspacemacs-default-font ", ")))
  (remove-hook 'focus-in-hook #'reset-default-font))
