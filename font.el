(defun screen-dpi ()
  "Return the screen DPI."
  ;; https://emacs.stackexchange.com/questions/28390/quickly-adjusting-text-to-dpi-changes."
  (let* ((attrs (car (display-monitor-attributes-list)))
         (size (assoc 'mm-size attrs))
         (sizex (cadr size))
         (res (cdr (assoc 'geometry attrs)))
         (resx (- (caddr res)
                  (car res)))
         dpi)
    (catch 'exit
      ;; in terminal
      (unless sizex
        (throw 'exit 10))
      ;; on big screen
      (when (> sizex 1000)
        (throw 'exit 10))
      ;; DPI
      (* (/ (float resx)
            sizex)
         25.4))))

(defun scale-font-size-to-dpi (size)
  "Scale a font size according to the screen DPI.
If the DPI is bigger than 192, this function returns a font size
two time bigger."
  (let ((dpi (screen-dpi)))
    (cond
     ((> dpi 192)
      (* size 2))
     (t size))))

(defun fix-font-scale (font)
  "Given a spacemacs font descriptor, return a copy of it with
the font size fixed according to DPI."
  (setq fixed-size (scale-font-size-to-dpi (spacemacs/mplist-get-value font :size)))
  (setq new-font (copy-tree font))
  (plist-put (cdr new-font)
             :size fixed-size)
  new-font)

(defun reset-default-font ()
  "This function reset the spacemacs font to `my-default-font'.
This function should be added as a hook in `focus-in-hook' and
auto-removes itself afterwards. It is useful for setting the
default font correctly in daemon mode, and take care of dealing
with the DPI."
  ;; https://github.com/syl20bnr/spacemacs/issues/6197#issuecomment-224248780
  (setq-default dotspacemacs-default-font (fix-font-scale my-default-font))
  (unless (spacemacs/set-default-font dotspacemacs-default-font)
    (spacemacs-buffer/warning "Cannot find any of the specified fonts (%s)! Font settings may not be correct."
                              (mapconcat 'car dotspacemacs-default-font
                                         ", ")))
  (remove-hook 'focus-in-hook #'reset-default-font))
