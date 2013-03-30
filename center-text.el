;;; center-text.el --- Center the text in a fixed-width column

;; Copyright (C) 2013 Pavel Panchekha <me@pavpanchekha.com>

;; Author: Pavel Panchekha <me@pavpanchekha.com>
;; Version: 0.8
;; Keywords: column center-text

;;; Commentary:

;; This package contains a minor mode to center the text of the
;; current buffer in a fixed-width column.  It's particularly nice to
;; use to edit plain text files with visual-line-mode.

(provide 'center-text)

;;; TODO
;; - Sometimes window-size-change-functions are simply not called

(define-minor-mode center-text-mode
  "Toggles center-text mode

With no argument, this command toggles the mode.
A positive argument turns the mode on.
A negative argument turns the mode off.

Center-text-mode causes Emacs to display the buffer contents
centered in a fixed-size column in the middle of the window.

Center-text-mode tries to recompute the margins necessary to
center the text with the given width every time the window size
changes.  However, Emacs doesn't always call the redisplay
handler.  In this case, you can force a redisplay with [C-x w],
or you can pass [C-x w] a prefix argument to change the column
width.
"

  :init-value nil
  :lighter " Center"
  :keymap `((,(kbd "C-x w") . center-text-set-width))
  :after-hook (center-text-mode-helper))

(defcustom center-text-default-width 80
  "The default width of the text column in center-text-mode"

  :type 'integer
  :group center-text-mode)

(defun center-text-mode-helper ()
  "Internal to center-text-mode"

  (if center-text-mode
      (center-text-mode-enable)
    (center-text-mode-disable)))

(defun center-text-mode-enable ()
  "Internal to center-text-mode"

  (make-local-variable 'center-text-width)
  (if (not (boundp 'center-text-width))
      (setf center-text-width center-text-default-width))

  (make-local-variable 'center-text-old-bg)
  (if (not (boundp 'center-text-old-bg))
      (setf center-text-old-bg (face-background 'fringe)))

  (set-face-background 'fringe (face-background 'default))
  
  (add-to-list 'window-size-change-functions
               'center-text-redisplay-helper)
   
  (center-text-center center-text-width))

(defun center-text-redisplay-helper (frame)
  "Internal to center-text-mode"

  (loop for window in (window-list frame)
        do
        (with-selected-window window
          (if (and center-text-mode
                   (boundp 'center-text-width))
              (center-text-center center-text-width))))

  (switch-to-buffer (current-buffer)))
            

(defun center-text-mode-disable ()
  "Internal to center-text-mode"

  (set-window-fringes nil nil nil)
  (if (boundp 'center-text-old-bg)
      (set-face-background 'fringe center-text-old-bg))
  (makunbound 'center-text-width)
  (makunbound 'center-text-old-bg)
  (setf window-size-change-functions
        (cl-remove 'center-text-redisplay-helper
                   window-size-change-functions
                   :count 1)))

(defun center-text-set-width (width)
  "Set the width of the main text column in center-text-mode.

  Set the width to WIDTH characters, which must be a positive integer.
  If WIDTH is 1, instead simply re-computes the margins.  This is
  sometimes necessary because Emacs doesn't properly fire window-size
  change hooks."

  (interactive "p")

  (if (= width 1)
      ; Then we're probably the non-prefix version, because no one
      ; wants a width-1 column of text.
      (center-text-center center-text-width)
    (center-text-center width)
    (setf center-text-width width)))

;;; center-text.el ends here

(defun center-text-center (max-chars)
  "Expand or shrink the fringe until the text width is
MAX-CHARS characters or fewer characters wide or less"

  (while (not (or (= (window-width) max-chars)
                  (and (< (window-width) max-chars)
                       (= (car (window-fringes)) 0))))
    (let ((char-width
           (* (aref (font-info (face-font 'default)) 2) (/ 1.0 12.0) 6.0))
          (current-chars
           (window-width))
          (current-fringe
           (cons (car (window-fringes)) (cadr (window-fringes)))))
      (let* ((excess-chars (- current-chars max-chars))
             (excess-width (* excess-chars char-width))
             (deficit-margin (floor (/ excess-width 2)))
             (left-fringe (max 0 (+ (car current-fringe) deficit-margin)))
             (right-fringe (max 0 (+ (cdr current-fringe) deficit-margin))))
        (set-window-fringes nil left-fringe right-fringe)))))
