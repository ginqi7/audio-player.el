;;; audio-player-ui.el ---                           -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Qiqi Jin

;; Author: Qiqi Jin <ginqi7@gmail.com>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.
;;; Commentary:
;;
;;; Code:
(require 'audio-player)

(require 'vui)

(defconst audio-player-ui--buffer-name "*audio-player-now-playing*"
  "Buffer name for the audio-player-ui now-playing child frame.")

(defconst audio-player-ui--frame-border-width 1
  "Width in pixels of the now-playing child-frame border.")

(defconst audio-player-ui--progress-bar-max-width 16
  "Maximum number of cells used for the now-playing progress bar.")

(defconst audio-player-ui--progress-bar-min-width 5
  "Minimum number of cells used for the now-playing progress bar.")

(defface audio-player-ui-child-frame-border
  '((((class color) (background light)) (:background "#8a8a8a"))
    (((class color) (background dark)) (:background "#6f6f6f"))
    (t (:background "gray50")))
  "Face for the now-playing child-frame border."
  :group 'audio-player-ui)

(defcustom audio-player-ui-child-frame-width 34
  "Fallback width of the now-playing child frame in character columns.
The frame normally fits itself to the displayed cover image."
  :type '(restricted-sexp
          :match-alternatives
          ((lambda (value)
             (and (integerp value) (> value 0)))))
  :group 'audio-player-ui)

(defvar audio-player-ui--frame nil
  "Child frame currently showing the now-playing buffer.")

(defvar audio-player-ui--frame-manual-position nil
  "Manual pixel position for the now-playing child frame.")

(defvar audio-player-ui--vui-component nil
  "The VUI component tree for the now-playing display.")

(defun audio-player-ui--apply-child-frame-border-face (frame)
  "Apply audio-player-ui child-frame border styling to FRAME."
  (let ((background (face-background 'audio-player-ui-child-frame-border frame t)))
    (set-face-background 'child-frame-border
                         (or background "gray50")
                         frame)))

(defun audio-player-ui--buffer-image (buffer)
  "Return the first image displayed in BUFFER, or nil."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (let (image)
        (while (and (not image) (not (eobp)))
          (when-let* ((display (get-text-property (point) 'display))
                      ((eq (car-safe display) 'image)))
            (setq image display))
          (goto-char (or (next-single-property-change (point) 'display)
                         (point-max))))
        image))))

(defun audio-player-ui--constrain-frame-position (frame position)
  "Return POSITION constrained within FRAME's parent."
  (if-let* ((parent (frame-parent frame)))
      (cons (max 0 (min (car position)
                        (max 0 (- (frame-pixel-width parent)
                                  (frame-pixel-width frame)))))
            (max 0 (min (cdr position)
                        (max 0 (- (frame-pixel-height parent)
                                  (frame-pixel-height frame))))))
    position))

(defun audio-player-ui--default-frame-position (frame)
  "Return FRAME's default lower-right child-frame position."
  (when-let* ((parent (frame-parent frame)))
    (let* ((bottom-window (car (window-at-side-list parent 'bottom)))
           (mode-line-height (if bottom-window
                                 (window-mode-line-height bottom-window)
                               0))
           (content-bottom (- (frame-pixel-height parent)
                              (window-pixel-height (minibuffer-window parent))
                              mode-line-height))
           (x-margin (* 2 (frame-char-width parent)))
           (y-margin (frame-char-height parent)))
      (cons (max 0 (- (frame-pixel-width parent)
                      (frame-pixel-width frame)
                      x-margin))
            (max 0 (- content-bottom
                      (frame-pixel-height frame)
                      y-margin))))))

(defun audio-player-ui--disable-ui-line-wrapping ()
  "Disable wrapping and extra line spacing in the current UI buffer."
  (setq-local truncate-lines t)
  (setq-local word-wrap nil)
  ;; Positive `line-spacing' creates visible seams between sliced images.
  (setq-local line-spacing 0))

(defun audio-player-ui--ensure-frame (buffer)
  "Return a child frame showing the now-playing BUFFER."
  (unless (frame-live-p audio-player-ui--frame)
    (setq audio-player-ui--frame-manual-position nil)
    (let* ((parent (selected-frame))
           (frame-resize-pixelwise t)
           (frame (make-frame
                   `((parent-frame . ,parent)
                     (minibuffer . nil)
                     (undecorated . t)
                     (skip-taskbar . t)
                     (no-other-frame . t)
                     (unsplittable . t)
                     (left-fringe . 0)
                     (right-fringe . 0)
                     (vertical-scroll-bars . nil)
                     (horizontal-scroll-bars . nil)
                     (scroll-bar-width . 0)
                     (scroll-bar-height . 0)
                     (right-divider-width . 0)
                     (bottom-divider-width . 0)
                     (menu-bar-lines . 0)
                     (tool-bar-lines . 0)
                     (tab-bar-lines . 0)
                     (internal-border-width . 0)
                     (child-frame-border-width . ,audio-player-ui--frame-border-width)
                     (no-focus-on-map . t)
                     (no-accept-focus . t)
                     (visibility . nil)))))
      (redirect-frame-focus frame parent)
      (setq audio-player-ui--frame frame)))
  (add-hook 'window-size-change-functions #'audio-player-ui--reposition-on-resize)
  (audio-player-ui--apply-child-frame-border-face audio-player-ui--frame)
  (let ((window (frame-root-window audio-player-ui--frame)))
    (when-let* ((parent (frame-parent audio-player-ui--frame)))
      (redirect-frame-focus audio-player-ui--frame parent))
    (set-window-buffer window buffer)
    (set-window-dedicated-p window t)
    (set-window-parameter window 'no-other-window t)
    (set-window-parameter window 'no-delete-other-windows t)
    (set-window-fringes window 0 0 nil t)
    (set-window-margins window 0 0)
    (set-window-scroll-bars window 0 nil 0 nil t))
  (audio-player-ui--fit-frame audio-player-ui--frame buffer)
  (audio-player-ui--position-frame audio-player-ui--frame)
  audio-player-ui--frame)

(defun audio-player-ui--fit-frame (frame buffer)
  "Size FRAME to fit BUFFER's image and text."
  (let ((frame-resize-pixelwise t))
    (if-let* ((image (audio-player-ui--buffer-image buffer)))
        (let* ((image-size (image-size image t frame))
               (width (car image-size)))
          (set-frame-width frame width nil t)
          (set-frame-height
           frame
           10
           nil
           t))
      (make-frame-visible frame)
      (fit-frame-to-buffer frame)
      (set-frame-width frame (max audio-player-ui-child-frame-width
                                  (+ 2 (frame-width frame)))))))

(defun audio-player-ui--format-duration (seconds)
  "Return SECONDS as a compact duration string."
  (if (not (numberp seconds))
      ""
    (setq seconds (floor seconds))
    (if (>= seconds 3600)
        (format "%d:%02d:%02d"
                (/ seconds 3600)
                (% (/ seconds 60) 60)
                (% seconds 60))
      (format "%d:%02d" (/ seconds 60) (% seconds 60)))))

(defun audio-player-ui--mdicon (name fallback)
  "Return Material Design icon NAME, or FALLBACK when unavailable."
  (if (and (require 'nerd-icons nil t)
           (fboundp 'nerd-icons-mdicon))
      (condition-case nil
          (funcall #'nerd-icons-mdicon name :height 1.0)
        (error fallback))
    fallback))

(defun audio-player-ui--playing-buffer ()
  "Return the now-playing buffer, creating it when needed."
  (let ((buffer (get-buffer-create audio-player-ui--buffer-name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'audio-player-ui--now-playing-mode)
        (audio-player-ui--now-playing-mode))
      (audio-player-ui--disable-ui-line-wrapping)
      (setq-local tab-line-format nil))
    buffer))

(defun audio-player-ui--position-frame (frame)
  "Position child FRAME at its manual or default position."
  (when-let* ((position (or audio-player-ui--frame-manual-position
                            (audio-player-ui--default-frame-position frame))))
    (setq position (audio-player-ui--constrain-frame-position frame position))
    (when audio-player-ui--frame-manual-position
      (setq audio-player-ui--frame-manual-position position))
    (set-frame-position frame (car position) (cdr position))))

(defun audio-player-ui--progress-bar (position duration width)
  "Return a Unicode progress bar for POSITION, DURATION, and WIDTH.
When DURATION is not known, return a fixed-width placeholder bar."
  (when (>= width audio-player-ui--progress-bar-min-width)
    (if (and (numberp duration)
             (> duration 0))
        (let* ((position (if (numberp position)
                             (min duration (max 0 position))
                           0))
               (ratio (/ (float position) duration))
               (filled (min width
                            (floor (* ratio width)))))
          (concat
           (make-string filled ?▰)
           (propertize (make-string (- width filled) ?▱) 'face 'shadow)))
      (propertize (make-string width ?▱) 'face 'shadow))))

(defun audio-player-ui--render-buffer ()
  "Render the now-playing buffer content using the VUI component tree.
Mounts the 'audio-player component inline and stores the resulting
component tree in `audio-player-ui--vui-component'."
  (with-current-buffer (get-buffer-create audio-player-ui--buffer-name)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (setq audio-player-ui--vui-component (vui-mount-inline (vui-component 'audio-player))))))

(defun audio-player-ui--reposition-on-resize (frame)
  "Re-pin the now-playing child frame when parent FRAME changes size."
  (if (frame-live-p audio-player-ui--frame)
      (when (eq frame (frame-parent audio-player-ui--frame))
        (audio-player-ui--position-frame audio-player-ui--frame))
    (remove-hook 'window-size-change-functions
                 #'audio-player-ui--reposition-on-resize)))

(defun audio-player-ui--show-child-frame (buffer &optional _focus)
  "Show now-playing BUFFER in a non-focusable child frame."
  (let ((selected-frame (selected-frame))
        (selected-window (selected-window))
        (frame (audio-player-ui--ensure-frame buffer)))
    (unwind-protect
        (progn
          (unless (frame-visible-p frame)
            (make-frame-visible frame))
          frame)
      (when (frame-live-p selected-frame)
        (select-frame selected-frame)
        (when (window-live-p selected-window)
          (select-window selected-window))))))

(defun audio-player-ui--vui-artist ()
  "Return a VUI component displaying the current track's artist."
  (when-let* ((playing (audio-player-current-playing)))
    (vui-box
        (vui-muted (or (oref playing artist) ""))
      :width audio-player-ui-child-frame-width
      :align :center)))

(defun audio-player-ui--vui-duration ()
  "Return a VUI component displaying the track duration."
  (when-let* ((playing (audio-player-current-playing)))
    (vui-text (audio-player-ui--format-duration (oref playing duration)))))

(defun audio-player-ui--vui-playing-controls ()
  "Return a VUI component with skip, seek, and play/pause buttons."
  (when-let* ((status (oref audio-player--instance status)))
    (vui-box
        (vui-hstack
         (audio-player-ui--vui-playing-controls-skip -1)
         (audio-player-ui--vui-playing-controls-seek -1)
         (audio-player-ui--vui-playing-play-pause)
         (audio-player-ui--vui-playing-controls-seek 1)
         (audio-player-ui--vui-playing-controls-skip 1))
      :width audio-player-ui-child-frame-width
      :align :center)))

(defun audio-player-ui--vui-playing-controls-seek (direction)
  "Return a VUI seek button for DIRECTION (-1 for backward, 1 for forward)."
  (let* ((mdicon-name (if (> direction 0) "nf-md-fast_forward" "nf-md-rewind"))
         (mdicon-name (if (member audio-player-seek-step '(5 10 15 30 60))
                          (concat mdicon-name "_" (format "%s" audio-player-seek-step))
                        mdicon-name)))
    (if (> direction 0)
        (vui-button (audio-player-ui--mdicon mdicon-name ">")
          :on-click (lambda () (audio-player-seek-forward)))
      (vui-button (audio-player-ui--mdicon mdicon-name "<")
        :on-click (lambda () (audio-player-seek-previous))))))

(defun audio-player-ui--vui-playing-controls-skip (direction)
  "Return a VUI skip button for DIRECTION (-1 for previous, 1 for next)."
  (if (> direction 0)
      (vui-button (audio-player-ui--mdicon "nf-md-skip_next" ">>")
        :on-click (lambda () (audio-player-next)))
    (vui-button (audio-player-ui--mdicon "nf-md-skip_previous" "<<")
      :on-click (lambda () (audio-player-previous)))))

(defun audio-player-ui--vui-playing-play-pause ()
  "Return a VUI button that toggles playback when clicked."
  (let* ((status (oref audio-player--instance status)))
    (vui-button (if (equal status 'paused)
                    (audio-player-ui--mdicon "nf-md-play" ">")
                  (audio-player-ui--mdicon "nf-md-pause" "||"))
      :on-click (lambda () (audio-player-toggle)))))

(defun audio-player-ui--vui-position ()
  "Return a VUI component displaying the current playback position."
  (when-let* ((playing (audio-player-current-playing)))
    (vui-text (audio-player-ui--format-duration (oref playing position)))))

(defun audio-player-ui--vui-process ()
  "Return a VUI component displaying position, progress bar, and duration."
  (when-let* ((playing (audio-player-current-playing))
              (position (oref playing position))
              (duration (oref playing duration)))
    (vui-box
        (vui-hstack
         (audio-player-ui--vui-position)
         (vui-text (audio-player-ui--progress-bar position duration audio-player-ui--progress-bar-max-width))
         (audio-player-ui--vui-duration))
      :width audio-player-ui-child-frame-width
      :align :center)))

(defun audio-player-ui--vui-title ()
  "Return a VUI component displaying the current track's title."
  (when-let* ((playing (audio-player-current-playing)))
    (vui-box (vui-heading-1 (oref playing title))
      :width audio-player-ui-child-frame-width
      :align :center)))

(defun audio-player-ui-delete-frame ()
  "Delete the now-playing child frame, if any."
  (interactive)
  (when (frame-live-p audio-player-ui--frame)
    (let ((parent (frame-parent audio-player-ui--frame)))
      (redirect-frame-focus audio-player-ui--frame nil)
      (delete-frame audio-player-ui--frame)
      (when (frame-live-p parent)
        (select-frame-set-input-focus parent))))
  (setq audio-player-ui--frame nil
        audio-player-ui--frame-manual-position nil)
  (remove-hook 'window-size-change-functions
               #'audio-player-ui--reposition-on-resize))

(defun audio-player-ui-refresh (&optional msg)
  "Re-render the now-playing UI by updating the VUI component.
This is called via `audio-player-update-hooks' when player state changes."
  (when audio-player-ui--vui-component
    (vui-update audio-player-ui--vui-component '())))

(defun audio-player-ui-show-frame (&optional focus)
  "Show the now-playing view using `audio-player-ui-display-style'.
FOCUS is accepted for compatibility; child frames stay non-focusable."
  (interactive)
  (let ((buffer (audio-player-ui--playing-buffer)))
    (audio-player-ui--render-buffer)
    (audio-player-ui--show-child-frame buffer focus)))

(define-derived-mode audio-player-ui--now-playing-mode special-mode "audio-player-ui-now"
  "Major mode for the audio-player-ui now-playing child frame."
  (audio-player-ui--disable-ui-line-wrapping)
  (setq-local mode-line-format nil)
  (setq-local cursor-type nil)
  (setq-local overflow-newline-into-fringe t)
  (setq-local fringe-indicator-alist
              (assq-delete-all
               'continuation
               (assq-delete-all 'truncation
                                (copy-tree fringe-indicator-alist))))
  (setq-local left-fringe-width 0)
  (setq-local right-fringe-width 0)
  (setq-local vertical-scroll-bar nil)
  (setq-local horizontal-scroll-bar nil)
  (setq-local indicate-buffer-boundaries nil)
  (setq-local indicate-empty-lines nil)
  (setq-local cursor-in-non-selected-windows nil)
  (line-number-mode -1)
  (display-line-numbers-mode -1))

(vui-defcomponent audio-player ()
    :render
    (vui-vstack
     (audio-player-ui--vui-title)
     (audio-player-ui--vui-artist)
     (audio-player-ui--vui-process)
     (audio-player-ui--vui-playing-controls)))

(add-to-list 'audio-player-update-hooks #'audio-player-ui-refresh)

(provide 'audio-player-ui)
;;; audio-player-ui.el ends here
