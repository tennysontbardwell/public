;;; vline.el --- show vertical line (column highlighting) mode.

;; Copyright (C) 2002, 2008-2012 by Taiki SUGAWARA <buzz.taiki@gmail.com>

;; Author: Taiki SUGAWARA <buzz.taiki@gmail.com>
;; Maintainer: Taiki SUGAWARA <buzz.taiki@gmail.com>
;; Keywords: faces, editing, emulating
;; Version: 1.11
;; Time-stamp: <2012-01-08 12:40:18 UTC taiki>
;; URL: http://www.emacswiki.org/cgi-bin/wiki/vline.el
;; URL: http://bitbucket.org/buzztaiki/elisp/src/tip/vline.el

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; put followings your .emacs
;;   (require 'vline)
;;
;; if you display a vertical line, type M-x vline-mode.  `vline-mode' doesn't
;; effect other buffers, because it is a buffer local minor mode.  if you hide
;; a vertical line, type M-x vline-mode again.
;;
;; if you display a vertical line in all buffers, type M-x vline-global-mode.
;;
;; `vline-style' provides a display style of vertical line.  see
;; `vline-style' docstring.
;;
;; if you don't want to visual line highlighting (ex.  for performance
;; issue), please to set `vline-visual' to nil.
;;
;; if you don't want to use timer (ex.  you want to highlight column
;; during moving cursors), please to set `vline-use-timer' to nil.

;;; Change Log:

;; 2012-01-08 taiki
;; fix for the Lint warnings.

;; 2010-02-02 taiki
;; improve performance.

;; 2009-08-26 taiki
;; support org-mode, outline-mode

;; 2009-08-18 taiki
;; add autoload cookies.

;; 2009-08-18 taiki
;; fix last line highlighting probrem.

;; 2009-08-18 taiki
;; support visual line highlighting.
;; - Added face vline-visual.
;; - Added defcustom vline-visual-face.
;; - Added defcustom vline-visual.
;;
;; 2009-08-17 taiki
;; fix continuas line problem.
;; - Don't display vline when cursor into fringe
;; - Don't expand eol more than window width.
;;
;; 2008-10-22 taiki
;; fix coding-system problem.
;; - Added vline-multiwidth-space-list
;; - Use ucs code-point for japanese fullwidth space.
;;
;; 2008-01-22 taiki
;; applied patch from Lennart Borgman
;; - Added :group 'vline
;; - Added defcustom vline-current-window-only
;; - Added header items to simplify for users

;;; TODO:
;; - track window-scroll-functions, window-size-change-functions.
;; - consider other minor modes (using {after,before}-string overlay).
;; - don't use {post,after}-command-hook for performance??

;;; Code:

(defvar vline-overlay-table-size 200)
(defvar vline-overlay-table (make-vector vline-overlay-table-size nil))
(defvar vline-line-char ?|)
(defvar vline-multiwidth-space-list
  (list
   ?\t
   (decode-char 'ucs #x3000)		; japanese fullwidth space
   ))
(defvar vline-timer nil)

(defcustom vline-style 'face
  "This variable holds vertical line display style.
Available values are followings:
`face'      : use face.
`compose'   : use composit char.
`mixed'     : use face and composit char."
  :type '(radio
          (const face)
          (const compose)
          (const mixed))
  :group 'vline)


(defface vline
  '((t (:background "light steel blue")))
  "A default face for vertical line highlighting."
  :group 'vline)

(defface vline-visual
  '((t (:background "gray90")))
  "A default face for vertical line highlighting in visual lines."
  :group 'vline)

(defcustom vline-face 'vline
  "A face for vertical line highlighting."
  :type 'face
  :group 'vline)

(defcustom vline-visual-face 'vline-visual
  "A face for vertical line highlighting in visual lines."
  :type 'face
  :group 'vline)

(defcustom vline-current-window-only nil
  "If non-nil then highlight column in current window only.
If the buffer is shown in several windows then highlight column only
in the currently selected window."
  :type 'boolean
  :group 'vline)

(defcustom vline-visual t
  "If non-nil then highlight column in visual lines.
If you specified `force' then use force visual line highlighting even
if `truncate-lines' is non-nil."
  :type '(radio
          (const nil)
          (const t)
          (const force))
  :group 'vline)

(defcustom vline-use-timer t
  "If non-nil, use idle timer instead of (post|after)-command-hook."
  :type 'boolean
  :group 'vline)

(defcustom vline-idle-time 0.02
  "Idle time for highlighting column."
  :type 'number
  :group 'vline)

;;;###autoload
(define-minor-mode vline-mode
  "Display vertical line mode."
  :global nil
  :lighter " VL"
  :group 'vline
  (if vline-mode
      (progn
        (add-hook 'pre-command-hook 'vline-pre-command-hook nil t)
        (if vline-use-timer
            (vline-set-timer)
          (add-hook 'post-command-hook 'vline-post-command-hook nil t)))
    (vline-cancel-timer)
    (vline-clear)
    (remove-hook 'pre-command-hook 'vline-pre-command-hook t)
    (remove-hook 'post-command-hook 'vline-post-command-hook t)))

;;;###autoload
(define-global-minor-mode vline-global-mode
  vline-mode
  (lambda ()
    (unless (minibufferp)
      (vline-mode 1)))
  :group 'vline)

(defun vline-pre-command-hook ()
  (when (and vline-mode (not (minibufferp)))
    (vline-clear)))

(defun vline-post-command-hook ()
  (when (and vline-mode (not (minibufferp)))
    (vline-show)))

(defun vline-set-timer ()
  (setq vline-timer
        (run-with-idle-timer
         vline-idle-time t 'vline-timer-callback)))

(defun vline-cancel-timer ()
  (when (timerp vline-timer)
    (cancel-timer vline-timer)))

(defun vline-timer-callback ()
  (when (and vline-mode (not (minibufferp)))
    (vline-show)))

(defun vline-clear ()
  (mapcar (lambda (ovr)
            (and ovr (delete-overlay ovr)))
          vline-overlay-table))

(defsubst vline-into-fringe-p ()
  (eq (nth 1 (posn-at-point)) 'right-fringe))

(defsubst vline-visual-p ()
  (or (eq vline-visual 'force)
      (and (not truncate-lines)
           vline-visual)))

(defsubst vline-current-column ()
  (if (or (not (vline-visual-p))
          ;; margin for full-width char
          (< (1+ (current-column)) (window-width)))
      (current-column)
    ;; hmm.. posn-at-point is not consider tab width.
    (- (current-column)
       (save-excursion
         (vertical-motion 0)
         (current-column)))))

(defsubst vline-move-to-column (col &optional bol-p)
  (if (or (not (vline-visual-p))
          ;; margin for full-width char
          (< (1+ (current-column)) (window-width)))
      (move-to-column col)
    (unless bol-p
      (vertical-motion 0))
    (let ((bol-col (current-column)))
      (- (move-to-column (+ bol-col col))
         bol-col))))

(defsubst vline-invisible-p (pos)
  (let ((inv (get-char-property pos 'invisible)))
    (and inv
         (or (eq buffer-invisibility-spec t)
             (memq inv buffer-invisibility-spec)
             (assq inv buffer-invisibility-spec)))))

(defsubst vline-forward (n)
  (unless (memq n '(-1 0 1))
    (error "n(%s) must be 0 or 1" n))
  (if (not (vline-visual-p))
      (progn
        (forward-line n)
        ;; take care of org-mode, outline-mode
        (when (and (not (bobp))
                   (vline-invisible-p (1- (point))))
          (goto-char (1- (point))))
        (when (vline-invisible-p (point))
          (if (< n 0)
              (while (and (not (bobp)) (vline-invisible-p (point)))
                (goto-char (previous-char-property-change (point))))
            (while (and (not (bobp)) (vline-invisible-p (point)))
              (goto-char (next-char-property-change (point))))
            (forward-line 1))))
    (vertical-motion n)))

(defun vline-face (visual-p)
  (if visual-p
      vline-visual-face
    vline-face))

(defun vline-show (&optional point)
  (vline-clear)
  (save-window-excursion
    (save-excursion
      (if point
          (goto-char point)
        (setq point (point)))
      (let* ((column (vline-current-column))
             (lcolumn (current-column))
             (i 0)
             (compose-p (memq vline-style '(compose mixed)))
             (face-p (memq vline-style '(face mixed)))
             (line-char (if compose-p vline-line-char ? ))
             (line-str (make-string 1 line-char))
             (visual-line-str line-str)
             (in-fringe-p (vline-into-fringe-p)))
        (when face-p
          (setq line-str (propertize line-str 'face (vline-face nil)))
          (setq visual-line-str (propertize visual-line-str 'face (vline-face t))))
        (goto-char (window-end nil t))
        (vline-forward 0)
        (while (and (not in-fringe-p)
                    (< i (window-height))
                    (< i (length vline-overlay-table))
                    (not (bobp)))
          (let ((cur-column (vline-move-to-column column t))
                (cur-lcolumn (current-column)))
            ;; non-cursor line only (workaround of eol probrem.
            (unless (= (point) point)
              ;; if column over the cursor's column (when tab or wide char is appered.
              (when (> cur-column column)
                (let ((lcol (current-column)))
                  (backward-char)
                  (setq cur-column (- cur-column (- lcol (current-column))))))
              (let* ((ovr (aref vline-overlay-table i))
                     (visual-p (or (< lcolumn (current-column))
                                   (> lcolumn (+ (current-column)
                                                 (- column cur-column)))))
                     ;; consider a newline, tab and wide char.
                     (str (concat (make-string (- column cur-column) ? )
                                  (if visual-p visual-line-str line-str)))
                     (char (char-after)))
                ;; create overlay if not found.
                (unless ovr
                  (setq ovr (make-overlay 0 0))
                  (overlay-put ovr 'rear-nonsticky t)
                  (aset vline-overlay-table i ovr))

                ;; initialize overlay.
                (overlay-put ovr 'face nil)
                (overlay-put ovr 'before-string nil)
                (overlay-put ovr 'after-string nil)
                (overlay-put ovr 'invisible nil)
                (overlay-put ovr 'window
                             (if vline-current-window-only
                                 (selected-window)
                               nil))

                (cond
                 ;; multiwidth space
                 ((memq char vline-multiwidth-space-list)
                  (setq str
                        (concat str
                                (make-string (- (save-excursion (forward-char)
                                                                (current-column))
                                                (current-column)
                                                (string-width str))
                                             ? )))
                  (move-overlay ovr (point) (1+ (point)))
                  (overlay-put ovr 'invisible t)
                  (overlay-put ovr 'after-string str))
                 ;; eol
                 ((eolp)
                  (move-overlay ovr (point) (point))
                  (overlay-put ovr 'after-string str)
                  ;; don't expand eol more than window width
                  (when (and (not truncate-lines)
                             (>= (1+ column) (window-width))
                             (>= column (vline-current-column))
                             (not (vline-into-fringe-p)))
                    (delete-overlay ovr)))
                 (t
                  (cond
                   (compose-p
                    (let (str)
                      (when char
                        (setq str (compose-chars
                                   char
                                   (cond ((= (char-width char) 1)
                                          '(tc . tc))
                                         ((= cur-column column)
                                          '(tc . tr))
                                         (t
                                          '(tc . tl)))
                                   line-char))
                        (when face-p
                          (setq str (propertize str 'face (vline-face visual-p))))
                        (move-overlay ovr (point) (1+ (point)))
                        (overlay-put ovr 'invisible t)
                        (overlay-put ovr 'after-string str))))
                   (face-p
                    (move-overlay ovr (point) (1+ (point)))
                    (overlay-put ovr 'face (vline-face visual-p))))))))
            (setq i (1+ i))
            (vline-forward -1)))))))

(provide 'vline)

;;; Local Variables:
;;; time-stamp-format: "%:y-%02m-%02d %02H:%02M:%02S %Z %u"
;;; time-stamp-line-limit: 16
;;; End:

;;; vline.el ends here
;;; col-highlight.el --- Highlight the current column.
;;
;; Filename: col-highlight.el
;; Description: Highlight the current column.
;; Author: Drew Adams
;; Maintainer: Drew Adams (concat "drew.adams" "@" "oracle" ".com")
;; Copyright (C) 2006-2018, Drew Adams, all rights reserved.
;; Created: Fri Sep 08 11:06:35 2006
;; Version: 0
;; Package-Requires: ((vline "0"))
;; Last-Updated: Mon Jan  1 10:17:48 2018 (-0800)
;;           By: dradams
;;     Update #: 446
;; URL: https://www.emacswiki.org/emacs/download/col-highlight.el
;; Doc URL: https://emacswiki.org/emacs/HighlightCurrentColumn
;; Keywords: faces, frames, emulation, highlight, cursor, accessibility
;; Compatibility: GNU Emacs: 22.x, 23.x, 24.x, 25.x, 26.x
;;
;; Features that might be required by this library:
;;
;;   `vline'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;  This library highlights the current column.  When you move the
;;  cursor, the highlighting follows (tracks the cursor), as long as
;;  the highlighting stays on.
;;
;;  Command `column-highlight-mode' toggles this highlighting on and
;;  off.
;;
;;  If you use `column-highlight-mode' twice in succession (I bind it
;;  to `C-+'), you can flash the highlighting to show you the current
;;  column temporarily.  An alternative way to flash-highlight is to
;;  use command `flash-column-highlight' (once).  It shows the
;;  highlighting for just a second or two (see option
;;  `col-highlight-period').
;;
;;  You can also have current-column highlighting come on
;;  automatically, when Emacs is idle.  Command
;;  `toggle-highlight-column-when-idle' toggles this mode.  Command
;;  `col-highlight-set-interval' changes the number of idle seconds to
;;  wait before highlighting.
;;
;;  You can use option `col-highlight-overlay-priority' to make the
;;  vline (i.e., column) highlighting appear on top of other overlay
;;  highlighting that might exist.
;;
;;  You can use option `col-highlight-show-only' to restrict
;;  current-column highlighting to a section of text of a particular
;;  kind: paragaph, sentence, page, defun, etc.
;;
;;
;;  To use this file, you must also have library `vline.el'.
;;  Put this in your Emacs init file (~/.emacs):
;;
;;    (require 'col-highlight) ; Load this file (and `vline')
;;
;;  If you want to turn on continual current-column highlighting by
;;  default, then add this to your init file:
;;
;;    (column-highlight-mode 1)
;;
;;  If you want to turn on automatic idle highlighting of the current
;;  column, then add this to your init file:
;;
;;    (toggle-highlight-column-when-idle 1)
;;
;;  If you want to use a different wait interval, before idle
;;  highlighting begins, then set it in your init file using
;;  `col-highlight-set-interval':
;;
;;    (col-highlight-set-interval 6) ; Wait 6 idle secs.
;;
;;  Note that `column-highlight-mode' is intentionally a global minor
;;  mode.  If you want a local minor mode, so that highlighting
;;  affects only a particular buffer, you can use `vline-mode' (in
;;  `vline.el').
;;
;;
;;  See also:
;;
;;  * Library `hl-line+.el', which offers the same functionality, but
;;    for the current line instead of the current column.
;;
;;  * Library `crosshairs.el', which combines the features of
;;    `col-highlight.el' and `hl-line+.el', providing a crosshair
;;    highlighting effect.  It requires `col-highlight.el' and
;;    `hl-line+.el'.
;;
;;  * Library `cursor-chg.el' or library `oneonone.el', to change the
;;    cursor type when Emacs is idle.
;;
;;  User options defined here:
;;
;;    `col-highlight-period', `column-highlight-mode',
;;    `col-highlight-overlay-priority', `col-highlight-show-only',
;;    `col-highlight-vline-face-flag'.
;;
;;  Faces defined here:
;;
;;    `col-highlight'.
;;
;;  Commands defined here:
;;
;;    `col-highlight-flash', `col-highlight-set-interval',
;;    `col-highlight-toggle-when-idle', `column-highlight-mode',
;;    `flash-column-highlight', `toggle-highlight-column-when-idle'.
;;
;;  Non-interactive functions defined here:
;;
;;    `col-highlight-highlight', `col-highlight-unhighlight'.
;;
;;  Internal variables defined here:
;;
;;    `col-highlight-face', `col-highlight-idle-interval',
;;    `col-highlight-idle-timer', `col-highlight-when-idle-p'.
;;
;;
;;  ***** NOTE: The following function defined in `vline.el' has
;;              been REDEFINED HERE:
;;
;;    `vline-show' - Respect options `col-highlight-overlay-priority'
;;                   and `col-highlight-show-only'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;; 2017/05/10 dadams
;;     vline-show: Wrap arg to make-string with abs.  Not a fix, but bypasses error from not
;;                 handling SPC char with display property value of (space :align-to N).
;; 2013/08/08 dadams
;;     Added: col-highlight-show-only, redefinition of vline-show.
;;     Removed defadvice of vline-show (replaced by redefinition).
;; 2012/12/25 dadams
;;     Added Package-Requires.
;; 2012/05/18 dadams
;;     Added: col-highlight-overlay-priority, defadvice of vline-show.
;; 2011/01/03 dadams
;;     Added autoload cookies for defgroup, defcustom, defface, and commands.
;; 2008/09/03 dadams
;;     col-highlight-highlight: Bind vline-current-window-only to t.
;; 2008/08/08 dadams
;;     col-highlight-(un)highlight: Added optional arg.
;; 2008/01/21 dadams
;;     Use vline.el instead of column-marker.el.
;;     Added: group column-highlight, option col-highlight-vline-face-flag.
;;     col-highlight-toggle-when-idle: col-highlight-unhighlight when turn off.
;;     col-highlight-flash: Use col-highlight-highlight, not mode.
;;     col-highlight-(un)highlight: Respect col-highlight-vline-face-flag.
;;                                  Don't highlight minibuffer.
;;     Renamed: face col-highlight-face to col-highlight.
;;     Removed semi-support for Emacs 20.
;; 2006/09/08 dadams
;;     Created.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(require 'vline)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;###autoload
(defgroup column-highlight nil
  "Highlight the current column."
  :prefix "col-highlight-"
  :group 'editing :group 'cursor :group 'hl-line :group 'frames
  :link `(url-link :tag "Send Bug Report"
                   ,(concat "mailto:" "drew.adams" "@" "oracle" ".com?subject=\
col-highlight.el bug: \
&body=Describe bug here, starting with `emacs -q'.  \
Don't forget to mention your Emacs and library versions."))
  :link '(url-link :tag "Other Libraries by Drew"
                   "https://www.emacswiki.org/emacs/DrewsElispLibraries")
  :link '(url-link :tag "Download"
                   "https://www.emacswiki.org/emacs/download/col-highlight.el"))

;;;###autoload
(defcustom col-highlight-show-only nil
  "Non-nil means `column-highlight-mode' affects only a section of text.
This affects `vline-mode' also.

The non-nil value determines the type of text section: paragraph,
sentence, defun, page...

The actual non-nil value is a forward movement command for the given
section type, e.g., `forward-paragraph', `end-of-defun'."
  :type '(choice
          (const    :tag "All text"  nil)
          (const    :tag "Paragraph" forward-paragraph)
          (const    :tag "Sentence"  forward-sentence)
          (const    :tag "Page"      forward-page)
          (const    :tag "Defun"     end-of-defun)
          (function :tag "Forward-thing function" :value forward-paragraph))
  :group 'column-highlight)

;;;###autoload
(defcustom col-highlight-vline-face-flag t
  "*Non-nil means `column-highlight-mode' uses `col-highlight-face'.
nil means that it uses `vline-face'."
  :type 'boolean :group 'column-highlight)

;;;###autoload
(defcustom col-highlight-period 1
  "*Number of seconds to highlight the current column."
  :type 'integer :group 'column-highlight)

;;;###autoload
(defcustom col-highlight-overlay-priority 300
  "*Priority to use for overlays in `vline-overlay-table'.
A higher priority can make the vline highlighting appear on top of
other overlays that might exist."
  :type '(choice
          (const   :tag "No priority (default priority)"  nil)
          (integer :tag "Priority"  300))
  :group 'column-highlight)

;;;###autoload
(defface col-highlight '((t (:background "SlateGray3")))
  "*Face for current-column highlighting by `column-highlight-mode'.
Not used if `col-highlight-vline-face-flag' is nil."
  :group 'column-highlight :group 'faces)

(defvar col-highlight-face 'col-highlight
  "Face used for highlighting current column.
Do NOT change this.")

(defvar col-highlight-idle-interval 5
  "Number of seconds to wait before highlighting current column.
Do NOT change this yourself to change the wait period; instead, use
`\\[col-highlight-set-interval]'.")

(defvar col-highlight-when-idle-p nil
  "Non-nil means highlight the current column whenever Emacs is idle.
Do NOT change this yourself; instead, use
`\\[toggle-highlight-column-when-idle]'.")

(defvar col-highlight-idle-timer
  (progn                              ; Cancel to prevent duplication.
    (when (boundp 'col-highlight-idle-timer)
      (cancel-timer col-highlight-idle-timer))
    (run-with-idle-timer col-highlight-idle-interval t 'col-highlight-highlight))
  "Timer used to highlight current column whenever Emacs is idle.")

;; Turn it off, by default.
;; You must use `toggle-highlight-column-when-idle' to turn it on.
(cancel-timer col-highlight-idle-timer)



;;  REPLACE ORIGINAL `vline-show' defined in `vline.el'.
;;
;;  1. Respect options `col-highlight-overlay-priority' and `col-highlight-show-only'.
;;  2. Tolerate SPC char with `display' property value (space :align-to N).
;;
(defun vline-show (&optional point)
  (vline-clear)
  (save-window-excursion
    (save-excursion
      (if point
          (goto-char point)
        (setq point  (point)))
      (let* ((column           (vline-current-column))
             (lcolumn          (current-column))
             (i                0)
             (compose-p        (memq vline-style '(compose mixed)))
             (face-p           (memq vline-style '(face mixed)))
             (line-char        (if compose-p vline-line-char ?\   ))
             (line-str         (make-string 1 line-char))
             (visual-line-str  line-str)
             (in-fringe-p      (vline-into-fringe-p))
             (only-beg         (and col-highlight-show-only
                                    (condition-case nil
                                        (save-excursion
                                          (funcall col-highlight-show-only -1)
                                          (point))
                                      (error nil))))
             (only-end         (and col-highlight-show-only
                                    (condition-case nil
                                        (save-excursion
                                          (funcall col-highlight-show-only 1)
                                          (point))
                                      (error nil)))))
        (when face-p
          (setq line-str (propertize line-str 'face (vline-face nil)))
          (setq visual-line-str  (propertize visual-line-str 'face (vline-face t))))
        (goto-char (window-end nil t))
        (vline-forward 0)
        (while (and (not (bobp))
                    (not in-fringe-p)
                    (< i (window-height))
                    (< i (length vline-overlay-table)))
          (let ((cur-column   (vline-move-to-column column t))
                (cur-lcolumn  (current-column)))
            (unless (or (= (point) point) ; Non-cursor line only (eol workaround).
                        (and only-beg  only-end  (or (<= (point) only-beg)
                                                     (>= (point) only-end))))
              (when (> cur-column column)
                (let ((lcol  (current-column)))
                  (backward-char)
                  (setq cur-column  (- cur-column (- lcol (current-column))))))
              (let* ((ovr       (aref vline-overlay-table i))
                     (visual-p  (or (< lcolumn (current-column))
                                    (> lcolumn (+ (current-column)
                                                  (- column cur-column)))))
                     ;; Consider a newline, tab and wide char.
                     (str       (concat (make-string (abs (- column cur-column)) ?\  )
                                        (if visual-p visual-line-str line-str)))
                     (char      (char-after)))
                (unless ovr
                  (setq ovr  (make-overlay 0 0))
                  (overlay-put ovr 'rear-nonsticky t)
                  (aset vline-overlay-table i ovr))
                (overlay-put ovr 'face nil)
                (overlay-put ovr 'before-string nil)
                (overlay-put ovr 'after-string nil)
                (overlay-put ovr 'invisible nil)
                (overlay-put ovr 'window (and vline-current-window-only  (selected-window)))
                (cond ((memq char vline-multiwidth-space-list) ; Multiwidth space
                       (setq str  (concat str (make-string (- (save-excursion (forward-char)
                                                                              (current-column))
                                                              (current-column)
                                                              (string-width str))
                                                           ?\  )))
                       (move-overlay ovr (point) (1+ (point)))
                       (overlay-put ovr 'invisible t)
                       (overlay-put ovr 'after-string str))
                      ((eolp)
                       (move-overlay ovr (point) (point))
                       (overlay-put ovr 'after-string str)
                       (when (and (not truncate-lines) ; Do not expand more than window width.
                                  (>= (1+ column) (window-width))
                                  (>= column (vline-current-column))
                                  (not (vline-into-fringe-p)))
                         (delete-overlay ovr)))
                      (t
                       (cond (compose-p
                              (let (str)
                                (when char
                                  (setq str  (compose-chars char
                                                            (cond ((= (char-width char) 1)
                                                                   '(tc . tc))
                                                                  ((= cur-column column)
                                                                   '(tc . tr))
                                                                  (t
                                                                   '(tc . tl)))
                                                            line-char))
                                  (when face-p
                                    (setq str  (propertize str 'face (vline-face visual-p))))
                                  (move-overlay ovr (point) (1+ (point)))
                                  (overlay-put ovr 'invisible t)
                                  (overlay-put ovr 'after-string str))))
                             (face-p
                              (move-overlay ovr (point) (1+ (point)))
                              (overlay-put ovr 'face (vline-face visual-p))))))))
            (setq i  (1+ i))
            (vline-forward -1))))))
  (mapc (lambda (ov) (when (overlayp ov) ; Set overlay priority to `col-highlight-overlay-priority'.
                       (overlay-put ov 'priority col-highlight-overlay-priority)))
        vline-overlay-table))

;;;###autoload
(define-minor-mode column-highlight-mode
  "Toggle highlighting the current column.
With ARG, turn column highlighting on if and only if ARG is positive.

Column-Highlight mode uses the functions
`col-highlight-unhighlight' and `col-highlight-highlight'
on `pre-command-hook' and `post-command-hook'."
  :init-value nil :global t :group 'column-highlight
  :link `(url-link :tag "Send Bug Report"
                   ,(concat "mailto:" "drew.adams" "@" "oracle" ".com?subject=\
col-highlight.el bug: \
&body=Describe bug here, starting with `emacs -q'.  \
Don't forget to mention your Emacs and library versions."))
  :link '(url-link :tag "Other Libraries by Drew"
                   "https://www.emacswiki.org/emacs/DrewsElispLibraries")
  :link '(url-link :tag
                   "Download" "https://www.emacswiki.org/emacs/download/col-highlight.el")
  :link '(url-link :tag "Description"
                   "https://www.emacswiki.org/emacs/ChangingCursorDynamically")
  :link '(emacs-commentary-link :tag "Commentary" "col-highlight")
  (cond (column-highlight-mode
         (add-hook 'pre-command-hook #'col-highlight-unhighlight)
         (add-hook 'post-command-hook #'col-highlight-highlight))
        (t
         (col-highlight-unhighlight)
         (remove-hook 'pre-command-hook #'col-highlight-unhighlight)
         (remove-hook 'post-command-hook #'col-highlight-highlight))))

;;;###autoload
(defalias 'toggle-highlight-column-when-idle 'col-highlight-toggle-when-idle)
;;;###autoload
(defun col-highlight-toggle-when-idle (&optional arg)
  "Turn on or off highlighting the current column when Emacs is idle.
With prefix argument, turn on if ARG > 0; else turn off."
  (interactive "P")
  (setq col-highlight-when-idle-p  (if arg
                                       (> (prefix-numeric-value arg) 0)
                                     (not col-highlight-when-idle-p)))
  (cond (col-highlight-when-idle-p
         (timer-activate-when-idle col-highlight-idle-timer)
         (add-hook 'pre-command-hook #'col-highlight-unhighlight)
         (message "Turned ON highlighting current column when Emacs is idle."))
        (t
         (cancel-timer col-highlight-idle-timer)
         (col-highlight-unhighlight)
         (remove-hook 'pre-command-hook #'col-highlight-unhighlight)
         (message "Turned OFF highlighting current column when Emacs is idle."))))

;;;###autoload
(defun col-highlight-set-interval (n)
  "Set the delay before highlighting current column when Emacs is idle.
Whenever Emacs has been idle for N seconds, the current column is
highlighted using the face that is the value of variable
`col-highlight-face'.

To turn on or off automatically highlighting the current column
when Emacs is idle, use `\\[toggle-highlight-column-when-idle]."
  (interactive
   "nSeconds to idle, before highlighting current column: ")
  (timer-set-idle-time col-highlight-idle-timer
                       (setq col-highlight-idle-interval  n)
                       t))

;;;###autoload
(defalias 'flash-column-highlight 'col-highlight-flash)
;;;###autoload
(defun col-highlight-flash (&optional arg)
  "Highlight the current column for `col-highlight-period' seconds.
With a prefix ARG, highlight for that many seconds."
  (interactive)
  (col-highlight-highlight)
  (let ((column-period  col-highlight-period))
    (when current-prefix-arg
      (setq column-period  (prefix-numeric-value current-prefix-arg)))
    (run-at-time column-period nil #'col-highlight-unhighlight)))

(defun col-highlight-highlight (&optional minibuffer-also-p)
  "Highlight current column.
This has no effect in the minibuffer, unless optional arg
MINIBUFFER-ALSO-P is non-nil."
  (unless (and (minibufferp)  (not minibuffer-also-p))
    (let ((vline-current-window-only  t))
      (if col-highlight-vline-face-flag
          (let ((vline-style  'face)
                (vline-face   col-highlight-face))
            (vline-show))
        (vline-show)))))

(defun col-highlight-unhighlight (&optional minibuffer-also-p)
  "Turn off highlighting of current column.
This has no effect in the minibuffer, unless optional arg
MINIBUFFER-ALSO-P is non-nil."
  (unless (and (minibufferp)  (not minibuffer-also-p))
    (if col-highlight-vline-face-flag
        (let ((vline-style  'face)
              (vline-face   col-highlight-face))
          (vline-clear))
      (vline-clear))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'col-highlight)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; col-highlight.el ends here
