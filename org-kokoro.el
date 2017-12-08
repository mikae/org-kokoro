;; org-kokoro.el --- -*- lexical-binding: t -*-
;;
;; Author: Minae Yui <minae.yui.sain@gmail.com>
;; Version: 0.1
;; URL: 
;; Keywords:
;; Compatibility:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;; .
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
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

(require 'emacs-ef)
(require 'org)

;; Constants
(defconst org-kokoro-edit-src-buffer-name "org-kokoro: edit src"
  "Name of edit src buffers.")

(defconst org-kokoro-edit-src-aliases '(("dot" . graphviz-dot)))

;; Modes
(define-minor-mode org-kokoro-edit-src-mode
  "Minor mode for shadowing some keys."
  :init-value nil
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-x C-s")     #'ignore)
            (define-key map (kbd "C-x C-c")     #'ignore)
            (define-key map (kbd "C-x C-x C-s") #'org-kokoro-edit-src-apply)
            (define-key map (kbd "C-x C-x C-c") #'org-kokoro-edit-src-reject)

            map))

;; Goodies
(defun org-kokoro-edit-src-find-language (lang langs)
  "Find satisfying LANG in LANGS."
  (let ((--language-mode))
    (while (and (null --language-mode)
                langs)
      (when (string= lang
                     (caar langs))
        (setq --language-mode (intern (concat (symbol-name (cdar langs))
                                              "-mode"))))
      (setq langs (cdr langs)))))

(defun org-kokoro-edit-src-apply ()
  "Apply changes in edit src buffer"
  (interactive)
  (when (and (boundp '--org-kokoro-save-function)
             (local-variable-p '--org-kokoro-save-function)
             (functionp --org-kokoro-save-function))
    (funcall --org-kokoro-save-function)))

(defun org-kokoro-edit-src-reject ()
  "Apply changes in edit src buffer"
  (interactive)
  (when (string-match org-kokoro-edit-src-buffer-name (buffer-name))
    (kill-buffer (current-buffer))))

(defun org-kokoro-edit-src ()
  "Narrow to src block, excludes BEGIN_SRC and END_SRC."
  (interactive)
  (let ((case-fold-search t)
        (blockp (org-between-regexps-p "^[ \t]*#\\+begin_src.*"
                                       "^[ \t]*#\\+end_src.*")))
    (if blockp
        (let* ((--block-beg      (car blockp))
               (--block-end      (cdr blockp))
               (--beg-end            (save-excursion
                                       (goto-char --block-beg)
                                       (search-forward-regexp "\n")
                                       (point)))
               (--end-beg            (save-excursion
                                       (goto-char --block-end)
                                       (search-backward-regexp "\n")
                                       (point)))
               (--block-beg-line (buffer-substring --block-beg --beg-end))
               (--text           (buffer-substring --beg-end --end-beg))
               (--source-buffer  (current-buffer))
               (--buffer         (generate-new-buffer org-kokoro-edit-src-buffer-name))
               (--language       (nth 1 (split-string --block-beg-line)))
               (--language-mode  nil))

          ;; Check if the mode from src block is derived mode in `org-src-lang-modes'
          (setq --language-mode
                (org-kokoro-edit-src-find-lang --language
                                               org-src-lang-modes))

          ;; Check mode aliases
          (unless --language-mode
            (setq --language-mode
                  (org-kokoro-edit-src-find-lang --language
                                                 org-kokoro-edit-src-aliases)))

          ;; Check if the mode from src block is derived mode in `org-src-lang-modes'

          ;; Otherwise, try to add "-mode" to language from src block
          (unless --language-mode
            (setq --language-mode (intern (concat --language
                                                  "-mode"))))

          (with-current-buffer --buffer
            (switch-to-buffer --buffer)
            (insert --text)
            (when (symbol-function --language-mode)
              (funcall --language-mode))
            (org-kokoro-edit-src-mode +1)
            (set (make-local-variable '--org-kokoro-save-function)
                 (lambda ()
                   (let ((--new-text (buffer-substring (point-min) (point-max))))
                     (with-current-buffer --source-buffer
                       ;; Replace old src block with newer one
                       (kill-region --block-beg
                                    --block-end)
                       (goto-char --block-beg)
                       (insert --block-beg-line)
                       (insert --new-text)
                       (insert "\n#+END_SRC")

                       ;; Update src block' beg and end points
                       (forward-line -1)
                       (setq blockp (org-between-regexps-p "^[ \t]*#\\+begin_src.*"
                                                           "^[ \t]*#\\+end_src.*"))
                       (if blockp
                           (setq --block-beg (car blockp)
                                 --block-end (cdr blockp))
                         (user-error "Error replacing scr block"))))))))

      (user-error "Not in a src block"))))

(defun org-kokoro-create-answer-table (&optional question-count)
  "Create question-answer-correct?-correction table."
  (interactive "P")
  (let ((--question-count (or question-count 1)))
    (org-table-create (format "5x%d" (+ 2 (or question-count 1))))
    ;; header content
    (dolist (--elem '("" "Q" "A" "OK?" "Correction"))
      (org-cycle)
      (insert --elem))
    (org-cycle)
    ;; number of answer
    (dotimes (--i (1- --question-count))
      (insert "#")
      (org-cycle)
      (insert (format "%d)" (1+ --i)))
      (dotimes (_ 4)
        (org-cycle)))
    ;; last row
    (insert "#")
    (org-cycle)
    (insert (format "%d)" --question-count))
    (org-table-insert-hline)
    (forward-line 2)
    (dotimes (_ 4)
      (org-cycle))
    (insert "0%")
    (org-table-align)
    (forward-line)
    (let ((--format (format "#+TBLFM: @%d$4='(format \"%%.2f%%%%\" (* (let ((--s (concat  @2$4..@%d$4))) (/ (s-count-matches \"v\" --s) %.1f)) 100))"
                            (+ 2 --question-count)
                            (+ 1 --question-count)
                            (* --question-count 1.0))))
      (insert --format))))

;; Hide/toggle src blocks
(defun org-kokoro--src-blocks-set-visibility (&optional visible)
  "Hide src blocks.
If VISIBLE, then make all src blocks visible.
Otherwise, hide(default)."
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search t))
      (while (re-search-forward "#\\+BEGIN_SRC" nil t)
        (when (org-at-block-p)
          (org-hide-block-toggle t)
          (when visible
            (org-hide-block-toggle nil)))))))

(defun org-kokoro-src-block-hide-all ()
  "Hide all src blocks."
  (interactive)
  (org-kokoro--src-blocks-set-visibility nil))

(defun org-kokoro-src-block-show-all ()
  "Show all src blocks."
  (interactive)
  (org-kokoro--src-blocks-set-visibility t))

;; org-at-timestamp-p
(defun org-kokoro-at-timestamp-p (&optional extended)
  "Returns t, if point at timestamp."
  (let ((regexp (if extended org-ts-regexp3 org-ts-regexp2)))
    (org-at-regexp-p regexp)))

(provide 'org-kokoro)
;;; org-kokoro.el ends here
