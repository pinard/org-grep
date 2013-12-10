;;; org-grep.el --- Kind of M-x rgrep adapted for Org mode.

;; Copyright © 2013 Progiciels Bourbeau-Pinard inc.

;; Author: François Pinard <pinard@iro.umontreal.ca>
;; Maintainer: François Pinard <pinard@iro.umontreal.ca>
;; URL: https://github.com/pinard/org-grep

;; This is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software Foundation,
;; Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.

;;; Commentary:

;; This tool allows for grepping files in a set of Org directories,
;; formatting the results as a separate Org buffer.  This buffer is
;; assorted with a few specific navigation commands so it works a bit
;; like M-x rgrep.  See https://github.com/pinard/org-grep.

;;; Code:

(defvar org-grep-directories nil
  "Directories to search, or ORG-DIRECTORY if nil.")

(defvar org-grep-extensions '(".org")
  "List of extensions for searchable files.")

(defvar org-grep-buffer-name "*Org grep*")
(defvar org-grep-buffer-name-copy-format "*Org grep %s*")
(defvar org-grep-hit-regexp "^- ")
(defvar org-grep-user-regexp nil)

(defun org-grep (regexp)
  (interactive
   (list (if (use-region-p)
             (buffer-substring (region-beginning) (region-end))
           (read-string "Org grep? "))))
  (when (string-equal regexp "")
    (error "Nothing to find!"))
  ;; Launch grep according to REGEXP.
  (pop-to-buffer org-grep-buffer-name)
  (toggle-read-only 0)
  (erase-buffer)
  (save-some-buffers t)
  (shell-command
   (concat "find "
           (if org-grep-directories
               (org-grep-join org-grep-directories " ")
             org-directory)
           (and org-grep-extensions
                (concat " -regex '.*\\.\\("
                        (org-grep-join org-grep-extensions "\\|")
                        "\\)'"))
           " -print0 | xargs -0 grep -i -n "
           (shell-quote-argument regexp))
   t)
  ;; For legibility, remove extra whitespace.
  (goto-char (point-min))
  (while (re-search-forward "[ \f\t\b][ \f\t\b]+" nil t)
    (replace-match " "))
  ;; Prefix found lines with sorting keys, a NUL, and clickable information.
  (goto-char (point-min))
  (while (re-search-forward "^\\([^:]+\\):\\([0-9]+\\):" nil t)
    (let* ((file (match-string 1))
           (directory (file-name-directory file))
           (base (file-name-sans-extension (file-name-nondirectory file))))
      (replace-match (concat (downcase base) " "
                             (format "%5d" (string-to-int (match-string 2)))
                             "\0- [[file:\\1::\\2][" base ":]]\\2 :: "))
      ;; Moderately try to resolve relative links.
      (while (re-search-forward "\\[\\[\\([^]\n:]+:\\)?\\([^]]+\\)"
                                (line-end-position) t)
        (cond ((not (match-string 1))
               (replace-match (concat "[[file:" file "::\\2")))
              ((string-equal (match-string 1) "file:")
               (unless (memq (aref (match-string 2) 0) '(?~ ?/))
                 (replace-match
                  (concat "[[file:" directory (match-string 2)))))))))
  ;; Sort lines, remove sorting keys and the NUL.
  (sort-lines nil (point-min) (point-max))
  (let ((counter 0))
    (goto-char (point-min))
    (while (re-search-forward "^[^\0]+\0" nil t)
      (replace-match "")
      (forward-line 1)
      (setq counter (1+ counter)))
    (when (zerop counter)
      (kill-buffer)
      (error "None found!"))
    (goto-char (point-min))
    (insert (format "* Grep found %d occurrences of %s\n\n" counter regexp))
    ;; Activate Org mode on the results.
    (org-mode)
    (goto-char (point-min))
    (org-show-subtree)
    ;; Highlight the search string.
    (when org-grep-user-regexp
      (hi-lock-unface-buffer (org-grep-hi-lock-helper org-grep-user-regexp)))
    (hi-lock-face-buffer (org-grep-hi-lock-helper regexp) 'hi-yellow)
    (setq org-grep-user-regexp regexp)
    ;; Add special commands to the keymap.
    (use-local-map (copy-keymap (current-local-map)))
    (toggle-read-only 1)
    (local-set-key "\C-c\C-c" 'org-grep-current-jump)
    (local-set-key "\C-x`" 'org-grep-next-jump)
    (local-set-key "." 'org-grep-current)
    (local-set-key "c" 'org-grep-copy)
    (local-set-key "g" 'org-grep-recompute)
    (local-set-key "n" 'org-grep-next)
    (local-set-key "p" 'org-grep-previous)
    (local-set-key "q" 'org-grep-quit)
    (when (boundp 'org-mode-map)
      (define-key org-mode-map "\C-x`" 'org-grep-maybe-next-jump))
    counter))

(defun org-grep-join (fragments separator)
  (if fragments
      (concat (car fragments)
              (apply 'concat
                     (mapcar (lambda (fragment) (concat separator fragment))
                             (cdr fragments))))
    ""))

(defun org-grep-hi-lock-helper (regexp)
  ;; Stolen from hi-lock-process-phrase.
  ;; FIXME: ASCII only.  Sad that hi-lock ignores case-fold-search!
  ;; Also, hi-lock-face-phrase-buffer does not have an unface counterpart.
  (replace-regexp-in-string
   "\\<[a-z]"
   (lambda (text) (format "[%s%s]" (upcase text) text))
   regexp))

(defun org-grep-copy ()
  (interactive)
  (let ((buffer (get-buffer-create (format org-grep-buffer-name-copy-format
                                           org-grep-user-regexp))))
    (copy-to-buffer buffer (point-min) (point-max))
    (switch-to-buffer buffer)
    (goto-char (point-min))
    (while (re-search-forward org-grep-hit-regexp nil t)
      (insert "[ ] ")
      (forward-line 1))
    (org-mode)
    (goto-char (point-min))
    (org-show-subtree)))
  

(defun org-grep-current ()
  (interactive)
  ;; FIXME: save-current-buffer fails: the current buffer is not restored.
  (save-current-buffer (org-grep-current-jump)))

(defun org-grep-current-jump ()
  (interactive)
  ;; FIXME: org-reveal fails: the goal line stays collapsed and hidden.
  (beginning-of-line)
  (forward-char 2)
  (org-open-at-point)
  (org-reveal))

(defun org-grep-maybe-next-jump ()
  (interactive)
  (let ((buffer (current-buffer))
        (hits (get-buffer org-grep-buffer-name))
        jumped)
    (when hits
      (pop-to-buffer hits)
      (when (re-search-forward org-grep-hit-regexp nil t)
        (org-grep-current-jump)
        (setq jumped t)))
    (unless jumped
      (set-buffer buffer)
      (next-error))))

(defun org-grep-next ()
  (interactive)
  (when (re-search-forward org-grep-hit-regexp nil t)
    (org-grep-current)))

(defun org-grep-next-jump ()
  (interactive)
  (when (re-search-forward org-grep-hit-regexp nil t)
    (org-grep-current-jump)))

(defun org-grep-previous ()
  (interactive)
  (when (re-search-backward org-grep-hit-regexp nil t)
    (forward-char 2)
    (org-grep-current)))

(defun org-grep-quit ()
  (interactive)
  (kill-buffer))

(defun org-grep-recompute ()
  (interactive)
  (when org-grep-user-regexp
    (org-grep org-grep-user-regexp)))

;;; org-grep.el ends here
