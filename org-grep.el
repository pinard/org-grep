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
;; like M-x rgrep.  Optionally, the tool may simultaneously search
;; Unix mailboxes, Gnus mailgroups, or other textual files.

;; See https://github.com/pinard/org-grep.

;;; Code:

(defvar org-grep-directories nil
  "Directories to search, or ORG-DIRECTORY if nil.")

(defvar org-grep-ellipsis "[...]"
  "Ellipsis text to replace any removed context.")

(defvar org-grep-maximum-context-size 200
  "Maximum size of a context chunk within a hit line, nil means no limit.")

(defvar org-grep-extensions '(".org")
  "List of extensions for searchable files.")

(defvar org-grep-extra-shell-commands nil
  "List of functions providing extra shell commands for grepping.
Each of such function is given REGEXP as an argument.")

(defvar org-grep-gnus-directory nil
  "Directory holding Gnus mail files.  Often ~/Mail.")

(defvar org-grep-rmail-shell-commands nil
  "List of functions providing shell commands to grep mailboxes.
Each of such function is given REGEXP as an argument.")

(defvar org-grep-hits-buffer-name " *Org grep hits*")
(defvar org-grep-hits-buffer-name-copy-format "*Org grep %s*")
(defvar org-grep-hit-regexp "^- ")
(defvar org-grep-mail-buffer nil)
(defvar org-grep-mail-buffer-file nil)
(defvar org-grep-mail-buffer-name " *Org grep mail*")
(defvar org-grep-user-regexp nil)

(defun org-grep (regexp &optional full)
  (interactive
   (list (if (use-region-p)
             (buffer-substring (region-beginning) (region-end))
           (read-string "Enter a string or a regexp to grep: "))
         current-prefix-arg))
  (when (string-equal regexp "")
    (user-error "Nothing to find!"))
  ;; Collect information.  Methods should prefix each line with
  ;; sorting keys, a NUL, "- ", clickable information, then " :: ".  A
  ;; sorting key before the NUL is the concatenation of some
  ;; alphabetical string related to the file name, followed by a line
  ;; number justified to the right into 5 columns and space filled.
  (pop-to-buffer org-grep-hits-buffer-name)
  (fundamental-mode)
  (setq buffer-read-only nil)
  (erase-buffer)
  (save-some-buffers t)
  (org-grep-from-org regexp)
  (when full
    (org-grep-from-rmail regexp)
    (org-grep-from-gnus regexp))
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
    ;; Remove extra whitespace, also elide big contexts.
    (goto-char (point-min))
    (while (re-search-forward "[ \f\t\b][ \f\t\b]+" nil t)
      (replace-match "  "))
    (when org-grep-maximum-context-size
      (let* ((ellipsis-length (length org-grep-ellipsis))
             (distance-trigger (+ org-grep-maximum-context-size
                                 ellipsis-length))
             (half-maximum (/ org-grep-maximum-context-size 2))
             (regexp-or-newline (concat "\n\\|\\(?:" regexp "\\)"))
             start-context end-context resume-point end-delete)
        (goto-char (point-min))
        (while (not (eobp))
          (setq start-context (point))
          (if (re-search-forward regexp-or-newline nil t)
              (setq end-context (match-beginning 0)
                    resume-point (match-end 0))
            (setq end-context (point-max)
                  resume-point (point-max)))
          (when (> (- end-context start-context) distance-trigger)
            (goto-char (- end-context half-maximum))
            (forward-word)
            (backward-word)
            (setq end-delete (point))
            (goto-char (+ start-context half-maximum))
            (backward-word)
            (forward-word)
            (let ((delete-size (- end-delete (point))))
              (when (> delete-size ellipsis-length)
                (delete-char delete-size)
                (insert org-grep-ellipsis)
                (setq resume-point
                      (- resume-point (- delete-size ellipsis-length))))))
          (goto-char resume-point))))
    ;; Activate Org mode on the results.
    (goto-char (point-min))
    (insert (format "* Grep found %d occurrences of %s\n\n" counter regexp))
    (org-mode)
    (goto-char (point-min))
    (org-show-subtree)
    ;; Highlight the search string and each ellipsis.
    (when org-grep-user-regexp
      (hi-lock-unface-buffer (org-grep-hi-lock-helper org-grep-user-regexp))
      (hi-lock-unface-buffer (regexp-quote org-grep-ellipsis)))
    (hi-lock-face-buffer (org-grep-hi-lock-helper regexp) 'hi-yellow)
    (hi-lock-face-buffer (regexp-quote org-grep-ellipsis) 'hi-blue)
    (setq org-grep-user-regexp regexp)
    ;; Add special commands to the keymap.
    (use-local-map (copy-keymap (current-local-map)))
    (setq buffer-read-only t)
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
    ;; Clean up.
    (when org-grep-mail-buffer
      (kill-buffer org-grep-mail-buffer)
      (setq org-grep-mail-buffer nil
            org-grep-mail-buffer-file nil))
    counter))

(defun org-grep-from-org (regexp)
  ;; Execute shell command.
  (goto-char (point-max))
  (let ((command (org-grep-join
                  (mapcar (lambda (function) (apply function (list regexp)))
                          (cons 'org-grep-from-org-shell-command
                                org-grep-extra-shell-commands))
                  "; ")))
    (shell-command command t))
  ;; Prefix found lines.
  (while (re-search-forward "^\\([^:]+\\):\\([0-9]+\\):" nil t)
    (let* ((file (match-string 1))
           (line (string-to-number (match-string 2)))
           (directory (file-name-directory file))
           (base (file-name-sans-extension (file-name-nondirectory file))))
      (replace-match (concat (downcase base) " " (format "%5d" line)
                             "\0- [[file:\\1::\\2][" base ":]]\\2 :: "))
      ;; Moderately try to resolve relative links.
      (while (re-search-forward "\\[\\[\\([^]\n:]+:\\)?\\([^]]+\\)"
                                (line-end-position) t)
        (let ((method (match-string 1))
              (reference (match-string 2)))
          (cond ((not method)
                 (replace-match (concat "[[file:" file "::\\2")))
                ((member method '("file:" "rmail:"))
                 (unless (memq (aref reference 0) '(?~ ?/))
                   (replace-match
                    (concat "[[" method directory reference)))))))
      (forward-line))))

(defun org-grep-from-gnus (regexp)
  (when (and org-grep-gnus-directory
             (file-directory-p org-grep-gnus-directory))
    ;; Execute shell command.
    (goto-char (point-max))
    (let ((command
           (concat
            "find " org-grep-gnus-directory " -type f"
            " | grep -v"
            " '\\(^\\|/\\)[#.]\\|~$\\|\\.mrk$\\|\\.nov$\\|\\.overview$'"
            " | grep -v"
            " '\\(^\\|/\\)\\(Incoming\\|archive/\\|active$\\|/junk$\\)'"
            " | xargs grep" (if case-fold-search " -i" "")
            " -n " (shell-quote-argument regexp))))
      (shell-command command t))
    ;; Prefix found lines.
    (while (re-search-forward "^\\([^:]+\\):\\([0-9]+\\):" nil t)
      (let* ((file (match-string 1))
             (line (string-to-number (match-string 2)))
             (base (file-name-nondirectory file))
             (ref (save-match-data (org-grep-message-ref
                                    file line org-grep-gnus-directory))))
        (save-match-data
          (when (string-match "^[0-9]+$" base)
            (setq base (file-name-nondirectory
                        (substring (file-name-directory file) 0 -1)))))
        (replace-match
         (concat (downcase base) " " (format "%5d" line) "\0- [[" ref
                 "][" base ":]]" (number-to-string line) " :: "))
        (forward-line)))))

(defun org-grep-from-rmail (regexp)
  ;; Execute shell command.
  (goto-char (point-max))
  (let ((command (org-grep-join
                  (mapcar (lambda (function) (apply function (list regexp)))
                          org-grep-rmail-shell-commands)
                  "; ")))
    (shell-command command t))
  ;; Prefix found lines.
  (while (re-search-forward "^\\([^:]+\\):\\([0-9]+\\):" nil t)
    (let* ((file (match-string 1))
           (line (string-to-number (match-string 2)))
           (base (file-name-sans-extension (file-name-nondirectory file)))
           (ref (save-match-data (org-grep-message-ref file line nil))))
      (replace-match
       (concat (downcase base) " " (format "%5d" line) "\0- [[" ref
               "][" base ":]]" (number-to-string line) " :: "))
      (forward-line))))

(defun org-grep-from-org-shell-command (regexp)
  (concat "find "
          (if org-grep-directories
              (org-grep-join org-grep-directories " ")
            org-directory)
          (and org-grep-extensions
               (concat " -regex '.*\\("
                       (org-grep-join
                        (mapcar 'regexp-quote org-grep-extensions)
                        "\\|")
                       "\\)'"))
          " -print0 | xargs -0 grep" (if case-fold-search " -i" "")
          " -n " (shell-quote-argument regexp)))

(defun org-grep-message-ref (file line gnus-directory)
  (unless (and org-grep-mail-buffer (buffer-name org-grep-mail-buffer))
    (setq org-grep-mail-buffer (get-buffer-create org-grep-mail-buffer-name)))
  (save-excursion
    (set-buffer org-grep-mail-buffer)
    (unless (string-equal file org-grep-mail-buffer-file)
      (erase-buffer)
      (insert-file file)
      (setq org-grep-mail-buffer-file file))
    (let ((case-fold-search t))
      (goto-line line)
      ;; FIXME: Should limit search to current message header!
      (if (not (search-backward "\nmessage-id:" nil t))
          (concat "file:" file "::" (number-to-string line))
        (forward-char 12)
        (skip-chars-forward " ")
        (let ((id (buffer-substring (point) (line-end-position))))
          (if gnus-directory
              (let ((group (dired-make-relative file gnus-directory)))
                (if (string-equal (substring group 0 6) "/nnml/")
                    (concat "gnus:nnml:"
                            (substring (file-name-directory group) 6 -1)
                            "#" id)
                  (concat "gnus:nnfolder:" (substring group 1) "#" id)))
            (concat "rmail:" file "#" id)))))))

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
  (if case-fold-search
      (replace-regexp-in-string
       "\\<[a-z]"
       (lambda (text) (format "[%s%s]" (upcase text) text))
       regexp)
    regexp))

(defun org-grep-copy ()
  (interactive)
  (let ((buffer (get-buffer-create (format org-grep-hits-buffer-name-copy-format
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
        (hits (get-buffer org-grep-hits-buffer-name))
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
