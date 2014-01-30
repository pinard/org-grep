;;; org-grep.el --- Kind of M-x rgrep adapted for Org mode.

;; Copyright © 2013, 2014 Progiciels Bourbeau-Pinard inc.

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

(require 'org)

(defvar org-grep-directories (list org-directory)
  "List of directories to search, default is org-directory only.")

(defvar org-grep-ellipsis " … "
  "Ellipsis text to replace any removed context, nil means no elision.")

(defvar org-grep-extensions '(".org")
  "List of extensions for searchable files.")

(defvar org-grep-extra-shell-commands nil
  "List of functions providing extra shell commands for grepping.
Each of such function is given REGEXP as an argument.")

(defvar org-grep-gnus-directory nil
  "Directory holding Gnus mail files.  Often \"~/Mail\".")

(defvar org-grep-grep-options "-i"
  "String containing default grep options.")

(defvar org-grep-hide-extension nil
  "Ignore extension while sorting and displaying.")

(defvar org-grep-maximum-context-size 200
  "Maximum size of a context chunk within a hit line, nil means no elision.")

(defvar org-grep-maximum-hits 2500
  "Maximum number of hits, nil means no limit.")

(defvar org-grep-rmail-shell-commands nil
  "List of functions providing shell commands to grep mailboxes.
Each of such function is given REGEXP as an argument.")

(defface org-grep-match-face
  '((((background dark)) (:background "lavender" :foreground "black"))
    (t (:background "lavender")))
  "Face for each org-grep match.")

(defface org-grep-ellipsis-face
  '((((background dark)) (:background "pink" :foreground "black"))
    (t (:background "pink")))
  "Face for each org-grep ellipsis.")

(defvar org-grep-buffer-name-format "*Org Grep — %s*")
(defvar org-grep-hits-buffer-name "*Org Grep hits*")
(defvar org-grep-mail-buffer-name "*Org Grep mail*")

(defvar org-grep-hit-regexp "^- ")
(defvar org-grep-mail-buffer nil)
(defvar org-grep-mail-buffer-file nil)
(defvar org-grep-last-full nil)
(defvar org-grep-last-options nil)
(defvar org-grep-last-regexp nil)

;;; Main driver functions.

;;;###autoload
(defun org-grep (regexp &optional prefix)
  (interactive
   (list (if (use-region-p)
             (buffer-substring (region-beginning) (region-end))
           (read-string "Enter a regexp to grep: "))
         current-prefix-arg))
  (if (and prefix (called-interactively-p 'any))
      (let ((org-grep-grep-options
             (read-string "Grep options: "
                          (and (not (string-equal org-grep-grep-options ""))
                               (concat org-grep-grep-options " ")))))
        (org-grep-internal regexp nil))
    (org-grep-internal regexp nil)))

(defun org-grep-full (regexp &optional prefix)
  (interactive
   (list (if (use-region-p)
             (buffer-substring (region-beginning) (region-end))
           (read-string "Enter a regexp to fully grep: "))
         current-prefix-arg))
  (if (and prefix (called-interactively-p 'any))
      (let ((org-grep-grep-options
             (read-string "Grep options: "
                          (and (not (string-equal org-grep-grep-options ""))
                               (concat org-grep-grep-options " ")))))
        (org-grep-internal regexp t))
    (org-grep-internal regexp t)))

(defun org-grep-internal (regexp full)
  (when (string-equal regexp "")
    (user-error "Nothing to find!"))

  ;; Prepare the hits buffer, removing its previous contents.
  (pop-to-buffer org-grep-hits-buffer-name)
  (buffer-disable-undo)
  (fundamental-mode)
  (setq buffer-read-only nil)
  (erase-buffer)
  (when org-grep-last-regexp
    (hi-lock-unface-buffer (org-grep-hi-lock-helper org-grep-last-regexp))
    (hi-lock-unface-buffer (regexp-quote org-grep-ellipsis)))

  ;; Save arguments, in particular for a redo command.
  (setq org-grep-last-full full
        org-grep-last-options org-grep-grep-options
        org-grep-last-regexp regexp)

  ;; Collect information.  Methods prefix each line with: some string
  ;; (likely the lower-cased base of the file name), a first NUL, a
  ;; disambiguing key (likely the full file name), a second NUL, a
  ;; line number justified to the right into 5 columns and space
  ;; filled, a third NUL, "- ", clickable information, then " :: ".
  (save-some-buffers t)
  (message "Finding occurrences...")
  (org-grep-from-org regexp)
  (when full
    (org-grep-from-rmail regexp)
    (org-grep-from-gnus regexp))

  ;; Sort lines, then attempt some serious cleanup on them.
  (sort-lines nil (point-min) (point-max))
  (let* ((hit-count (count-lines (point-min) (point-max)))
         (truncated (and org-grep-maximum-hits
                         (> hit-count org-grep-maximum-hits)))
         info duplicates key name pair current-name
         ellipsis-length distance-trigger half-maximum
         line-start line-limit start-context end-context
         resume-point end-delete delete-size shrink-delta)

    ;; Truncate the buffer if it contains too many hits.
    (if (not truncated)
        (message "Finding occurrences... done (%d found)" hit-count)
      (message "Finding occurrences... done (showing %d / %d)"
               org-grep-maximum-hits hit-count)
      (goto-char (point-min))
      (forward-line org-grep-maximum-hits)
      (delete-region (point) (point-max)))

    ;; Do a preliminary pass to discover duplicate keys.
    (goto-char (point-min))
    (while (not (eobp))
      (when (looking-at "\\([^\0]*\\)\0\\([^\0]*\\)\0")
        (setq key (match-string 1)
              name (match-string 2)
              pair (assoc key info))
        (cond ((not pair) (setq info (cons (cons key name) info)))
              ((string-equal (cdr pair) name))
              ((member (car pair) duplicates))
              (t (setq duplicates (cons key duplicates)))))
      (forward-line 1))
    (when (and org-grep-ellipsis org-grep-maximum-context-size)
      (setq ellipsis-length (length org-grep-ellipsis)
            distance-trigger (+ org-grep-maximum-context-size ellipsis-length)
            half-maximum (/ org-grep-maximum-context-size 2)))

    ;; Insert title and initial header.
    (org-grep-insert-title "Org Grep hits"
                           (if truncated
                               (format "retained *%d* occurrences out of *%d*"
                                       org-grep-maximum-hits hit-count)
                             (format "found *%d* occurrences" hit-count)))
    (insert "* Occurrences\n")

    ;; Find and process all prefixed lines.
    (while (re-search-forward "^\\([^\0]*\\)\0\\([^\0]*\\)\0[^\0]*\0" nil t)

      ;; Remove sorting information
      (setq key (match-string 1)
            name (match-string 2))
      (replace-match "")
      (setq line-end (line-end-position))
      (when (search-forward " :: " line-end t)

        ;; Give more information on the file name if this is sound.
        (cond ((= (following-char) ?\n)
               (let ((base (file-name-nondirectory name))
                     (directory (file-name-directory name)))
                 ;; The context would be otherwise empty.
                 (insert "="
                         (abbreviate-file-name
                          (if org-grep-hide-extension name directory))
                         "=   [[file:" directory "::" (regexp-quote base)
                         "][dired]]")
                 (setq line-end (line-end-position))))
              ((and (member key duplicates)
                    (not (string-equal name current-name)))
               ;; The reference is ambiguous.
               (setq current-name name)
               (backward-char 4)
               (insert " ="
                       (abbreviate-file-name (if org-grep-hide-extension
                                                 name
                                               (file-name-directory name)))
                       "=")
               (forward-char 4)
               (setq line-end (line-end-position))))

        ;; Remove extra whitespace.
        (setq line-start (point))
        (while (re-search-forward " [ \f\t\b]+\\|[\f\t\b][ \f\t\b]*"
                                  line-end t)
          (replace-match "  ")
          (setq line-end (line-end-position)))

        ;; Possibly elide big contexts.
        (when distance-trigger
          (goto-char line-start)
          (while (< (point) line-end)
            (setq start-context (point))
            (if (re-search-forward regexp line-end t)
                (setq end-context (match-beginning 0)
                      resume-point (match-end 0))
              (setq end-context line-end
                    resume-point line-end))
            (when (> (- end-context start-context) distance-trigger)
              (goto-char (- end-context half-maximum))
              (forward-word)
              (backward-word)
              (setq end-delete (point))
              (goto-char (+ start-context half-maximum))
              (backward-word)
              (forward-word)
              (setq delete-size (- end-delete (point))
                    shrink-delta (- delete-size ellipsis-length))
              (when (> shrink-delta 0)
                (delete-char delete-size)
                (insert org-grep-ellipsis)
                (setq resume-point (- resume-point shrink-delta)
                      line-end (- line-end shrink-delta))))
            (goto-char resume-point))))
      (forward-line 1))

    ;; Activate Org mode on the results.
    (org-mode)
    (goto-char (point-min))
    (show-all)

    ;; Highlight the search string and each ellipsis.
    (hi-lock-face-buffer (org-grep-hi-lock-helper regexp)
                         'org-grep-match-face)
    (hi-lock-face-buffer (regexp-quote org-grep-ellipsis)
                         'org-grep-ellipsis-face)

    ;; Add special commands to the keymap.
    (use-local-map (copy-keymap (current-local-map)))
    (setq buffer-read-only t)
    (local-set-key "\C-c\C-c" 'org-grep-current-jump)
    (local-set-key "\C-x`" 'org-grep-next-jump)
    (local-set-key "." 'org-grep-current)
    (local-set-key "c" 'org-grep-copy)
    (local-set-key "g" 'org-grep-redo)
    (local-set-key "n" 'org-grep-next)
    (local-set-key "p" 'org-grep-previous)
    (local-set-key "q" 'org-grep-quit)
    (local-set-key "s" 'org-grep-structure)
    (when (boundp 'org-mode-map)
      (define-key org-mode-map "\C-x`" 'org-grep-maybe-next-jump))

    ;; Clean up.
    (when org-grep-mail-buffer
      (kill-buffer org-grep-mail-buffer)
      (setq org-grep-mail-buffer nil
            org-grep-mail-buffer-file nil))
    hit-count))

;;; Shell code generation.

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
           (base (if org-grep-hide-extension
                     (file-name-base file)
                   (file-name-nondirectory file))))
      (replace-match
       (concat (downcase base) "\0" file "\0" (format "%5d" line) "\0"
               "- [[file:\\1::\\2][" base ":]]\\2 :: "))

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
            " | xargs grep " org-grep-grep-options
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
         (concat (downcase base) "\0" file "\0" (format "%5d" line) "\0"
                 "- [[" ref "][" base ":]]" (number-to-string line) " :: "))
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
           (base (if org-grep-hide-extension
                     (file-name-base file)
                   (file-name-nondirectory file)))
           (ref (save-match-data (org-grep-message-ref file line nil))))
      (replace-match
       (concat (downcase base) "\0" file "\0" (format "%5d" line) "\0"
               "- [[" ref "][" base ":]]" (number-to-string line) " :: "))
      (forward-line))))

(defun org-grep-from-org-shell-command (regexp)
  (if org-grep-directories
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
              " -print0 | xargs -0 grep " org-grep-grep-options
              " -n " (shell-quote-argument regexp))
    ":"))

;;; Additional commands for an Org Grep hits buffer.

(defun org-grep-copy ()
  (interactive)
  (let ((buffer (get-buffer-create
                 (format org-grep-buffer-name-format
                         org-grep-last-regexp))))
    (copy-to-buffer buffer (point-min) (point-max))
    (switch-to-buffer buffer)
    (goto-char (point-min))
    (delete-region (point) (line-end-position))
    (insert "#+TITLE: Org Grep copy")
    (while (re-search-forward org-grep-hit-regexp nil t)
      (insert "[ ] ")
      (forward-line 1))
    (org-mode)
    (goto-char (point-min))
    (show-all)))

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

(defun org-grep-redo ()
  (interactive)
  (when org-grep-last-regexp
    (let ((org-grep-grep-options org-grep-last-options))
      (org-grep-internal org-grep-last-regexp org-grep-last-full))))

(defun org-grep-structure ()
  (interactive)
  (let ((buffer (current-buffer))
        ;; INFO is a recursive structure, made up of a list of ITEMs.
        ;; Each ITEM is either (START . END) or (SUBDIR INFO).  START
        ;; and END are integers specifying the limits of a textual
        ;; region in the original, already sorted hits buffer.  SUBDIR
        ;; is a string representing the last fragment of a file path.
        info path seen start)

    ;; Digest all needed information into INFO.
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward org-grep-hit-regexp nil t)
        (cond ((looking-at "\\[\\[file:\\([^:]+\\)")
               (setq seen (match-string-no-properties 1))
               (unless (and path (string-equal seen path))
                 (beginning-of-line)
                 (when path
                   (setq info (org-grep-structure-add-file
                               info start (point) path)))
                 (setq path seen
                       start (point))
                 (forward-line 1)))))
      (when (and start (> (point-max) start))
        (setq info (org-grep-structure-add-file
                    info start (point-max) path))))

    ;; Reorganise all saved information into a new buffer.
    (switch-to-buffer (get-buffer-create
                       (format org-grep-buffer-name-format
                               org-grep-last-regexp)))
    (erase-buffer)
    (org-grep-insert-title "Org Grep structure" nil)
    ;; The first SUBDIR is always empty, this loop pops it out.
    (mapc (lambda (pair)
            (org-grep-structure-rebuild (cdr pair) buffer "*" (car pair)))
          (org-grep-structure-sort-info info))

    ;; Use Org mode over the reconstructed buffer.
    (org-mode)
    (goto-char (point-min))
    (org-overview)
    (org-content)))

(defun org-grep-structure-add-file (info start end text)
  (setq text (abbreviate-file-name (file-name-directory text)))
  (when (= (aref text (1- (length text))) ?/)
    (setq text (substring text 0 (1- (length text)))))
  (org-grep-structure-digest
   info start end
   (mapcar (lambda (fragment) (concat fragment "/"))
           (split-string text "/"))))

(defun org-grep-structure-digest (info start end fragments)
  "Modify INFO to register that PATH uses START to END in the hits buffer."
  (if fragments
      (let ((pair (assoc (car fragments) info)))
        (if (not pair)
            (cons (cons (car fragments)
                        (org-grep-structure-digest
                         nil start end (cdr fragments)))
                  info)
          (rplacd pair (org-grep-structure-digest
                        (cdr pair) start end (cdr fragments)))
          info))
    (cons (cons start end) info)))

(defun org-grep-structure-rebuild (info buffer prefix path)

    ;; Collapse hierarchy whenever possible.
    (while (and (= (length info) 1) (stringp (caar info)))
      (setq path (concat path (caar info))
            info (cdar info)))

    ;; Insert an Org header.
    (insert prefix " =" path "=   [[file:" path "][dired]]\n")

    ;; Insert all information under that header.
    (mapc (lambda (pair)
            (if (stringp (car pair))
                ;; We have (SUBDIR INFO).  Insert subdirectories recursively.
                (org-grep-structure-rebuild (cdr pair) buffer (concat prefix "*")
                                            (concat path (car pair)))
              ;; We have (START . END).  Insert items from the original hits buffer.
              (insert-buffer-substring buffer (car pair) (cdr pair))))
          (org-grep-structure-sort-info info)))

(defun org-grep-structure-sort-info (info)
  "Sort INFO to get all (START . END) first, then all (SUBDIR INFO)."
  (sort info (lambda (a b)
               (if (stringp (car a))
                   (and (stringp (car b))
                        (string-lessp (car a) (car b)))
                 (or (stringp (car b))
                     (< (car a) (car b)))))))

;;; Miscellaneous service functions.

(defun org-grep-insert-title (title comment)
  (goto-char (point-min))
  (insert "#+TITLE: " title "\n"
          "=org-grep" (if org-grep-last-full "-full" "")
          (if (string-equal org-grep-grep-options "")
              ""
            (concat " " org-grep-grep-options))
          " " (shell-quote-argument org-grep-last-regexp) "="
          (if comment (concat " " comment) "")
          "\n\n"))

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
  (if (string-match "\\b-[A-Za-z]*i" org-grep-grep-options)
      (replace-regexp-in-string
       "\\<[a-z]"
       (lambda (text) (format "[%s%s]" (upcase text) text))
       regexp)
    regexp))

;;; org-grep.el ends here
