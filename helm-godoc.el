;;; helm-godoc.el --- godoc with helm interface

;; Copyright (C) 2013 by Syohei YOSHIDA

;; Author: Syohei YOSHIDA <syohex@gmail.com>
;; URL: https://github.com/syohex/emacs-helm-godoc
;; Version: 0.01
;; Package-Requires: ((go-mode "0") (helm "1.0"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'helm)
(require 'go-mode)

(defgroup helm-godoc nil
  "`godoc' with helm interface"
  :group 'helm)

(defconst helm-godoc--package-regexp
  (concat "[[:word:][:multibyte:]/:.]+"))

(defconst helm-godoc--imported-module-regexp
  (format "\\(?:\\(%s\\)\\s-+\\)?\"\\(%s\\)\""
          go-identifier-regexp helm-godoc--package-regexp))

(defvar helm-godoc--imported-modules nil)

(defun helm-godoc--parse-oneline-import (start)
  (let ((end (line-end-position)))
    (when (re-search-forward helm-godoc--imported-module-regexp end t)
      (let ((alias (match-string-no-properties 1))
            (package (match-string-no-properties 2)))
        (if (and alias (not (string= alias "")))
            (cons (format "%s (alias %s)" package alias) package)
          (cons package package))))))

(defun helm-godoc--parse-group-import (start)
  (when (re-search-forward ")" nil t)
    (let ((end (point)))
      (save-excursion
        (goto-char start)
        (loop with importeds = nil
              while (< (point) end)
              do
              (progn
                (helm-aif (helm-godoc--parse-oneline-import (point))
                    (push it importeds))
                (forward-line 1)
                (back-to-indentation))
              finally return (reverse importeds))))))

(defconst helm-godoc--package-regexp
  (format "^\\s-*package\\s-+\\(%s\\)" go-identifier-regexp))

(defun helm-godoc--find-package (package)
  (let ((importeds (progn
                     (helm-godoc--collect-imported-modules (current-buffer))
                     helm-godoc--imported-modules)))
    (loop for imported in importeds
          for regexp = (concat package "\\'")
          when (string-match-p regexp imported)
          return imported)))

;;;###autoload
(defun helm-godoc-at-cursor ()
  (interactive)
  (let ((definfo (godef--call (point))))
    (if (string-match-p "\\`\\(?:-\\|error \\|godef: \\)" (car definfo))
        (message "Error: no information at cursor")
      (let* ((file (car (split-string (car definfo) ":")))
             (func (car (split-string (cadr definfo))))
             (buf (find-file-noselect file))
             (package (with-current-buffer buf
                        (goto-char (point-min))
                        (when (re-search-forward helm-godoc--package-regexp nil t)
                          (match-string-no-properties 1)))))
        (if (not package)
            (message "Not found: package")
          (let ((real-package (helm-godoc--find-package package)))
            (kill-buffer buf)
            (if (not real-package)
                (message "Not found: %s" package)
              (with-current-buffer (get-buffer-create "*godoc*")
                (view-mode -1)
                (erase-buffer)
                (let ((cmd (format "godoc %s %s" real-package func)))
                  (unless (zerop (call-process-shell-command cmd nil t))
                    (error "Failed: '%s'" cmd))
                  (goto-char (point-min))
                  (view-mode +1)
                  (pop-to-buffer (current-buffer)))))))))))

(defun helm-godoc--collect-imported-modules (buf)
  (setq helm-godoc--imported-modules nil)
  (let ((importeds nil))
    (with-current-buffer buf
      (save-excursion
        (goto-char (point-min))
        (while (and (re-search-forward "^\\s-*import\\s-*\\((\\)?" nil t)
                    (not (go-in-string-or-comment-p)))
          (let ((imported nil)
                (group-import (match-string-no-properties 1)))
            (if (and group-import (string= group-import "("))
                (setq imported (helm-godoc--parse-group-import (point)))
              (setq imported (helm-godoc--parse-oneline-import (point))))
            (when imported
              (setq importeds (append imported importeds)))))))
    (when importeds
      (setq helm-godoc--imported-modules (mapcar 'cdr importeds)))
    importeds))

(defun helm-godoc--import-package (candidate &optional as-alias)
  (let ((not-imported nil))
    (dolist (package (helm-marked-candidates))
      (if (member package helm-godoc--imported-modules)
          (push package not-imported)
        (go-import-add as-alias package)))
    (when not-imported
      (error "Already imported: '%s'"
             (mapconcat 'identity (reverse not-imported) ", ")))))

(defsubst helm-godoc--view-source-buffer (package)
  (get-buffer-create (format "*Godoc %s*" package)))

(defun helm-godoc--view-source-code (package)
  (with-current-buffer (helm-godoc--view-source-buffer package)
    (view-mode -1)
    (erase-buffer)
    (let ((cmd (concat "godoc -src " package)))
      (unless (zerop (call-process-shell-command cmd nil t))
        (error "Failed: %s" cmd))
      (goto-char (point-min))
      (go-mode)
      (view-mode +1))
    (pop-to-buffer (current-buffer))))

(defvar helm-godoc--imported-package-source
  '((name . "Imported Go Package")
    (candidates . (lambda ()
                    (helm-godoc--collect-imported-modules
                     helm-current-buffer)))
    (volatile)
    (action . (("View Document" . godoc)
               ("View Source Code" . helm-godoc--view-source-code)))
    (candidate-number-limit . 9999)))

(defvar helm-godoc--installed-package-source
  '((name . "Installed Go Package")
    (candidates . (lambda ()
                    (cons "builtin" (go-packages))))
    (action . (("View Document" . godoc)
               ("View Source Code" . helm-godoc--view-source-code)
               ("Import Package" . helm-godoc--import-package)
               ("Import Package as Alternative Name" .
                (lambda (cand)
                  (helm-godoc--import-package cand t)))))
    (candidate-number-limit . 9999)))

;;;###autoload
(defun helm-godoc ()
  (interactive)
  (helm :sources '(helm-godoc--imported-package-source
                   helm-godoc--installed-package-source)
        :buffer (get-buffer-create "*helm godoc*")))

(provide 'helm-godoc)

;;; helm-godoc.el ends here
