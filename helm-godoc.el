;;; helm-godoc.el --- godoc with helm interface -*- lexical-binding: t; -*-

;; Copyright (C) 2016 by Syohei YOSHIDA

;; Author: Syohei YOSHIDA <syohex@gmail.com>
;; URL: https://github.com/syohex/emacs-helm-godoc
;; Version: 0.01
;; Package-Requires: ((go-mode "1.0.0") (helm-core "1.7.7") (emacs "24.4"))

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

;; helm-godoc provides viewing document and importing packaging statement
;; functions with helm interface.

;;; Code:

(require 'helm)
(require 'go-mode)
(require 'cl-lib)
(require 'subr-x)

(defgroup helm-godoc nil
  "`godoc' with helm interface"
  :group 'go)

(defconst helm-godoc--package-path-regexp
  (concat "[[:word:][:multibyte:]/:.-]+"))

(defconst helm-godoc--package-regexp
  (concat "[[:word:][:multibyte:]:_.-]+"))

(defconst helm-godoc--imported-module-regexp
  (format "\\(?:\\(%s\\)\\s-+\\)?\"\\(%s\\)\""
          helm-godoc--package-regexp helm-godoc--package-path-regexp))

(defvar helm-godoc--imported-modules nil)
(defconst helm-godoc--has-gopkgs (executable-find "gopkgs"))

(defun helm-godoc--format-alias (alias package)
  (cond ((string= alias "_")
         (format "%s (Unused import)" package))
        ((string= alias ".")
         (format "%s (Dot import)" package))
        (t (format "%s (alias: %s)" package alias))))

(defun helm-godoc--parse-oneline-import ()
  (let ((end (line-end-position)))
    (when (re-search-forward helm-godoc--imported-module-regexp end t)
      (let ((alias (match-string-no-properties 1))
            (package (match-string-no-properties 2)))
        (if (and alias (not (string-empty-p alias)))
            (cons (helm-godoc--format-alias alias package) package)
          (cons package package))))))

(defun helm-godoc--parse-group-import (start)
  (when (re-search-forward ")" nil t)
    (let ((end (point)))
      (save-excursion
        (goto-char start)
        (cl-loop with importeds = nil
                 while (< (point) end)
                 do
                 (progn
                   (helm-aif (helm-godoc--parse-oneline-import)
                       (push it importeds))
                   (forward-line 1)
                   (back-to-indentation))
                 finally return (reverse importeds))))))

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
              (setq imported (list (helm-godoc--parse-oneline-import))))
            (when imported
              (setq importeds (append imported importeds)))))))
    (when importeds
      (setq helm-godoc--imported-modules (mapcar #'cdr importeds)))
    importeds))

(defun helm-godoc--import-package (_candidate &optional as-alias)
  (let ((not-imported nil))
    (dolist (package (helm-marked-candidates))
      (if (member package helm-godoc--imported-modules)
          (push package not-imported)
        (go-import-add as-alias package)))
    (when not-imported
      (message "Already imported: '%s'"
               (mapconcat 'identity (reverse not-imported) ", ")))))

(defmacro with-helm-godoc-gopath (&rest body)
  (declare (indent 0) (debug t))
  (let ((vendor-path (cl-gensym))
        (orig-path (cl-gensym)))
    `(let* ((,orig-path (getenv "GOPATH"))
            (,vendor-path (cl-loop for dir in '("vendor" "_vendor")
                                   when (locate-dominating-file default-directory dir)
                                   return (concat it dir)))
            (process-environment (if ,vendor-path
                                     (cons (format "GOPATH=%s:%s" (expand-file-name ,vendor-path) ,orig-path)
                                           process-environment)
                                   process-environment)))
       ,@body)))

(defsubst helm-godoc--view-source-buffer (package)
  (get-buffer-create (format "*Godoc %s*" package)))

(defun helm-godoc--run-view-document (package)
  (with-helm-godoc-gopath
    (if current-prefix-arg
        (let* ((initval (with-helm-current-buffer
                          (thing-at-point 'symbol)))
               (queries (split-string (read-string "SubQuery: " initval))))
          (apply #'process-file "godoc" nil t nil package queries))
      (process-file "godoc" nil t nil package))))

(defun helm-godoc--view-document (package)
  (with-helm-godoc-gopath
    (let ((buf (get-buffer-create (format "*godoc %s*" package))))
      (with-current-buffer buf
        (view-mode -1)
        (erase-buffer)
        (unless (zerop (helm-godoc--run-view-document package))
          (error "Faild: 'godoc %s'" package))
        (view-mode +1)
        (goto-char (point-min))
        (pop-to-buffer (current-buffer))))))

(defun helm-godoc--view-source-code (package)
  (with-current-buffer (helm-godoc--view-source-buffer package)
    (view-mode -1)
    (erase-buffer)
    (with-helm-godoc-gopath
      (unless (zerop (process-file "godoc" nil t nil "-src" package))
        (error "Failed: 'godoc -src %s'" package)))
    (goto-char (point-min))
    (go-mode)
    (view-mode +1)
    (pop-to-buffer (current-buffer))))

(defun helm-godoc--go-packages ()
  (with-helm-godoc-gopath
   (cl-loop for package in (if helm-godoc--has-gopkgs
                               (sort (process-lines "gopkgs") #'string<)
                             (go-packages))
            unless (string-match-p "\\(?:^\\|/\\)\\(?:Godeps\\|internal\\)\\(?:/\\|$\\)" package)
            collect package)))

(defun helm-godoc--browse-url (package)
  (let ((url (cond ((string-match "github\\.com/[^/]+/[^/]+" package)
                    (concat "https://" (match-string-no-properties 0 package)))
                   ((string-match-p "golang.org/" package)
                    (concat "https://" package))
                   ((not (string-match-p "\\." package))
                    (concat "https://golang.org/pkg/" package))
                   (t package))))
    (browse-url url)))

(defvar helm-godoc--imported-package-source
  (helm-build-sync-source "Imported Go Package"
    :candidates (lambda ()
                  (helm-godoc--collect-imported-modules helm-current-buffer))
    :candidate-number-limit 9999
    :volatile t
    :action (helm-make-actions
             "View Document" #'helm-godoc--view-document
             "View Source Code" #'helm-godoc--view-source-code
             "Browse Web page" #'helm-godoc--browse-url)))

(defvar helm-godoc--installed-package-source
  (helm-build-sync-source "Installed Go Package"
    :candidates (lambda ()
                  (append '("builtin" "unsafe") (helm-godoc--go-packages)))
    :candidate-number-limit 9999
    :action (helm-make-actions
             "View Document" #'helm-godoc--view-document
             "View Source Code" #'helm-godoc--view-source-code
             "Import Package" #'helm-godoc--import-package
             "Import Package as Alternative Name"
             (lambda (cand)
               (helm-godoc--import-package cand t))
             "Browse Web page" #'helm-godoc--browse-url)))

(defvar helm-godoc--import-package-source
  (helm-build-sync-source "Import Go Package"
    :candidates #'helm-godoc--go-packages
    :candidate-number-limit 9999
    :action (helm-make-actions
             "Import Package" #'helm-godoc--import-package
             "Import Package as Alternative Name"
             (lambda (cand)
               (helm-godoc--import-package cand t))
             "View Document" #'helm-godoc--view-document
             "View Source Code" #'helm-godoc--view-source-code)))

(defvar helm-godoc--history nil)

;;;###autoload
(defun helm-godoc ()
  (interactive)
  (helm :sources '(helm-godoc--imported-package-source
                   helm-godoc--installed-package-source)
        :buffer "*helm godoc*"
        :history 'helm-godoc--history))

;;;###autoload
(defun helm-godoc-import ()
  (interactive)
  (helm :sources '(helm-godoc--import-package-source)
        :buffer "*helm godoc*"))

(provide 'helm-godoc)

;;; helm-godoc.el ends here
