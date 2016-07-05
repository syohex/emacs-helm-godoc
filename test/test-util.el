;;; test-util.el --- unit test for helm-go.el

;; Copyright (C) 2014 by Syohei YOSHIDA

;; Author: Syohei YOSHIDA <syohex@gmail.com>

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

(require 'ert)
(require 'cl-lib)

(ert-deftest parse-oneline-import-stdlib ()
  "Parsing normal import for standard library"
  (with-go-temp-buffer
    "
import (
	\"fmt\"
)
"
    (forward-cursor-on "fmt")
    (goto-char (line-beginning-position))
    (let ((got (helm-godoc--parse-oneline-import)))
      (should (equal got (cons "fmt" "fmt"))))))

(ert-deftest parse-oneline-import-github-library ()
  "Parsing normal import for github library"
  (with-go-temp-buffer
    "
import (
	\"github.com/peco/peco\"
)
"
    (forward-cursor-on "github")
    (goto-char (line-beginning-position))
    (let ((got (helm-godoc--parse-oneline-import))
          (expected (cons "github.com/peco/peco" "github.com/peco/peco")))
      (should (equal got expected)))))

(ert-deftest parse-oneline-import-stdlib-alias ()
  "Parsing alias import"
  (with-go-temp-buffer
    "
import (
	format \"fmt\"
)
"
    (forward-cursor-on "fmt")
    (goto-char (line-beginning-position))
    (let ((got (helm-godoc--parse-oneline-import)))
      (should (equal got (cons "fmt (alias: format)" "fmt"))))))

(ert-deftest parse-oneline-import-underscore ()
  "Parsing unused import"
  (with-go-temp-buffer
    "
import (
	_ \"github.com/mattn/go-sqlite\"
)
"
    (forward-cursor-on "github")
    (goto-char (line-beginning-position))
    (let ((got (helm-godoc--parse-oneline-import))
          (expected (cons "github.com/mattn/go-sqlite (Unused import)"
                          "github.com/mattn/go-sqlite")))
      (should (equal got expected)))))

(ert-deftest parse-oneline-import-dot ()
  "Parsing dot import"
  (with-go-temp-buffer
    "
import (
	. \"io/ioutil\"
)
"
    (forward-cursor-on "io")
    (goto-char (line-beginning-position))
    (let ((got (helm-godoc--parse-oneline-import))
          (expected (cons "io/ioutil (Dot import)" "io/ioutil")))
      (should (equal got expected)))))

(ert-deftest parse-group-import ()
  "Parsing group import"
  (with-go-temp-buffer
    "
import (
        \"fmt\"
        \"os\"
        \"reflect\"
        \"runtime\"

        \"github.com/jessevdk/go-flags\"
        \"github.com/nsf/termbox-go\"
        \"github.com/peco/peco\"
)
"
    (forward-cursor-on "import")
    (goto-char (line-end-position))
    (let ((got (mapcar 'car (helm-godoc--parse-group-import (point))))
          (expected (list "fmt" "os" "reflect" "runtime"
                          "github.com/jessevdk/go-flags"
                          "github.com/nsf/termbox-go"
                          "github.com/peco/peco")))
      (should (equal got expected)))))

(ert-deftest browse-url ()
  "Browse URL"
  (cl-letf (((symbol-function 'browse-url) (lambda (url &rest _args) url)))
    (should (string= (helm-godoc--browse-url "fmt")  "https://golang.org/pkg/fmt"))
    (should (string= (helm-godoc--browse-url "github.com/github/hub/version")
                     "https://github.com/github/hub"))
    (should (string= (helm-godoc--browse-url "golang.org/x/net/context")
                     "https://golang.org/x/net/context"))
    (should (string= (helm-godoc--browse-url "9fans.net/go/acme")
                     "9fans.net/go/acme"))))

;;; test-util.el ends here
