(require 'f)

(defvar fogbugz-mode-support-path
  (f-dirname load-file-name))

(defvar fogbugz-mode-features-path
  (f-parent fogbugz-mode-support-path))

(defvar fogbugz-mode-root-path
  (f-parent fogbugz-mode-features-path))

(add-to-list 'load-path fogbugz-mode-root-path)

(require 'fogbugz)
(require 'espuds)
(require 'ert)
(require 'el-mock)

(Setup
 (setq fogbugz-email "bob@bob.com")
 (setq fogbugz-password "secret")
 (setq fogbugz-domain "localhost:1234"))

(Before
 ;; Before each scenario is run
 )

(After
 ;; After each scenario is run
 )

(Teardown
 ;; After when everything has been run
 )
