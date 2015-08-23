(require 'f)

(defvar fogbugz-support-path
  (f-dirname load-file-name))

(defvar fogbugz-features-path
  (f-parent fogbugz-support-path))

(defvar fogbugz-root-path
  (f-parent fogbugz-features-path))

(add-to-list 'load-path fogbugz-root-path)

(require 'fogbugz)
(require 'espuds)
(require 'ert)
(require 'el-mock)
(require 'shut-up)

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
