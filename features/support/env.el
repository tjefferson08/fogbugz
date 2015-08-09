(require 'f)

(defvar fogbugz-mode-support-path
  (f-dirname load-file-name))

(defvar fogbugz-mode-features-path
  (f-parent fogbugz-mode-support-path))

(defvar fogbugz-mode-root-path
  (f-parent fogbugz-mode-features-path))

(add-to-list 'load-path fogbugz-mode-root-path)

(require 'fogbugz-mode)
(require 'espuds)
(require 'ert)

(Setup
 ;; Before anything has run
 )

(Before
 ;; Before each scenario is run
 )

(After
 ;; After each scenario is run
 )

(Teardown
 ;; After when everything has been run
 )
