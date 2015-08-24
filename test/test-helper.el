(require 'el-mock)
(require 'cl)

(when (require 'undercover nil t)
  (setq undercover-force-coverage t)
  (undercover "*.el" (:report-file "local-report.json") (:send-report nil)))
(require 'fogbugz)

(setq fogbugz-domain "example.fogbugz.com")
(setq fogbugz-email "email@example.com")
(setq fogbugz-token "abcde")
(setq fogbugz-password "secret")

