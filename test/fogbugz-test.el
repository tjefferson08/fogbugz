(require 'fogbugz-mode)
(require 'el-mock)
(require 'cl)

(ert-deftest fogbugz-base-uri-test ()
  (let ((fogbugz-domain "some-domain.com"))
    (should (equal
             (fogbugz-base-uri "some-method")
             "https://some-domain.com/api.asp?cmd=some-method"))))

(ert-deftest fogbugz-base-uri-error-test ()
  (let ((fogbugz-domain "some-domain.com"))
    (should-error
     (fogbugz-base-uri nil)))

  (let ((fogbugz-domain nil))
    (should-error
     (fogbugz-base-uri "some-method"))))

(ert-deftest fogbugz-logon-request-test ()
  (with-mock
    (mock (fogbugz-request
           "logon"
           '(("email" . "email@email.com") ("password" . "secret")))
          => "raw-data")
    (mock (fogbugz-parse-logon-response "raw-data") => "parsed-token")

    (let ((fogbugz-token nil)
          (fogbugz-email "email@email.com")
          (fogbugz-password "secret"))
      (should (equal (fogbugz-logon) "parsed-token")))))

(ert-deftest fogbugz-logon-no-request-test ()
  (with-mock
    (not-called fogbugz-request)
    (not-called fogbugz-parse-logon-response)

    (let ((fogbugz-token "already-set"))
      (should (equal (fogbugz-logon) "already-set")))))

(defun run-tests ()
  (interactive)
  (shell-command
   "cd ~/fogbugz-mode && cask exec ert-runner -l ./fogbugz.el"))
