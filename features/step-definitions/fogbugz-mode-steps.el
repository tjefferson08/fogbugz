;; This file contains your project specific step definitions. All
;; files in this directory whose names end with "-steps.el" will be
;; loaded automatically by Ecukes.

;; (Given "I am in buffer" "\\(.+\\)\"$"
;;        (lambda (buffer-name)
;;       switch
;;       ))

(And "the mock is set for \"\\(.+\\)\"$"
     (lambda (method)

       (cond
        ((equal method "logon")

         ;; nuclear "mock", just redefine the whole dang function
         (defun url-retrieve-synchronously (url)
           (setq url-http-end-of-headers 13)
           (switch-to-buffer (get-buffer-create "xml-mock"))
           (insert "FAKE HEADERS<?xml version=\"1.0\" encoding=\"UTF-8\"?><response><token>123</token></response>")
           (get-buffer "xml-mock"))))))

(And "I inspect the variable \"\\(.+\\)\"$"
     (lambda (variable-name-string)
       (message (symbol-value (intern variable-name-string)))))
