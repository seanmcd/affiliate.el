;; -*- coding: utf-8 -*-

;; Unit tests for `affiliate.el'

(require 'ert)
(require 'affiliate)

(ert-deftest aff-replace-urls-in-region-test ()
    "Borrow Gruber's url-matching test data."
    ;; Adapted from http://daringfireball.net/misc/2010/07/url-matching-regex-test-data.text
  (flet ((aff-transform-url (text) text))
    (mapcar*
     (lambda (pair)
       (let ((haystack (car pair))
             (needle (cdr pair)))
         (message "Searching for [%s] in [%s] ..." needle haystack)
           (with-temp-buffer
           (insert haystack)
             (goto-char (point-min))
           (let ((needles-found
                  (aff-replace-urls-in-region (point-min) (point-max))))
             (message "post-search haystack: [%s]."
                      (buffer-substring-no-properties (point-min) (point-max)))
             (should (equal needles-found needle))))))
     '(("the quick brown http://example.com/path jumped over" . ("http://example.com/path"))
       ("http://example.com/blah_blah" . ("http://example.com/blah_blah"))
       ("http://example.com/blah_blah/" . ("http://example.com/blah_blah/"))))))

