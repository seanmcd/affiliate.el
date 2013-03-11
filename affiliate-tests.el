;; -*- coding: utf-8 -*-

;; Unit tests for `affiliate.el'

(require 'ert)
(require 'affiliate)

(ert-deftest aff-replace-urls-in-region-test ()
    "Borrow Gruber's url-matching test data."
    ;; Adapted from http://daringfireball.net/misc/2010/07/url-matching-regex-test-data.text
  (flet ((try-url (url)
           (with-temp-buffer
             (insert url)
             (goto-char (point-min))
             (aff-replace-urls-in-region (point-min) (point-max)))))
    (should (equal "http://foo.com/blah_blah"
                   (try-url "http://foo.com/blah_blah")))
    (should (equal "http://foo.com/blah_blah/"
                   (try-url "http://foo.com/blah_blah/")))))
