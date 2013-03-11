;; -*- coding: utf-8 -*-

;; Unit tests for `affiliate.el'

(require 'ert)
(require 'affiliate)

(ert-deftest aff-find-urls ()
  "Tests whether the function finds the relevant URLs in input."
  ;; Adapted from http://daringfireball.net/misc/2010/07/url-matching-regex-test-data.text
  (flet ((aff-transform-url (text) text)
         (scrutinize-pair (pair)
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
                 (should (equal needles-found needle)))))))
    (mapcar
     #'scrutinize-pair
     '(("the quick brown http://example.com/path_path jumped over" . ("http://example.com/path_path"))
       ("http://example.com/path_path/path" . ("http://example.com/path_path/path"))
       ("http://example.com/path_path/path/" . ("http://example.com/path_path/path/"))
       ("(Something like http://example.com/path_path/path)" . ("http://example.com/path_path/path"))
       ;; Although legal, usually a full-stop as the last character of a URL is usually not intended.
       ;; ("http://example.com/path_path/path." . ("http://example.com/path_path/path"))
       ;; ("http://example.com/path_path/path/." . ("http://example.com/path_path/path/"))
       ("<http://example.com/path_path/path>" . ("http://example.com/path_path/path"))
       ("<http://example.com/path_path/path/>" . ("http://example.com/path_path/path/"))
       ("http://example.com/path_path/path," . ("http://example.com/path_path/path"))
       ("http://www.extinguishedscholar.com/wpglob/?p=364." . ("http://www.extinguishedscholar.com/wpglob/?p=364"))
       ("http://example.com/something?with,commas,in,url, but not at end" . ("http://example.com/something?with,commas,in,url"))
       ("can it find a bit.ly/foo url?" . ("bit.ly/foo"))
       ("“is.gd/foo/”" . ("is.gd/foo/"))
       ("http://www.asianewsphoto.com/(S(neugxif4twuizg551ywh3f55))/Web_ENG/View_DetailPhoto.aspx?PicId=752" . ("http://www.asianewsphoto.com/(S(neugxif4twuizg551ywh3f55))/Web_ENG/View_DetailPhoto.aspx?PicId=752"))))))

