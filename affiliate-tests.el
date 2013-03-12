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
             (with-temp-buffer
               (insert haystack)
               (goto-char (point-min))
               (let ((needles-found
                      (aff-replace-urls-in-region (point-min) (point-max))))
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
       ("<tag>http://example.com/path_path</tag>" . ("http://example.com/path_path"))
       ("“is.gd/foo/”" . ("is.gd/foo/"))
       ("http://www.asianewsphoto.com/(S(neugxif4twuizg551ywh3f55))/Web_ENG/View_DetailPhoto.aspx?PicId=752" . ("http://www.asianewsphoto.com/(S(neugxif4twuizg551ywh3f55))/Web_ENG/View_DetailPhoto.aspx?PicId=752"))
       ;; Non-encoded non-ASCII characters are not matched.
       ("www.c.ws/䨹" . nil)
       ;; Domain names with no path component are not matched.
       ("Just a www.example.com link." . nil)
       ("<tag>http://example.com</tag>" . nil)
       ;; Not all three-letter extensions are domain names.
       ("The phrase filename.txt is not a domain name." . nil)
       ;; Will not match "raw" IDNs correctly, but should match Punycode IDNs.
       ("http://✪df.ws/1234" . ("df.ws/1234"))
       ("http://xn--df-oiy.ws/1234" . ("http://xn--df-oiy.ws/1234"))
       ("Jackdaws love my big sphinx of quartz. Pack my box with five dozen liquor jugs." . nil)
       ;; When multiple URLs appear, the found-urls list has them in reverse order from how they appear in the text.
       ("The canonical placeholder domains are example.com and its .org and .net kin; you can point to any path within them (e.g. http://www.example.org/path/path/path), cf [RFC 2606](http://tools.ietf.org/html/rfc2606)" . ("http://tools.ietf.org/html/rfc2606" "http://www.example.org/path/path/path"))))))

(ert-deftest aff-guess-merchant ()
  "Tests whether `aff-guess-merchant' correctly identifies merchant in input."
  (mapcar
   (lambda (pair) (should (equal (cdr pair) (aff-guess-merchant (car pair)))))
   '(("empty or not an URL" . unknown)
     ("http://itunes.apple.com/us/app/omnifocus-for-iphone/id284885288?mt=8" . itunes)
     ("http://www.amazon.com/The-Emacs-Lisp-Reference-Manual/dp/188211440X" . amazon))))

