;; -*- coding: utf-8 -*-

;; Unit tests for `affiliate.el'

(require 'ert)
(require 'affiliate)

(defmacro aff-test-pairs (fun-under-test &rest pairs)
  `(progn
     ,@(loop for pair in pairs
             collecting  (let ((test-input (car pair))
                               (expected-output (cdr pair)))
                           `(should (equal (,fun-under-test ,test-input) ,expected-output))))))

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
  (aff-test-pairs
   aff-guess-merchant
   ("empty or not an URL" . 'unknown)
   ("http://itunes.apple.com/us/app/omnifocus-for-iphone/id284885288?mt=8" . 'itunes)
   ("http://www.amazon.com/The-Emacs-Lisp-Reference-Manual/dp/188211440X" . 'amazon)))

(ert-deftest aff-dissect-amazon-url ()
  "Tests whether function correctly identifies ASIN/domain in URLs."
  (aff-test-pairs
   aff-dissect-amazon-url
   ("http://www.amazon.com/gp/product/006097625X" . '("006097625X" "com"))
   ("http://www.amazon.com/Lisp-Advanced-Techniques-Common/dp/0130305529/ref=sr_1_1?s=books&ie=UTF8&qid=1349897609&sr=1-1&keywords=on+lisp" . '("0130305529" "com"))
   ("http://www.amazon.com/dp/1435712757/ref=pd_sim_b_2" . '("1435712757" "com"))
   ("http://www.amazon.com/Practical-Common-Lisp-first-Text/dp/B004T91X0E/ref=tmm_hrd_title_2" . '("B004T91X0E" "com"))
   ("http://www.amazon.com/o/ASIN/B00746LVOM/ref=sr_1_1?ie=UTF8&qid=1349897747&sr=8-1&keywords=apple+ipad" . '("B00746LVOM" "com"))
   ("http://www.amazon.com/gp/product/006097625X/ref=as_li_qf_sp_asin_il_tl?ie=UTF8&camp=1789&creative=9325&creativeASIN=006097625X&linkCode=as2&tag=othercode-20" . '("006097625X" "com"))))

(ert-deftest aff-dissect-itunes-url ()
  "Tests whether function correctly identifies content-ids in iTunes URLs."
  (aff-test-pairs
   aff-dissect-itunes-url
   ("http://itunes.apple.com/us/app/omnifocus-for-iphone/id284885288?mt=8" . '("id284885288" "app" "us"))
   ("http://itunes.apple.com/us/app/omnifocus-for-ipad/id383804552?mt=8" . '("id383804552" "app" "us"))
   ("http://itunes.apple.com/us/app/fog-of-world/id505367096?mt=8&partnerId=30" . '("id505367096" "app" "us"))
   ("https://itunes.apple.com/us/album/immersion-deluxe-version/id414575481" . '("id414575481" "album" "us"))))

(ert-deftest aff-make-amazon-link ()
  "Tests whether function correctly creates an Amazon affiliate link."
  (flet
      ((aff-dissect-amazon-url (url) url))
    (let ((aff-amazon-id "amazon_test_id"))
      (aff-test-pairs
       aff-make-amazon-link
       ('("006097625X" "com")  . "https://www.amazon.com/gp/product/006097625X/?tag=amazon_test_id")
       ('("0130305529" "com")  . "https://www.amazon.com/gp/product/0130305529/?tag=amazon_test_id")
       ('("1435712757" "com")  . "https://www.amazon.com/gp/product/1435712757/?tag=amazon_test_id")
       ('("B004T91X0E" "com")  . "https://www.amazon.com/gp/product/B004T91X0E/?tag=amazon_test_id")
       ('("B00746LVOM" "com")  . "https://www.amazon.com/gp/product/B00746LVOM/?tag=amazon_test_id")
       ('("006097625X" "com")  . "https://www.amazon.com/gp/product/006097625X/?tag=amazon_test_id")))))

(ert-deftest aff-make-itunes-link ()
  "Tests whether function correctly creates an iTunes affiliate link."
  (flet
      ((aff-dissect-itunes-url (url) url))
    (let ((aff-itunes-id "itunes_test_id"))
      (aff-test-pairs
       aff-make-itunes-link
       ('("id284885288" "app" "us") . "https://itunes.apple.com/us/app/id284885288?partnerId=30&siteID=itunes_test_id")
       ('("id383804552" "app" "us") . "https://itunes.apple.com/us/app/id383804552?partnerId=30&siteID=itunes_test_id")
       ('("id505367096" "app" "us") . "https://itunes.apple.com/us/app/id505367096?partnerId=30&siteID=itunes_test_id")
       ('("id414575481" "album" "us") . "https://itunes.apple.com/us/album/id414575481?partnerId=30&siteID=itunes_test_id")))))

(ert-deftest aff-make-itunes-link-idempotent ()
  "Tests whether function has a fixed point that plays nice."
  (let ((aff-itunes-id "itunes_test_id"))
    (should
     (equal (aff-make-itunes-link (aff-make-itunes-link "https://itunes.apple.com/us/app/id383804552?partnerId=30&siteID=itunes_test_id"))
            "https://itunes.apple.com/us/app/id383804552?partnerId=30&siteID=itunes_test_id"))))

(ert-deftest aff-transform-url ()
  "Tests that the function plays nice when the functions it calls play nice."
  (let ((aff-itunes-id "itunes_test_id")
        (aff-amazon-id "amazon_test_id"))
    (aff-test-pairs
     aff-transform-url
     ("http://www.amazon.com/Metabarons-Ultimate-Collection-Alexandro-Jodorowsky/dp/1594650640" .
      (aff-make-amazon-link "http://www.amazon.com/Metabarons-Ultimate-Collection-Alexandro-Jodorowsky/dp/1594650640"))
     ("https://itunes.apple.com/us/app/id284885288?partnerId=30&siteID=itunes_test_id" .
      (aff-make-itunes-link "https://itunes.apple.com/us/app/id284885288?partnerId=30&siteID=itunes_test_id")))))
