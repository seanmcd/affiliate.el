;; -*- coding: utf-8 -*-
;; init-affiliate.el

;; Configuration variables.
(defgroup affiliate nil
  "Affiliate link creation and editing."
  :prefix "aff-" :group 'external)

(defcustom aff-amazon-id ""
  "Affiliate ID to use for links to Amazon."
  :type '(string :tag "Amazon Affiliate ID")
  :group 'affiliate)

(defcustom aff-itunes-id ""
  "Affiliate ID to use for links to the iTunes Store."
  :type '(string :tag "iTunes Store Affiliate ID")
  :group 'affiliate)

(defcustom aff-verbosity t
  "Whether or not to emit messages about operation.

When non-nil, will emit grumblings to the *Messages buffer about unset
affiliate IDs, URLs that can't be matched to an affiliate program, URLs that
already contain an affiliate ID, etc. After initial setup, you may went to
change this to nil if you're not a member of all of the supported affiliate
programs."
  :type 'boolean
  :group 'affiliate)


;; Interactive functions; user-facing, stable.
(defun aff-transform-url (url &optional merchant)
  "Rewrites URL to include an affiliate ID.

If MERCHANT is provided, we trust that extra information and processes the URL
as a link to MERCHANT without examining it. Otherwise, we examine URL and
deduce MERCHANT from the domain name.

Returns the new version of URL if and only if we can positively identify which
merchant it matches and generate a well-formed affiliate link. Otherwise,
returns URL unchanged."
  (interactive "sURL to add affiliate code to: ")
  (let* ((merchant (or merchant (aff-guess-merchant url)))
         (new-url
          (cond
           ((eq merchant 'amazon) (aff-make-amazon-link url))
           ((eq merchant 'itunes) (aff-make-itunes-link url))
           ((eq merchant 'apple-appstore)
            (aff-make-itunes-link (aff-dissect-apple-appstore-link url)))
           ((eq merchant 'amazon-shorturl)
            (aff-make-amazon-link (aff-dissect-amazon-shorturl-link url)))
           (t (when aff-verbosity
                (message "Can't match the URL [%s] to a known affiliate program." url))
              url))))
    (when (called-interactively-p 'any)
      (message "New url: [%s]." new-url))
    new-url))

(defun aff-replace-urls-in-region (start end)
  "Scans for URLs between START and END, sends them to `aff-transform-url'.

Called interactively, operates on the region. Called from lisp, operates on the
region given by its arguments. Returns a list of URLs found in the given
region, and as a side effect, does any affiliate-code-adding that it
can. Intended to operate idempotently - if you run this twice in a row on the
same block of text and the second run changes something the first didn't, there
is a bug."
  (interactive "r")
  (save-excursion
    (let ((found-url-list) '())
      (goto-char start)
      (while (< (point) end)
        (when (search-forward-regexp
               ;; only matches domain.tld/stuff/ URLs because every affiliate program I know
               ;; of puts the code in the /stuff part instead of the domain.tld part.
               (concat "\\b\\(" ;; start on word boundary, begin group for whole URL
                       "\\(?:https?://\\)?" ;; Optional protocol specifier
                       "\\(?:www[[:digit:]]\\{0,3\\}\\.\\)?"  ;; "www.", "www2.", etc.
                       "[a-zA-Z0-9.-]+\\.[a-z]\\{2,4\\}"      ;; won't match ".museum" TLDs. shrug.
                       "/[][a-zA-Z0-9-._~:/?#@!$)(*+,;=]+"    ;; match a run of normal URL characters
                       "[^][[:space:]`!(){};:'\".,<>?«»“”‘’]" ;; ending with something non-punctuation-y
                       "\\)"
                       ;; followed by something that's probably not part of an URL.
                       "\\(?:[][[:space:]`!(){};:'\".,<>?«»“”‘’]\\|\\b\\|$\\)")
               end 'move-to-limit) ;; failing the search breaks us out of the while loop and returns nil.
          (let* ((found-url (buffer-substring-no-properties (match-beginning 1) (match-end 1)))
                 (new-url (aff-transform-url found-url)))
            (replace-match new-url nil t nil 1)
            (setq found-url-list (cons found-url found-url-list)))))
      found-url-list)))

(defun aff-replace-urls-in-buffer (&optional target-buffer)
  ;; call aff-replace-urls-in-region but just use point-min and point-max for args.
  ;; if BUFFER is provided, do the fiddling-about in that buffer, otherwise, current.
  (interactive "bAdd affiliate code to URLs in this buffer: ")
  (save-excursion
    (when target-buffer
      (switch-to-buffer target-buffer))
    (aff-replace-urls-in-region (point-min) (point-max))))


;; Implementation details.
(defun aff-guess-merchant (url)
  "Looks at the start of URL, matches it to a merchant."
  (cond
   ((string-match
     "^\\(https?://\\)?\\(www\\.\\)?amazon\\.\\(com\\|ca\\|cn\\|fr\\|de\\|it\\|co\\.jp\\|es\\|co\\.uk\\)/"
     url) 'amazon)
   ((string-match
     "^\\(\\(https?\\|itms\\|itms-apps\\)://\\)?\\(itunes\\|phobos\\)\\.apple\\.com/[a-z]\\{2\\}/"
     url) 'itunes)
   ((string-match
     "^\\(https?://\\)?\\(www\\.\\)?appstore\\.com/"
     url) 'apple-appstore)
   ((string-match
     "^\\(https?://\\)?\\(www\\.\\)?amzn\\.\\(com\\|to\\)/"
     url) 'amazon-shorturl)
   (t 'unknown)))


;; The `aff-make-foo-link' family of functions *will* produce garbage results
;; if you feed them arbitrary strings instead of something that has been or
;; would be blessed by `aff-guess-merchant'.
(defun aff-make-amazon-link (url)
  "Turn Amazon link URL into affiliate-linked URL."
  (multiple-value-bind
      (asin country-tld)
      (aff-dissect-amazon-url url)
    (format
     "https://www.amazon.%s/gp/product/%s/?tag=%s"
     country-tld asin aff-amazon-id)))

(defun aff-make-itunes-link (url)
  "Turn iTunes Store link URL into an affiliate-linked URL."
  (multiple-value-bind
      (content-id content-type country-id)
      (aff-dissect-itunes-url url)
    ;; example: http://itunes.apple.com/us/app/omnifocus-for-iphone/id284885288?mt=8
    (format
     "https://itunes.apple.com/%s/%s/%s?partnerId=30&siteID=%s"
     country-id content-type content-id aff-itunes-id)))


(defun aff-dissect-amazon-url (url)
  "Retrieves the ASIN from URL.

Amazon links center around the Amazon Standard Identification Number - each
ASIN will identify exactly one product in a given country's Amazon store. ASINs
used to be globally unique for Amazon products, but now they're
per-country. For more on ASINs and Amazon's URL structure in general, see:

* http://en.wikipedia.org/wiki/Amazon_Standard_Identification_Number
* http://stackoverflow.com/q/1764605/244494
* https://sites.google.com/site/steveyegge2/saving-time

The current version of this package assumes that the country-TLD of whatever
URL it finds an ASIN in, is a reliable indicator of which country's Amazon
store that ASIN is intended to refer to and preserve the country TLD: i.e. when
this function is handed an amazon.co.jp URL, it will return an amazon.co.jp
URL, and when handed an amazon.com URL, will return an amazon.com URL."
  (if (string-match
       ;; Bug: won't match the http://amzn.com/$ASIN style of URL.
       (concat "^\\(?:https?://\\)?\\(?:www\\.\\)?amazon\\."
               "\\(com\\|ca\\|cn\\|fr\\|de\\|it\\|co\\.jp\\|es\\|co\\.uk\\)/"
               "\\(?:exec/obidos/tg/detail/-/\\|" ;; Amazon has used several
               "o/ASIN/\\|"                       ;; URL schemes over time.
               "[dg]p/\\(?:product/\\)?\\|"
               "[a-zA-Z0-9][^/]+/dp/\\)"
               "\\([A-Z0-9]\\{10\\}\\)"           ;; The actual ASIN.
               "/?.*$")                           ;; Trailing query-string gunk
       url)
      (let ((country-tld (match-string 1 url))
            (asin (match-string 2 url)))
        (list asin country-tld))
    (when aff-verbosity
      (message "Couldn't process [%s] as an Amazon URL." url))))

(defun aff-dissect-amazon-shorturl-url (url)
  (error "Not yet implemented!"))

(defun aff-dissect-itunes-url (url)
  "Retrives content, content-type, and country identifiers from URL.

Returns a list like:

  (content-id content-type country-id)

For more on the structure of iTunes Store URLs, see:

* http://stackoverflow.com/q/433907/244494
* https://developer.apple.com/library/ios/#qa/qa2008/qa1629.html
* https://developer.apple.com/library/ios/#qa/qa1633/_index.html
* http://itunes.apple.com/linkmaker/
* http://www.apple.com/itunes/affiliates/resources/

The US iTunes Store operates its affiliate program through Rakuten LinkShare:
other regions may operate through other affiliate programs. The iTunes-related
functions may later be split into region-specific functions to reflect this."
  (if (string-match
       (concat "^\\(?:\\(?:https?\\|itms\\|itms-apps\\)://\\)?"
               "\\(?:itunes\\|phobos\\)\\.apple\\.com/"
               "\\([a-z]\\{2\\}\\)/" ;; country code
               "\\([a-z]+\\)/" ;; content type e.g. album, app, artist
               "\\(?:[^/]+/\\)?" ;; human-readable content name, optional
               "\\(id[[:digit:]]+\\)" ;; content identifier
               "\\(.*?$\\)") ;; Possible trailing query-string gunk
       url)
      (let* ((content-id (match-string 3 url))
             (content-type (match-string 2 url))
             (country-id (match-string 1 url))
             (detritus (match-string 4 url)))
        (when aff-verbosity
          (message "Found trailing detritus in query string: [%s]" detritus))
        (list content-id content-type country-id))
    (when aff-verbosity
      (message "Couldn't process [%s] as an iTunes Store URL." url))))

(defun aff-dissect-apple-appstore-url (url)
  (error "Not yet implemented!"))


(provide 'affiliate)
