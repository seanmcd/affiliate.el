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
  "Whether or not `affiliate.el' warns you about unset affiliate IDs.

When nil, no messages about missing affiliate IDs will be emitted
- this may be desireable after initial setup if you're not a
member of all of the supported affiliate programs."
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
  (let ((merchant (or merchant (aff-guess-merchant url))))
    (cond
     ((eq merchant 'amazon) (aff-make-amazon-link url))
     ((eq merchant 'itunes) (aff-make-itunes-link url))
     ((eq merchant 'apple-appstore (aff-make-apple-appstore-link url)))
     (t (when aff-verbosity
          (message "Can't match the URL [%s] to a known affiliate program." url))
        url))))

(defun aff-replace-urls-in-region (start end)
  ;; go through the region and replace every link you can with an affiliate
  ;; version of itself.

  ;; Attempts to find *all* URLs as future-proofing. Deciding whether an URL is
  ;; something we care about or not, is for other functions.

  ;; only matches domain.tld/stuff/ URLs because every affiliate program I know
  ;; of puts the code in the /stuff part instead of the domain.tld part.
  (interactive "r")
  (save-excursion
    (goto-char start)
    (while (< (point) end)
      (search-forward-regexp
       ;; Adapted from http://daringfireball.net/2010/07/improved_regex_for_matching_urls
       (concat "\\b\\(" ;; start on word boundary, begin group for whole URL
               "\\(?:https?://\\)?" ;; Optional protocol specifier
               "\\(?:www[[:digit:]]\\{0,3\\}\\.\\)?" ;; "www.", "www2.", etc.
               "[a-zA-Z0-9.-]+\\.[a-z]\\{2,4\\}" ;; won't match ".museum" TLDs. shrug.
               ;; (?:                       # One or more:
               "/\\(?:"
               ;;   [^\s()<>]+                  # Run of non-space, non-()<>
               "[^[:space:]()<>]+"
               ;;   |                      #   or balanced parens, up to 2 levels
               "\\|"
               ;;   \( # literal open paren A(
               "("
               ;;     ([^\s()<>]+
               "\\(?:[^[:space:]()<>]+" ;; a path segment that starts with a (, or
               ;;     |
               "\\|"
               ;;       (
               "\\("
               ;;         \([^\s()<>]+\) # literal open paren B(, literal close paren B)
               "([^[:space:]()<>]+)"
               ;;       )
               "\\)"
               ;;     )*
               "\\)*"
               ;;   \) # literal close paren A)
               ")"
               ;; )+
               "\\)+"
               ;; (?:                       # End with:
               "\\(?:"
               ;;   \(([^\s()<>]+|
               "(\\(?:[^[:space:]()<>]+\\|"
               ;; (\([^\s()<>]+\)
               "\\(?:([^[:space:]()<>]+)"
               ;; ))*\)  # balanced parens, up to 2 levels
               "\\)\\)*)"
               ;;   |                               #   or
               "\\|"
               ;;   [^\s`!()\[\]{};:'".,<>?«»“”‘’]        # not a space or one of these punct chars
               "[^][[:space:]`!(){};:'\".,<>?«»“”‘’]"
               ;; )
               "\\)")
       end 'move-to-end) ;; failing the search should break us out of the while loop.
      (when (match-string 0)
        (let ((new-url (aff-transform-url (match-string 1))))
          (replace-match new-url)
          new-url)))))

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
     "^\\(https?://\\)?\\((www\\.\\)?amazon\\.\\(com\\|ca\\|cn\\|fr\\|de\\|it\\|co\\.jp\\|es\\|co\\.uk\\)/"
     url) 'amazon)
   ((string-match
     "^\\(\\(https?\\|itms\\|itms-apps\\)://\\)?\\((itunes\\|phobos\\)\\.apple\\.com/[a-z]\\{2\\}/"
     url) 'itunes)
   ((string-match
     "^\\(https?://\\)?\\(www\\.\\)?appstore\\.com/"
     url) 'apple-appstore)
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
     "https://www.amazon.%s/dp/%s/"
     country-tld asin)))

(defun aff-make-itunes-link (url)
  "Turn iTunes Store link URL into an affiliate-linked URL."
  (multiple-value-bind
      (content-id content-type country-id)
      (aff-dissect-itunes-url url)
    ;; example: http://itunes.apple.com/us/app/omnifocus-for-iphone/id284885288?mt=8
    (format
     "https://itunes.apple.com/%s/%s/%s?partnerId=30&siteID=%s"
     country-id content-type content-id aff-itunes-id)))

(defun aff-make-apple-appstore-link (url)
  (error "Not yet implemented!"))


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
  (string-match
   ;; Bug: won't match the http://amzn.com/$ASIN style of URL.
   (concat
    "^\\(?:https?://\\)?\\((?:www\\.\\)?amazon\\.\\(com\\|ca\\|cn\\|fr\\|de\\|it\\|co\\.jp\\|es\\|co\\.uk\\)/"
    "\\(?:exec/obidos/tg/detail/-/\\|" ;; Amazon has used several
    "o/ASIN/\\|"                       ;; URL schemes over time.
    "[dg]p/\\(?:product/\\)?\\|"
    "[a-zA-Z0-9][^/]+/dp/\\)"
    "\\([A-Z0-9]\\{10\\}\\)"           ;; The actual ASIN.
    "/?.*$"))                          ;; Trailing query-string gunk
  (if (match-string 0)
      (let ((country-tld (match-string 1))
            (asin (match-string 2)))
        (list asin country-tld))
    (when aff-verbosity
      (message "Couldn't process [%s] as an Amazon URL." url))))

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
  (string-match
   (concat "^\\(?:\\(https?\\|itms\\|itms-apps\\)://\\)?\\((?:itunes\\|phobos\\)\\.apple\\.com/"
           "\\([a-z]\\{2\\}\\)/" ;; country code
           "\\([a-z]+\\)/" ;; content type e.g. album, app, artist
           "[^/]+/" ;; human-readable content name
           "\\(id[[:digit:]]+\\)" ;; content identifier
           "\\(.*?$\\)") ;; Possible trailing query-string gunk
   url)
  (if (match-string 0)
      (let* ((content-id (match-string 3))
             (content-type (match-string 2))
             (country-id (match-string 1))
             (detritus (match-string 4)))
        (when aff-verbosity
          (message "Found trailing detritus in query string: [%s]" detritus))
        (list content-id content-type country-id))
    (when aff-verbosity
      (message "Couldn't process [%s] as an iTunes Store URL." url))))

(defun aff-dissect-apple-appstore-url (url)
  (error "Not yet implemented!"))


(provide 'affiliate)
