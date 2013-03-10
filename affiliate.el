;; init-affiliate.el

;; Currently only supports iTunes and Amazon affiliate links, but those are the
;; biggest anyhow, so that's fine to start with.

(defgroup affiliate nil
  "Affiliate link creation and editing."
  :prefix "aff-"
  :group 'external)

(defcustom aff-itunes-id
  ""
  "Affiliate ID to use for links to the iTunes Store."
  :type '(string
          :tag "iTunes Store Affiliate ID")
  :group 'affiliate)

(defcustom aff-amazon-id
  ""
  "Affiliate ID to use for links to Amazon."
  :type '(string
          :tag "iTunes Store Affiliate ID")
  :group 'affiliate)

(defcustom aff-verbosity
  t
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
  (interactive "r")
  (save-excursion
    ;; crawl forward until you find a URL, tinker with it, then go to next, repeat.
    (error "Not yet implemented!")))

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

(defun aff-make-amazon-link (url)
  (error "Not yet implemented!"))

(defun aff-make-itunes-link (url)
  "Turn iTunes Store link URL into an affiliate-linked URL.

URL must be something blessed by `aff-guess-merchant': otherwise
`aff-dissect-itunes-url' will return nil and this function's invocation of
`multiple-value-bind' will throw an error."
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
  "Docstring"
  (error "Not yet implemented!"))

(defun aff-dissect-apple-appstore-url (url)
  (error "Not yet implemented!"))

(defun aff-make-amazon-link (url)
  (error "Not yet implemented!"))

(defun aff-make-itunes-link (url)
  (error "Not yet implemented!"))

(defun aff-make-apple-appstore-link (url)
  (error "Not yet implemented!"))



(provide 'affiliate)
