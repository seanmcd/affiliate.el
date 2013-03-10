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


(defun aff-transform-url (url &optional merchant)
  "Rewrites URL to include an affiliate ID.

If MERCHANT is provided, we trust that extra information and processes the URL
as a link to MERCHANT without examining it. Otherwise, we examine URL and
deduce MERCHANT from the domain name.

Returns the new version of URL if and only if we can positively identify which
merchant it matches and generate a well-formed affiliate link. Otherwise,
returns URL unchanged."
  (let ((merchant (or merchant (aff-guess-merchant url))))
    (cond
     ((eq merchant 'amazon) (aff-make-amazon-link url))
     ((eq merchant 'itunes) (aff-make-itunes-link url))
     ((eq merchant 'apple-appstore (aff-make-apple-appstore-link url)))
     (t (when aff-verbosity
          (message "Can't match the URL [%s] to a known affiliate program." url))
        url))))

(defun aff-replace-urls-in-region (start end &optional restrict-to-domains)
  ;; go through the region and replace every link you can with an affiliate
  ;; version of itself.
  (interactive)
  (save-excursion
    ;; crawl forward until you find a URL, tinker with it, then go to next, repeat.
    (error "Not yet implemented!")))

(defun aff-replace-urls-in-buffer (&optional target-buffer)
  ;; call aff-replace-urls-in-region but just use point-min and point-max for args.
  ;; if BUFFER is provided, do the fiddling-about in that buffer, otherwise, current.
  (save-excursion
    (when target-buffer
      (switch-to-buffer target-buffer))
    (aff-replace-urls-in-region (point-min) (point-max))))

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

(defun aff-canonicalize-url (url)
  ;; twiddle an URL into an unambiguous form so that we know what we're dealing with.
  (error "Not yet implemented!"))

(defun make-amazon-aff-link (url)
  (error "Not yet implemented!"))

(defun make-itunes-aff-link (url)
  (error "Not yet implemented!"))




(provide 'affiliate)
