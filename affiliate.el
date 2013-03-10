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

(defun aff-investigate-string ()
  (error "Not yet implemented!"))

(defun aff-find-urls (text-blob)
  (error "Not yet implemented!"))

(defun aff-transform-url (url &optional merchant)
  ;; attempt to guess store ID from domain name in URL
  "Generate a link for STORE with my affiliate code from URL.

If STORE is in the list of known stores, this function examines URL and returns
a string with a URL that identifies the same product, but with my affiliate
code attached. If STORE is not known, returns nil. If the function can't figure
out how to generate the right kind of URL to return, it should return the
original URL."
  (let ((merchant (or merchant (aff-guess-merchant url))))
    (cond
     ((string-equal merchant "amazon") (make-amazon-aff-link url))
     ((string-equal merchant "itunes") (make-itunes-aff-link url))
     (t (error "Don't know a store named '%s'." (pp-to-string store))))))

(defun aff-guess-merchant (url)
  (error "Not yet implemented!"))

(defun aff-canonicalize-url (url)
  ;; twiddle an URL into an unambiguous form so that we know what we're dealing with.
  (error "Not yet implemented!"))

(defun make-amazon-aff-link (url)
  (error "Not yet implemented!"))

(defun make-itunes-aff-link (url)
  (error "Not yet implemented!"))




(provide 'affiliate)
