
Affiliate.el
============

`affiliate.el` is a collection of emacs functions for creating and manipulating
links in bodies of text. I created it so that I would be able to convert all
links in my Octopress blog entries to affiliate links automatically.

Currently, this package only supports tinkering with affiliate links to the
iTunes Store and to Amazon. They're the two biggest affiliate programs. If I
join other affiliate programs or get pull requests implementing them, those
will show up here.

Usage
=====

* The easiest way to add `affiliate.el` to your emacs is to download it to your
  .emacs.d directory and then `(require 'affiliate)`.

* You must provide your affiliate ID via the `aff-itunes-id` and/or
  `aff-amazon-id` variables for the package to work. The easiest way to do this
  is `M-x customize-group RET affiliate`: they're both in that group. If either
  one is left as an empty string, warnings about it will be sent to
  `*Messages*` (they can be silenced by toggling the `aff-verbosity` variable).

* The minimum-effort way to operate this package is to add
  `aff-replace-urls-in-buffer` to the `before-save-hook` hooks. You'll probably
  want to restrict it to specific modes. Example:
  ```elisp
  (add-hook 'before-save-hook
     (lambda ()
       (when (member major-mode '(markdown-mode text-mode))
         (aff-replace-urls-in-buffer))))
  ```

License
=======

`affiliate.el` is released under the 3-clause BSD license. I care about
licenses and intellectual property rights, but trying to enforce them is a
Sisyphean task in the best of times, and certainly not worth it for this
particular package: hence the BSD license, which is permissive and easy to
understand.

It would bring me personal gratification to hear that this code helped you. :)
