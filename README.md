smart-cursor-color-mode
============================

Change cursor-color dynamically on Emacs.

[iTerm2](http://www.iterm2.com/#/section/home) have `smart cursor color'.

But GNU Emacs deosn't have this option.

The `smart-cursor-color-mode' can make do that.

Quickstart
----------

To make the mode enabled every time Emacs starts, add the following

to Emacs initialisation file (~/.emacs or ~/.emacs.d/init.el):

If installed from [melpa](http://melpa.milkbox.net).

    (smart-cursor-color-mode +1)

If installed manually,

    (add-to-list 'load-path "path-to-installed-directory")
    (require 'smart-cursor-color)
    (smart-cursor-color-mode +1)
