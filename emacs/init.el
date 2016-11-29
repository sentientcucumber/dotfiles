;;; init.el -- A masterpiece in progress.

;; -*- coding: utf-8 -*-

;; Author: Michael Hunsinger <mike.hunsinger@gmail.com>
;; URL: https://www.github.com/shellhead/dotfiles

;;; Commentary:

;; This is an attempt to be as amazing as Mr. Lissner, the author of
;; the DOOM setup. If you haven't come across this yet, you should
;; check it out, it's nothing short of amazing,
;; https://github.com/hlissner/.emacs.d.

;; I've used Emacs quite a bit in the past, but anything that was
;; fruitful was often the result of beleagured-bashing on the keyboard
;; until I was able to coax Emacs to do what I wished or I found a
;; snippet of code somewhere snag. This is a more thoughtful attempt
;; at setting up Emacs for me, and hopefully learn a thing or two
;; about Emacs and functional programming along the way.

;; With that being said, when viewing the source, there may be an
;; abundance of comments. While this is mostly to jog my own memory of
;; how to do things, I believe it may also be useful to those who are
;; in the same situation as I am, and looking to learn more about
;; Emacs. Ideally, this should go into a blog or a more appropriate
;; medium, but knowing me, I'll spend an inordinate amount of time
;; setting the blog up than working on this.

;; `sin' is a from-the-ground-up configuration design that takes heavy
;; inspiration from DOOM and is largely driven by my own personal
;; needs and desire to play and learn Emacs. If you're looking for
;; something cleaner and not written by a total n00b, I suggest
;; checking DOOM, EOS (Emacs Operating System) or one of the many
;; other configuration setup out there. If you come across this and
;; take pity upon my terrible coding skills or somehow this
;; abomination suits your needs, you're more than welcome to use it or
;; submit a PR to address any issues you come across.

;;; Usage:

;; 1. Clone the repository to the standard `$HOME/emacs.d/' location.
;; 2. Run `cask' to install the dependencies.
;; 3. Startup Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

;;;; Setup

;; The eigth sin.

(package-initialize)
(require 'cask "$HOME/.cask/cask.el")
(cask-initialize)

(setq auto-save-default nil
      make-backup-files nil)

;;;; Vanity

;; This section is devoted to altering Emacs' appearance to make it
;; look amazing.

(setq inhibit-startup-screen t)

;; When using the GUI (which I exclusively use), it adds the standard
;; window decorations and a scroll bar. They're great if you're
;; getting used to Emacs, but I'm looking to replace their
;; functionality through other packages and keybindings.

(when window-system
  (scroll-bar-mode   -1)
  (tool-bar-mode     -1)
  (menu-bar-mode     -1)
  (blink-cursor-mode -1))

(use-package doom-themes
  :init
  (load-theme 'doom-one t))

;;;; Gluttony

;; This section is devoted to configuring the many Emacs' packages. If
;; you've ever looked through MELPA or Marmalade, you're aware of the
;; number of packages available, often going against the rule of "Do
;; One Thing Well".

;; All package configurations, with the exception of `evil' (which can
;; be found in "Envy"), can be found in this section.

(defvar sin/shackle-size 20
  "Set the size of `shackle'd popups.")

(defvar sin/shackle-alignment 'below
  "Set where `shackle'd popups appear.")

(use-package shackle
  ;; `shackle' is great when trying to tame annoying popup windows or
  ;; configuring consistent behavior across all windows.
  :preface
  (setq shackle-default-size      sin/shackle-size
	shackle-default-alignment sin/shackle-alignment
	shackle-rules             '(("*Help*"     :select t)
				    ("*Messages*" :ignore t)
				    ("*Warnings*" :select t)))
  :init
  (shackle-mode))

;;;; Envy

;; This section is devoted to making Emacs act more like it's nemises,
;; Vi(m), using the popular package `evil'. I've only sat on the
;; sidelines whenever an editor has broken out, I never used Vi enough
;; to offer any valid opinion. I was attracted to it after my thumb
;; started to bother me after long days of typing and doing
;; finger-gymnastics in Emacs. At the time of this writing (12/2016),
;; I've used it for about six months now and love it. However, I do
;; hope to strike a balance between modal and linear editing.

(defvar sin/mode-start-states
  '((special-mode . motion))
  "An associative list of modes and their starting state.")

(defun sin/init-start-states! (c)
  "Initialize the starting states for the list of modes."
  (evil-set-initial-state (car c) (cdr c)))

(use-package evil
  :preface
  (setq evil-echo-state nil
	;; tags, by default, show up on the `mode-line' to indicate
	;; the current state
	evil-normal-state-tag    "N"
        evil-insert-state-tag    "I"
        evil-visual-state-tag    "V"
        evil-emacs-state-tag     "E"
        evil-operator-state-tag  "O"
        evil-motion-state-tag    "M"
	evil-replace-state-tag   "R"
	;; change the cursor depending on the `evil' state we're in
	evil-default-cursor      (face-attribute 'cursor :background nil t)
	evil-normal-state-cursor 'box
	evil-insert-state-cursor 'bar
	evil-visual-state-cursor '(hbar . 2))
  :diminish
  undo-tree-mode
  :bind
  (:map evil-normal-state-map
	("d" . evil-backward-char)
	("n" . evil-forward-char)
	("h" . evil-next-visual-line)
	("t" . evil-previous-visual-line)
	("j" . evil-find-char-to)
	("k" . evil-delete)
	("K" . evil-delete-line)
	:map evil-visual-state-map
	("d" . evil-backward-char)
	("n" . evil-forward-char)
	("h" . evil-next-visual-line)
	("t" . evil-previous-visual-line)
	("j" . evil-find-char-to)
	("k" . evil-delete)
	:map evil-motion-state-map
	("d" . evil-backward-char)
	("n" . evil-forward-char)
	("h" . evil-next-visual-line)
	("t" . evil-previous-visual-line)
	("j" . evil-find-char-to)
	("k" . evil-delete)
	("l" . evil-search-next)
	("L" . evil-search-previous))
  :init
  (evil-mode 1)
  :config
  (mapc 'sin/init-start-states! sin/mode-start-states))

;;;; Sloth

;; This section is devoted for things that I've come across that I may
;; want to implement in the future, but don't either don't have the
;; time, knowledge, or willpower to do. These may be half-implemented
;; or commented functions or `TODO' items.

;; TODO - DOOM warns `cask-initialize' can greatly impact load times,
;;   look at changing this when necessary.

;; TODO - I suck at spelling, get `flyspell' enabled. Since this
;;   requires one of the spelling command line binaries, create a
;;   Makefile to download this dependency.

;; TODO - The front page should accept a number of "widgets" (not an
;;   Emacs' widget). These widgets should be a way to display recent
;;   files, tips, etc. by reading a and displaying a list. This could
;;   be extendable to making API calls to GitHub/StackOverflow,
;;   etc. and displaying.

;; TODO - To ensure good elisp coding standards, get `flycheck' setup.

;; TODO - Working in elisp is hard to do without parenthesis help,
;;   setup `show-paren', `electric-paren' and `paredit'.

;; TODO - Document symbols ending in `!' have side effects and imply
;;   they are functions.

;; TODO - Create aliases that are found in Clojure, such as `number?'
;;   rather than elisp's `numberp'.
