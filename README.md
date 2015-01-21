# Shellhead's dotfiles
Miscellaneous dotfiles. A masterpiece in progress. A big thanks to Lee
(dakrone) for helping me out with emacs and zsh on numerous occasions.

### Font
I use [Adobe's Source Code Pro]
(http://sourceforge.net/projects/sourcecodepro.adobe/). It's the best thing
since slice bread.

### Stow
I use stow to try and keep things organized and synchronized between my system
and what I share here. Stow allows me to move all the miscellaneous dotfiles
into one area. For instance, my emacs init file resides in
`~/dotfiles/emacs/.emacs.d/init.el` and a symbolic link resides in
`~/.emacs.d/init.el`. Emacs is happy because it can find the init.el file and
I'm happy because I can find all my dotfiles in one place.

Here's an example of stowing `.vim` and `.viminfo` (assuming they're in the
home directory) into `dotfiles/vim.` I'll be doing one at a time so I can show
when to use the restow option. At the end, I'll use the delete option. Upon
deletion, the symbolic link in the home directory will be blown away.
```
~/dotfiles  mkdir vim
~/dotfiles  mv ../.vim vim
~/dotfiles  stow vim
~/dotfiles  mv ../.viminfo vim
~/dotfiles  stow -R vim
~/dotfiles  stow -D vim
```

### ZSH
Fairly basic zsh prompt, used some code found at [Phil!'s ZSH Prompt]
(http://aperiodic.net/phil/prompt/) to get the horizontal line working.