How to use GNU stow:

Move things into the dotfiles, and stow them with:
stow <folder>

If I've added more to a folder in the dotfiles, restow them with:
stow -R <folder>

If I don't want that stuff anymore:
stow -D <folder>

Resources:
http://brandon.invergo.net/news/2012-05-26-using-gnu-stow-to-manage-your-dotfiles.html