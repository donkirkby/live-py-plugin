## Emacs Development
Install the `live-py-mode` package as described in the README file, then replace all the files in
`~/.emacs.d/elpa/live-py-mode-X.Y` with symbolic links to your git repository.
You'll also need the [git download instructions] if you don't already have it.

[git download instructions]: https://git-scm.com/downloads

### Publish a new release for the Emacs package
Releases are built automatically on each commit, and published at [melpa.org].
Stable releases are built from each tag, but there are some helpful steps to
follow for each release.

1. Update the version number in `emacs-live-py-mode/live-py-mode.el`.
2. Click the README badge for Emacs downloads to see the current number, then
   update it in the badge's image URL.

[melpa.org]: https://melpa.org

### Debugging live-py-mode in Emacs
When adding temporary debugging messages with the `message` function, it is
recommended to open a second Emacs frame for the Messages buffer by typing
`C-x 5 2` in the Messages buffer.

Run the commands listed in `.github/workflows/python-package.yml` in the section
"Test with Emacs" manually to check for compiler warnings and test regressions.
