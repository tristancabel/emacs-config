This is my current emacs-config

# Dependencies

The current dependencies are: *libatomic*, *clang*, *ncurses-compat-lib* (needed by cquery), and *cquery*.

To install them on fedora do: `sudo dnf install libatomic clang clang-libs ncurses-compat-lib`
Then install CQuery

## CQuery install
Do the following commands:

```
cd ~/Tools
git clone https://github.com/jacobdufault/cquery --single-branch --depth=1
cd cquery
git submodule update --init
./waf configure   # --variant=debug if you want to report issues.
./waf build       # --variant=debug . Yes, it is duplicated here
```

The executable will be at *build/release/bin/cquery*.

# Install
To install this emacs config, create a symbolic link from `~.emacs.d` to this directory:

```
cd ~
ln -s ~/Tools/emacs-config .emacs.d
```
