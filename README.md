This is my current emacs-config

# Dependencies

The current dependencies are: *libatomic*, *clang*, *ncurses-compat-lib* (needed by cquery), *the_silver_searcher* (ag) and *cquery*.

To install them on fedora do: `sudo dnf install libatomic clang clang-libs ncurses-compat-lib the_silver_searcher libasan libubsan`
Then install CQuery

## CQuery install
Do the following commands:

```
cd ~/Tools
git clone https://github.com/jacobdufault/cquery --single-branch --depth=1
cd cquery
git submodule update --init
mkdir build
ccmake -DASAN:BOOL=ON -DASSERT:BOOL=ON -DREPROC_SANITIZERS:BOOL=ON ..
```

The executable will be at *build/release/bin/cquery*.

# Install
To install this emacs config, create a symbolic link from `~.emacs.d` to this directory:

```
cd ~
ln -s ~/Tools/emacs-config .emacs.d
```
