This is my current emacs-config

# Dependencies

The current dependencies are: *clang*, *libclang-dev*, *ccls*, *pyls*

for **pyls** do the following:
```
pip install 'python-language-server[all]'
pip3 install 'python-language-server[all]'
```

to enable python debugging with **dap-mode**, do the folowwing:
```
pip install "ptvsd>=4.2"
```

## ccls install
Do the following commands:

```
cd ~/Tools
git clone --depth=1 --recursive https://github.com/MaskRay/ccls/
cd ccls
cmake -H. -BRelease -DCMAKE_BUILD_TYPE=Release
cmake --build Release
```

The executable will be at *build/Release/ccls*, you shloud add it to your path in **~/.bashrc** file
`export PATH="$PATH:/home/trcabel/Tools/ccls/Release/"`

# Install
To install this emacs config, create a symbolic link from `~.emacs.d` to this directory:

```
cd ~
ln -s ~/Tools/emacs-config .emacs.d
```
