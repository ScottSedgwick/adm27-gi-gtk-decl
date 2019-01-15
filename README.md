# Adm27 with gi-gtk-declarative in Haskell

Yet another ADM27 generator, this time using the GTK UI toolkit as a front end and the gi-gtk-declarative library to give an Elm architecture in Haskell with native controls.

# Configuring your development environment

You will need to install the pre-requisites for `haskell-gi`, instructions for which can be found here: https://github.com/haskell-gi/haskell-gi#installation

For MacOSX users, here is what you need:

```
brew install gobject-introspection gtk+ gtk+3
```
Ensure the path to libffi (probably /usr/local/opt/libffi/lib/pkgconfig) is in the PKG_CONFIG_PATH environment variable.