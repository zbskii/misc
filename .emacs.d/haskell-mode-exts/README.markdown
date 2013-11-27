Modular Emacs extensions to Haskell's editing and interactive modes
===================================================================

This project contains several completely independent (of
eachother and haskell-mode) Emacs modules which provide
additional helpful functionality to working with Haskell in
Emacs.

Each one of the modules contains a brief description of the
purpose and usage. Below is a brief overview of each of the
modules, for ease of searching.

Table of Contents
-----------------

1. haskell-align-imports
2. haskell-installed-packages.el
3. haskell-navigate-imports.el
4. haskell-sort-imports.el
5. inf-haskell-send-cmd.el

1. haskell-align-imports.el
---------------------------

Provides one main function with which one can align
up imports in a Haskell file.

2. haskell-installed-packages.el
--------------------------------

Provides a few functions with which one can access
the currently installed packages registered with ghc-pkg, read
the package descriptions, and all modules.

3. haskell-navigate-imports.el
------------------------------

Provides one function which jumps you to the first import group
in your Haskell file. Running it repeatedly cycles through groups
of import statements. Running it with a prefix argument takes you
back to where you were before you visited the import groups.

4. haskell-sort-imports.el
--------------------------

Provides one function which sorts the list of imports at the
point alphabetically. Works nicely with haskell-align-imports.

If the region is active it sorts the imports within the region.

5. inf-haskell-send-cmd.el
--------------------------

Send arbitrary commands to inferior Haskell without echoing what
is sent. Useful for controlling cabal and hlint from within
Emacs.
