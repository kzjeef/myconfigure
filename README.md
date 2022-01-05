myconfigure
===========

This repo is my emacs (mostly) config repo, 
the basic idea is when I have a new ubuntu / Mac machine, 
I can setup the development environment with just a script,
also keep multi dev box in same shape.

## emacs

which is is the (dot)emacs file.

## install.sh

which is the install script. it should be a Makefile, but just keep it simple.

## font 
which store some programming font.


## Enable CCLS in C++ project

1. in `dotspacemacs-configuration-layers` add `lsp` and :

```     (c-c++ :variables
           ; c-c++-enable-clang-support nil
            c-c++-backend 'lsp-ccls
            c-c++-enable-google-style t
            c-c++-default-mode-for-headers 'c++-mode)

```

2. in user-config: 
```  
(add-hook 'prog-mode-hook #'lsp)

;; and path to ccls.
 (cond (on_darwin
         (setq ccls-executable "/usr/local/bin/ccls")
         ))
 (require 'company-lsp)
  ;; fly check for ccls;
 (setq lsp-prefer-flymake nil)
 
 
 ;; font lock 
  (setq ccls-sem-highlight-method 'font-lock)

```

3. in project root folder.

create `.ccls` file, and `.ccls` file content should at leaset have 
```
%compile_commands.json
```

link json to root folder (.ccls) folder

4. flycheck.

by default, fly check use clang directly, and the ccls compile flag(eg, include path) cannot used by flycheck, therefore some include path error will be occurs.
to fix this issue, 
create a `.dir-locals.el` file under the project root folder.
and the content of this fly should like this, the incldue path list can be copy from compile_commands.json
```
((nil . ((flycheck-clang-language-standard . "gnu++14") ; c++ standard.
         (flycheck-clang-include-path . ("."          ; include path.
                                         "src"
                                         "util"
                                         "include"
                                         ))
         )
      ))
```


