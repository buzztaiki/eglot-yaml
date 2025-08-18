# eglot-yaml

YAML Language Server protocol extention for [Eglot](https://github.com/joaotavora/eglot/).

## Installation
```lisp
(use-package eglot-yaml :ensure t
  :after (eglot)
  :vc (:url "https://github.com/buzztaiki/eglot-yaml.git")
  :init
  (add-to-list 'eglot-server-programs
               '((yaml-ts-mode yaml-mode) eglot-yaml-lsp-server "yaml-language-server" "--stdio")))
```

## Featuers

- `M-x eglot-yaml-select-schema`: Select a schema for the current buffer by schema name.
- `M-x eglot-yaml-set-schema`: Set a schema for the current buffer by schema uri.
- `M-x eglot-yaml-reset-schema`: Reset the schema for the current buffer.
- `M-x eglot-yaml-show-schema`: Show the current schema for the current buffer.

Schema association is persisted within current Emacs session, and is not saved to disk.

## License
GPL-3.0
