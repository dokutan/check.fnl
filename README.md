# check.fnl
A linter for [Fennel](https://fennel-lang.org)

[![tests](https://github.com/dokutan/check.fnl/actions/workflows/test.yml/badge.svg)](https://github.com/dokutan/check.fnl/actions)
## Dependencies
- Fennel interpreter
- Fennel Lua module (fennel.lua)
- Lua interpreter:
  - Compiling check.fnl is only tested with Fennel on PUC Lua 5.4.
  - Running the built Lua file is possible with either luajit or PUC Lua, but:
  - Luajit (and Lua without the utf8 module) counts line lengths by bytes instead of unicode codepoints.

It is recommended that you install Fennel using your package manager, this should take care of all dependencies. If you don't have Fennel properly installed, you might need to download the Fennel Lua module and place it in the same directory as check.fnl.

## Usage
Clone this repository and run
```
make
./check.fnl file1.fnl file2.fnl …
```

A [configuration file](configuration.md) can be specified with the ``-c`` option. This allows you to change the enabled checks, as well as their behaviour.
```
./check.fnl -c config.fnl file1.fnl file2.fnl …
```

Use the ``-s`` option to show a list of all checks.

## Features
The analysed code is not executed, only parsed by the Fennel parser.

Currently check.fnl finds:
- deprecated forms
- bad style
  - wrong characters in names
  - lines that exceed 80 columns
  - wrong number of semicolons in comments
  - closing delimiters on their own line
  - missing docstrings
- useless forms
- ``if`` replacable with ``when``
- uses of ``not`` that can be replaced
- lists that don't begin with an identifier
- uses of ``local`` that can be replaced with ``let``
- syntax errors
  - ``let``
  - ``when``
  - ``if``
  - …
- redefined and shadowed symbols

Not all checks are enabled by default, and some are likely to produce false positives.

## Directives
A line with a comment containing ``no-check`` will suppress all errors for that line:
```
(+ 1) ; no-check
```

## References
- The Fennel reference: https://fennel-lang.org/reference
- The Fennel style guide: https://fennel-lang.org/style
- The official linter plugin: https://git.sr.ht/~technomancy/fennel/tree/main/item/src/linter.fnl
