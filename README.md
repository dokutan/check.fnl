# check.fnl
A linter for [Fennel](https://fennel-lang.org)

## Dependencies
- Fennel interpreter
- Lua interpreter (required by Fennel)
- Fennel Lua module (fennel.lua)

It is recommended that you install Fennel using your package manager, this should take care of all dependencies. If you don't have Fennel properly installed, you might need to download the Fennel Lua module and place it in the same directory as check.fnl.

## Usage
Download check.fnl and run
```
chmod +x check.fnl
./check.fnl file1.fnl file2.fnl …
```

## Features
Currently check.fnl finds:
- deprecated forms
- bad style
  - wrong characters in names
  - lines that exceed 80 columns
  - wrong number of semicolons in comments
  - closing delimiters on their own line
  - missing docstrings
- useless ``do`` forms
- ``if`` replacable with ``when``
- invalid ``let`` bindings

## References
- The Fennel reference: https://fennel-lang.org/reference
- The Fennel style guide: https://fennel-lang.org/style
- The official linter plugin: https://git.sr.ht/~technomancy/fennel/tree/main/item/src/linter.fnl
