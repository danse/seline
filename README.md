
Seline is a small package providing something like a web `select`
input for command line scripts

## Usage

Here is an interpreter session showcasing the interface:

```
ghci> import qualified Seline
ghci> Seline.simple ["apples", "oranges", "grapes"]
grapes · 3,  oranges · 2,  apples · 1

1
grapes · 2,  oranges · 1
apples
2
oranges · 1
apples > grapes

Just ["grapes","apples"]
```

Users can type a number or a full item to select it, then seline
prints remaining choices. Typing an empty line causes seline to return
the whole selection

## Maybe

Seline returns a `Maybe [String]` because `Nothing` indicates that an
user pressed `Ctrl-D`. So application code can distinguish empty input
submission (no selection) from input termination (quitting the
interface)
