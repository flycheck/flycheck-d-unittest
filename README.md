## For Flycheck-d users
Now D syntax checker is a part of Flycheck!
This library only provides D unittest checker.

# Flycheck-d-unittest

This library adds D unittest support to flycheck.

Requirements:
  * DMD 2.063 or later
  * [flycheck.el](https://github.com/flycheck/flycheck)
  * [dash.el](https://github.com/magnars/dash.el)

To use this package, add the following line to your `.emacs` file:
```elisp
    (require 'flycheck-d-unittest)
    (setup-flycheck-d-unittest)
```
It detects any compile errors, warnings and deprecated features during unittest.

Note: Flycheck-d-unittest runs DMD with `-unittest` and `-main` option for unittesting.
Please enclose main function in `version(!unittest)` block as follows:

```d
import std.stdio;

version(unittest) {}
else
void main()
{
    writeln("Hello!");
}

unittest
{
    assert(1+2 == 3);
}
```

Link:
  * [Start D with Emacs](https://github.com/tom-tan/flycheck-d/wiki/Start-D-with-Emacs)
  * [Emacs で始める D言語!](http://qiita.com/tm_tn/items/1d01c4500e1ca7632140) (Written in Japanese)
