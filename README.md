#flycheck-d.el

This library adds D support to flycheck.

Requirements:
  * DMD 2.063 or later
  * [dash.el](https://github.com/magnars/dash.el)

To use this package, add the following line to your `.emacs` file:
```elisp
    (require 'flycheck-d)
```
It detects any compile errors, warnings and deprecated features.
And it also detects any errors during unittest.

Note: `flycheck-d` runs DMD with `-unittest` and `-main` option for unittesting.
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
