#flycheck.el

This library adds D support to flycheck.

Requirements:
  * DMD 2.063 or later
  * [dash.el](https://github.com/magnars/dash.el)

To use this package, add the following line to your `.emacs` file:
```elisp
    (require 'flycheck-d)
```
It detects any compile errors, warnings and deprecated features.
And it also detects any errors during unit test.

