====================
 Kill Things Easily
====================

Provide a more powerful command ``easy-kill`` for key ``M-w``.

``M-w`` tries in order:

#. current region if active
#. url at point
#. email at point
#. current line

Keys (customisable) immediately following ``M-w``:

#. ``w`` -> word at point
#. ``s`` -> sexp at point
#. ``f`` -> file at point
#. ``l`` -> list at point
#. ``d`` -> defun at point

More features are planned.

To Use
~~~~~~

::

   (require 'easy-kill)
   (global-set-key "\M-w" 'easy-kill)
