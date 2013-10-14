=============================
 Kill Things Easily in Emacs
=============================
 
Commands ``easy-kill`` and ``easy-mark`` let users kill/mark things at
point easily.

``easy-kill`` is a drop-in replacement for ``kill-ring-save``. It
tries in order:

#. current region if active
#. url at point (snarf char properties ``help-echo``, ``shr-url``,
   ``w3m-href-anchor`` etc.)
#. email at point
#. current line

Keys (customisable) immediately following ``easy-kill``:

#. ``w`` -> word at point
#. ``s`` -> sexp at point
#. ``f`` -> file at point
#. ``l`` -> list at point
#. ``d`` -> defun at point
#. ``b`` -> ``buffer-file-name`` or ``default-directory``
#. ``@`` -> append selection to previous kill
#. ``C-w`` -> kill selection
#. ``+``, ``-`` and ``0..9`` -> expand/shrink selection
#. ``SPC`` -> turn selection into an active region
#. ``C-g`` -> abort

The following `screenshot <http://i.imgur.com/8TNgPly.png>`_ shows
``M-w l`` in action:

.. figure:: http://i.imgur.com/8TNgPly.png
   :target: http://i.imgur.com/8TNgPly.png
   :alt: ``M-w l``

To Use
~~~~~~

::

   (require 'easy-kill)
   (global-set-key [remap kill-ring-save] 'easy-kill)

Extensions
~~~~~~~~~~

New things can be defined by following package ``thingatpt.el``'s
convention, or by defining new functions named like
``easy-kill-on-THING-NAME``. See ``easy-kill-on-buffer-file-name`` and
``easy-kill-on-url`` for examples.

Bugs
~~~~

https://github.com/leoliu/easy-kill/issues
