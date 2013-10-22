=============================
 Kill Things Easily in Emacs
=============================
 
Provide commands (``easy-kill``, ``easy-mark`` etc.) to let users kill
or mark things easily.

easy-kill
~~~~~~~~~

``easy-kill`` is a drop-in replacement for ``kill-ring-save``. It
saves something to ``kill-ring`` in this order:

#. current region if active
#. url at point (snarf char properties ``help-echo``, ``shr-url``,
   ``w3m-href-anchor`` etc.)
#. email at point
#. current line

Immediately following ``easy-kill``, the follow keys are temporarily
active:

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

Any other keys exit the temporary keymap and automatically save
selection to the ``kill-ring``. See ``M-w l`` (save list at point to
the kill ring) in action in `screenshot
<http://i.imgur.com/8TNgPly.png>`_:

.. figure:: http://i.imgur.com/8TNgPly.png
   :target: http://i.imgur.com/8TNgPly.png
   :alt: ``M-w l``

easy-mark
~~~~~~~~~

``easy-mark`` is similar to ``easy-kill`` but marks the region
immediately.

``easy-mark-sexp`` can be a handy replacement for ``mark-sexp``, which
allows +,=/- to do list-wise expanding/shrinking and marks the
whole sexp even when in the middle of one.

To Use
~~~~~~

``easy-kill`` is available on `MELPA
<http://melpa.milkbox.net/#/easy-kill>`_.

::

   (require 'easy-kill)
   (global-set-key [remap kill-ring-save] 'easy-kill)
   (global-set-key [remap mark-sexp] 'easy-mark-sexp)

Extensions
~~~~~~~~~~

New things can be defined by following package ``thingatpt.el``'s
convention, or by defining new functions named like
``easy-kill-on-THING-NAME``. See ``easy-kill-on-buffer-file-name`` and
``easy-kill-on-url`` for examples.

Bugs
~~~~

https://github.com/leoliu/easy-kill/issues
