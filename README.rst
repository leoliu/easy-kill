=============================
 Kill Things Easily in Emacs
=============================
 
Provide commands ``easy-kill`` and ``easy-mark`` to let users kill or
mark things easily.

easy-kill
~~~~~~~~~

``easy-kill`` is a drop-in replacement for ``kill-ring-save``. To Use:
::

   (global-set-key [remap kill-ring-save] 'easy-kill)

After this configuration, ``M-w`` serves as both a command and a
prefix key for other commands. ``M-w`` alone saves in the order of
active region, url, email and finally current line. As a prefix key:

#. ``M-w w``: save word at point
#. ``M-w s``: save sexp at point
#. ``M-w f``: save file at point
#. ``M-w l``: save list at point
#. ``M-w d``: save defun at point
#. ``M-w D``: save defun-name; works even when in a diff hunk
#. ``M-w b``: save ``buffer-file-name`` or ``default-directory``

The following keys modify the selection:

#. ``@``: append selection to previous kill and exit
#. ``C-w``: kill selection and exit
#. ``+``, ``-`` and ``0..9``: expand/shrink selection
#. ``C-SPC``: turn selection into an active region
#. ``C-g``: abort

See ``M-w l`` (save list at point) in action in `screenshot
<http://i.imgur.com/8TNgPly.png>`_:

.. figure:: http://i.imgur.com/8TNgPly.png
   :target: http://i.imgur.com/8TNgPly.png
   :alt: ``M-w l``

easy-mark
~~~~~~~~~

``easy-mark`` is similar to ``easy-kill`` but marks the region
immediately. It can be a handy replacement for ``mark-sexp`` allowing
``+``/``-`` to do list-wise expanding/shrinking and marks the whole
sexp even when in the middle of one. ::

   (global-set-key [remap mark-sexp] 'easy-mark)

Install
~~~~~~~

``easy-kill`` is available on `MELPA
<http://melpa.milkbox.net/#/easy-kill>`_.
   
Extensions
~~~~~~~~~~

New things can be defined by following package ``thingatpt.el``'s
convention, or by defining new functions named like
``easy-kill-on-THING-NAME``. See ``easy-kill-on-buffer-file-name`` and
``easy-kill-on-url`` for examples.

Bugs
~~~~

https://github.com/leoliu/easy-kill/issues
