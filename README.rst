======================================================
 hsearch-mode — an Emacs major mode to search Haskell
======================================================

hsearch provides a native Emacs way to search Hoogle and Hayoo.

Installation
============

Place hsearch-mode.el on your `load-path` by adding this to your
`user-init-file`, usually ~/.emacs or ~/.emacs.d/init.el

    (add-to-list 'load-path "~/PATH_TO_HSEARCH")

Load the code:

    (autoload 'hsearch-mode "hsearch"
              "Major mode for searching Haskell." nil)

Screenshots
===========

.. image:: raw/master/hsearch-screenshot_2011-12-16.png

              
Overview
========

hsearch is roughly divided into three categories.  A generic model
based on EIEIO (Elisp implementation of CLOS), the Emacs display and
control functions and the adapters for Hoogle and Hayoo.

Generic Model
-------------

`hsearch-query` is the model associated with a query to Hoogle or
Hayoo.  It contains the raw query string, the request URL and a list
of results that match the query.

Each result is an `hsearch-result` class composed of a category, name,
signature, locations and a documentation string.  Each of these fields
is also a class.

Each class defined in hsearch implements the `hsearch-renderable`
interface to provide the `render` method.  To write the results to the
display buffer, we call `(render query)` and depend on each of the
fields within `query` to render themselves.  The `hsearch-query` is
the only class that inserts text into a buffer, allowing us
flexibility for any final tweaks to the output.

Emacs Display and Control Functions
-----------------------------------

`hsearch-mode` provides all commands for working with the `*hsearch*`
buffer.  

Hoogle Adapter
--------------

Currently, hsearch parses the HTML response from Hoogle to build an
`hsearch-query`.  The code makes many assumptions and is fragile.
There is hope that Hoogle will at some point provide a JSON response.

Hayoo Adapter
-------------

Yet to be written.




