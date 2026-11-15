# Wrapping an inherited built-in method reaches user classes

`Class.^find_method('clone')` now reports the built-in class that declares the
method. A wrap on that method reaches instances of user classes that inherit
it, including subclasses, while their own method overrides still win.
