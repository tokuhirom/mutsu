# Module methods see compunit routines

A method now retains the package that lexically contained its declaration. Bare
routine calls consult that package before falling back through the method's
runtime package hierarchy and `GLOBAL`.

This lets a method on an explicitly qualified class, including a `GLOBAL::`
class or a method installed with `.^add_method`, call a private routine from the
module where the method was written. The routine no longer needs to be exported
or explicitly qualified merely to remain visible to the method body.
