# Explicit method invocants remain in callable signatures

Lexical and package-scoped methods now retain an explicitly declared invocant in their callable form, including an anonymous typed invocant such as `Int:D:` and a named `$self:` invocant. Previously the shared registration helper removed a parameter named `self`, so calling the method through `.&` rejected a valid argument as surplus.
