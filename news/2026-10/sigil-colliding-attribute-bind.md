# Binding a private `%!x` no longer replaces a public `@.x`

With `has @.headers; has %!headers;` (one bare name, two sigils) a method that
binds the hash (`%!headers := %h`) replaced the public array when the method
returned: the exit reconcile matched the bound container to attributes by bare
name. The public array was then read back as the hash's pairs (Email::Simple:
"No such method 'lc' for invocant of type 'Pair'"; both of its test files now
match rakudo). The method-entry seeding of `@!x`/`%!x` locals also read the bare
key instead of the sigil-qualified key a colliding private declaration is stored
under. Both now honour the sigil.
