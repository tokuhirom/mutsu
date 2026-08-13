# Unquoted dynamic method names now require a Callable

Mutsu now distinguishes the general string-name `\.""` method operator from
an unquoted `.$name` call. The quoted form continues to resolve a method name
from a string at runtime. The unquoted form invokes the name value as a
Callable, passing the receiver as its first argument, while type objects still
select a method by their short type name.

Previously both source forms compiled identically, so a bare string such as
`my $name = "uc"; "hi".$name()` silently called `.uc`. It now raises
`X::Method::NotFound` for `CALL-ME`, matching Rakudo. The parser, AST, compiler,
and dynamic method opcodes preserve the quoted distinction through VM dispatch,
with regression coverage for strings, routines, `CALL-ME` objects, type
objects, and method-call modifiers.
