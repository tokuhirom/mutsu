use Test;

# Two parser fixes:
#  1. A trait with a parenthesized argument between an enum name and its value
#     list (`enum E is export(:traits) <a b c>`) must be consumed, otherwise the
#     `(:traits)` is mistaken for the enum's `(...)` body and the `<values>` are
#     dropped.
#  2. A call to a sub whose name carries a `:<...>` adverbial part in a category
#     other than infix/prefix/postfix — notably `trait_mod:<of>(...)` — parses
#     as a function call.

plan 6;

our enum Precedence is export(:traits) <Skip Depth Name Stat>;
is Skip.value,  0, 'enum with is export(:traits) keeps its first value';
is Stat.value,  3, 'enum with is export(:traits) keeps later values';
is Depth.Int,   1, 'enum value coerces to its ordinal';

enum Plain is rw <Red Green Blue>;
is Green.value, 1, 'enum with a no-arg trait still works';

# trait_mod:<...> call syntax (definition already worked; the *call* did not).
sub trait_mod:<of>($routine, $type) { "of:$type" }
is trait_mod:<of>('r', 'MyType'), 'of:MyType', 'trait_mod:<of>(...) parses and dispatches as a call';

# The operator-name call forms keep working.
is infix:<+>(2, 3), 5, 'infix:<+>(...) still works';
