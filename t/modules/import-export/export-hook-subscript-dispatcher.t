use Test;

# A `sub EXPORT` that returns a dispatcher derived from `postcircumfix:<{ }>`
# overrides the `{ }` subscript in the block that `use`s it, and only there
# (#15371). Checked against rakudo.

plan 3;

use lib $?FILE.IO.parent(2).sibling('lib').Str;

{
    use HookSubscriptDispatcher;
    is SubscriptFoo.new{'x'}, 'win x', 'the added candidate takes the subscript';
    is { a => 1 }<a>, 1, 'the built-in takes the rest';
}
is { b => 2 }<b>, 2, 'outside the block the subscript is the built-in';
