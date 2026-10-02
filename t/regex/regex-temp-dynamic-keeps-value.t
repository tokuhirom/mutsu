use Test;

# From JSON::Mask: `:temp @*stack;` in a grammar rule keeps the variable's
# current value (raku's temp only restores on scope exit) instead of
# rebinding it to a fresh empty container.

plan 4;

my @seen;
grammar G {
    token TOP { :my @*stack; <p> }
    token p { <r> }
    token r { <k> [ '(' [ :temp @*stack; { push @*stack, ~$<k> } <p> ] ')' || { @seen.push: @*stack.join('.') } ] }
    token k { \w+ }
}
G.parse('a(b(c))');
is @seen.elems, 1, 'leaf reached once';
is @seen[0], 'a.b', 'nested :temp @*stack accumulates across levels';

grammar H {
    token TOP { :my @*stack = 7; <r> }
    token r { 'x' [ :temp @*stack; { @seen.push: "r:" ~ @*stack.join(',') } <q> ] }
    token q { 'y' { @seen.push: "q:" ~ @*stack.join(',') } }
}
@seen = ();
H.parse('xy');
is-deeply @seen, ['r:7', 'q:7'], ':temp keeps the initialised value';

class X::P is Exception {
    has Cursor $.cursor is required;
    method message { 'bad' }
}
grammar Q { token TOP { a } }
is X::P.new(cursor => Q.new).cursor.^name, 'Q', 'a Cursor attribute accepts a grammar instance';
