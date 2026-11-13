use Test;

# `andthen` / `orelse` call their right-hand side with the topic only when it
# is a block literal (bare, pointy or placeholder block, possibly
# parenthesized). Any other expression -- including one that evaluates to a
# Callable, such as a hash lookup holding a sub -- is the result as is.

plan 8;

my %h = code => sub (:$node) { "plain $node" };

is (%h andthen .<code>).^name, 'Sub', 'andthen returns a Sub-valued RHS without calling it';
if %h andthen .<code> -> &cb {
    is cb(:node(1)), 'plain 1', 'the returned Sub binds to a pointy block';
}
my &s = sub ($x) { "called $x" };
is (5 andthen &s).^name, 'Sub', 'a &var RHS is not called';
is (5 andthen sub ($x) { "sub $x" }).^name, 'Sub', 'a sub literal RHS is not called';
is (5 andthen -> $x { "pointy $x" }), 'pointy 5', 'a pointy block RHS is called with the topic';
is (5 andthen { "block $_" }), 'block 5', 'a bare block RHS sees the topic';
is (5 andthen ({ "paren $_" })), 'paren 5', 'a parenthesized block RHS is called';
is (Nil orelse -> $x { "orelse " ~ $x.^name }), 'orelse Nil', 'orelse calls a pointy block too';
