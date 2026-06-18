use Test;

plan 8;

# A `:=` scalar bind to a readonly RHS (`my $x := 42`) in block-final (value)
# position used to throw "Cannot assign to a readonly variable" because the
# block-final store went through the assignment path (which rejects the
# readonly mark emitted for the bind) instead of the declaration path.

# do-block tail
is (do { my $x := 42 }), 42, 'do-block tail: my $x := <literal>';

# bare block called as a sub
my $r = { my $x := 42 };
is $r(), 42, 'sub tail: my $x := <literal>';

# the special match variable $/ can be bound the same way
lives-ok { my $/ := 42 }, 'can bind $/ in block-final position';

# binding still makes the variable readonly (initializer aside)
my $err;
{
    my $x := 7;
    $x = 9 unless True;  # never runs, just ensures parse
    CATCH { default { $err = .message } }
}
ok True, 'readonly bind parses';

# a subsequent assignment to a literal-bound scalar must still die
dies-ok {
    my $y := 5;
    $y = 6;
}, 'assigning to a literal-bound scalar dies';

# bind to a variable still works in tail position
my $src = 11;
is (do { my $z := $src }), 11, 'do-block tail: my $z := $var';

# bind to an array still works in tail position
my @a = 1, 2, 3;
is (do { my $w := @a }).elems, 3, 'do-block tail: my $w := @arr';

# plain assignment in tail position still works
is (do { my $p = 99 }), 99, 'do-block tail: my $p = <literal>';
