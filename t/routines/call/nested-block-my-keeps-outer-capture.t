use Test;

# A `my` in a nested block that shadows an enclosing frame's lexical shadows
# it only for that block: the routine still reads and writes the outer
# variable everywhere else (#13391). A closure body does the same, and its
# exit writeback leaves the outer variable alone (#13396).

plan 13;

my $b = 99;
sub in-for { for 1 { my $b = 3 }; $b }
sub in-if  { if 1 { my $b = 3 }; $b }
sub before-and-after { my $before = $b; { my $b = 3 }; "$before/$b" }
sub outer-routine { my $q = 7; sub inner { { my $q = 3 }; $q }; inner() }

is in-for(), 99, 'after a for body that declares the name';
is in-if(), 99, 'after an if branch that declares the name';
is before-and-after(), '99/99', 'before and after a bare block that declares the name';
is outer-routine(), 7, 'a lexical of an enclosing routine';

sub writes { { my $b = 3 }; $b = 100; $b }
is writes(), 100, 'a write after the block reaches the outer variable';

sub inner-value { my $seen; { my $b = 3; $seen = $b }; $seen }
is inner-value(), 3, 'inside the block the name is the block\'s own variable';

# Closures (#13396).
{
    my $c = 99;
    my $f = { { my $c = 3 }; $c };
    is $f(), 99, 'a closure reads the outer variable after the nested block';
    is $c, 99, 'calling the closure does not clobber the outer variable';

    my $w = { { my $c = 3 }; $c = 100; $c };
    is $w(), 100, 'a closure write after the nested block returns the new value';
    is $c, 100, 'the closure write reaches the outer variable';

    my $p = -> $x { { my $c = $x }; $c };
    is $p(5), 100, 'a pointy block reads the outer variable after the nested block';

    my $seen;
    my $i = { { my $c = 3; $seen = $c }; $c };
    is "{$i()}/$seen", '100/3', 'inside the nested block the name is its own variable';

    my @r = (1..3).map({ { my $c = $_ }; $c });
    is @r.join(','), '100,100,100', 'a map block reads the outer variable';
}
