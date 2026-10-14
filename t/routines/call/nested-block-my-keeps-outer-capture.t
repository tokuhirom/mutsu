use Test;

# A `my` in a nested block that shadows an enclosing frame's lexical shadows
# it only for that block: the routine (or closure) still reads and writes the
# outer variable everywhere else.

plan 7;

my $b = 99;
sub in-for { for 1 { my $b = 3 }; $b }
sub in-if  { if 1 { my $b = 3 }; $b }
sub before-and-after { my $before = $b; { my $b = 3 }; "$before/$b" }
my &closure = { { my $b = 3 }; $b };
sub outer-routine { my $q = 7; sub inner { { my $q = 3 }; $q }; inner() }

is in-for(), 99, 'after a for body that declares the name';
is in-if(), 99, 'after an if branch that declares the name';
is before-and-after(), '99/99', 'before and after a bare block that declares the name';
is closure(), 99, 'in a closure';
is outer-routine(), 7, 'a lexical of an enclosing routine';

sub writes { { my $b = 3 }; $b = 100; $b }
is writes(), 100, 'a write after the block reaches the outer variable';

sub inner-value { my $seen; { my $b = 3; $seen = $b }; $seen }
is inner-value(), 3, 'inside the block the name is the block\'s own variable';
