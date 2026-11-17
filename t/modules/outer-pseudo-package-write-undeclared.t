use Test;

# `OUTER::` names exactly one scope. When that scope does not declare the
# name, `$OUTER::x` is Nil, so a write through it dies as a write to Nil does,
# instead of being silently dropped (#17839).

plan 8;

my $z = 3;
sub assigns() { my $b = { $OUTER::z = 9 }; $b() }
throws-like { assigns() }, X::Assignment::RO,
    message => 'Cannot modify an immutable Nil value',
    'an assignment through a $OUTER:: name the outer scope lacks dies';
is $z, 3, '... and does not reach a variable further out';

sub in-expression() { my $b = { ($OUTER::z = 9) }; $b() }
throws-like { in-expression() }, X::Assignment::RO, '... also as an expression';

sub increments() { my $b = { $OUTER::z++ }; $b() }
throws-like { increments() }, X::Multi::NoMatch, '$OUTER::z++ dies as Nil++ does';
sub pre-increments() { my $b = { ++$OUTER::z }; $b() }
throws-like { pre-increments() }, X::Multi::NoMatch, '... and ++$OUTER::z';

sub reads() { my $b = { $OUTER::z }; $b() }
is-deeply reads(), Nil, 'a read through it is Nil';

sub declared() { my $z = 1; my $b = { $OUTER::z = 9; $OUTER::z++ }; $b(); $z }
is declared(), 10, 'a $OUTER:: name the outer scope declares is written';

my $t = 1;
{ $OUTER::t = 5 }
is $t, 5, 'a bare block writes its outer scope\'s variable';
