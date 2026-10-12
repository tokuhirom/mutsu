use Test;

# A routine whose last statement is a `sub` declaration returns that Sub,
# whether it is called by name or through its code object (#13040). The
# two call paths (named call vs. closure dispatch) must agree.

plan 11;

my sub pw { sub tw($x) { $x } }
is pw()(5), 5, 'by name: trailing sub declaration is the value';
isa-ok &pw(), Sub, 'through &name: trailing sub declaration is a Sub';
is &pw()(6), 6, 'through &name: the returned Sub is callable';

my &c = &pw;
is c()(7), 7, 'through a code variable';

my sub typed(--> Code:D) { sub tt($x) { $x * 2 } }
is &typed()(4), 8, 'a --> Code:D return type accepts the declared Sub';

sub outer { my $a = 3; sub inner { $a } }
is &outer()(), 3, 'the returned Sub sees its declaring frame\'s lexicals';

my $blk = { sub bs($x) { "b$x" } };
is $blk()(1), 'b1', 'a bare block ending in a sub declaration';

my $p = -> { sub ps { 'p' } };
is $p()(), 'p', 'a pointy block ending in a sub declaration';

my sub q(--> Code:D) { sub r($x --> Int) { $x } }
nok (try &q()('x')).defined, 'the returned Sub checks its own return type';
is &q()(3), 3, 'the returned Sub returns a value of its own return type';

my sub plain { my $x = 1; $x + 1 }
is &plain(), 2, 'an ordinary last-statement value is unchanged';
