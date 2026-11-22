use Test;

# Failure's own methods are rows of the method table (ADR-11276), with the
# answers of Rakudo's src/core.c/Failure.rakumod: a value-producing method
# throws an unhandled Failure (marking it handled), and answers a defused
# value once it is handled.

plan 35;

my $f = Failure.new("oops");
is $f.handled, False, 'a fresh Failure is not handled';
isa-ok $f.exception, X::AdHoc, '.exception is the wrapped exception';
is $f.exception.message, 'oops', 'the wrapped message';
like $f.raku, /^ 'Failure.new(' /, '.raku of an unhandled Failure';
is $f.handled, False, '.raku and .exception do not defuse it';
isa-ok $f.backtrace, Backtrace, '.backtrace is the Backtrace of the fail site';
like $f.mess, /^ "oops\n" /, '.mess of an unhandled Failure';
is $f.handled, False, '.backtrace and .mess do not defuse it';
throws-like { $f.gist }, X::AdHoc, '.gist throws an unhandled Failure';
is $f.handled, True, 'and the throw handles it';

my $s = Failure.new("str");
throws-like { $s.Str }, X::AdHoc, '.Str throws an unhandled Failure';
is $s.handled, True, '... handling it';
is $s.Str, $s.mess, '.Str of a handled Failure is its .mess';
like $s.gist, /^ "(HANDLED) str\n" /, '.gist of a handled Failure';

my $b = Failure.new("again");
is $b.Bool, False, '.Bool is False';
is $b.handled, True, '.Bool defuses the Failure';
like $b.raku, /'Failure'/, '.raku of a handled Failure';
is (try { $b.perl }), $b.raku, '.perl is .raku';

my $g = Failure.new("defined");
is $g.defined, False, '.defined is False';
is $g.handled, True, '.defined defuses it';

# Numeric and QuantHash coercions.
my $n = Failure.new("num");
throws-like { $n.Int }, X::AdHoc, '.Int throws an unhandled Failure';
is $n.handled, True, '... handling it';
ok $n.Int === Int, '.Int of a handled Failure is the Int type object';
ok $n.Num.isNaN, '.Num of a handled Failure is NaN';
ok $n.Numeric.isNaN, '.Numeric of a handled Failure is NaN';
my $q = Failure.new("set");
throws-like { $q.Set }, X::AdHoc, '.Set throws an unhandled Failure';
is $q.Set.elems, 1, '.Set of a handled Failure holds it';
isa-ok $q.Bag, Bag, '.Bag of a handled Failure';

# A subscript propagates the Failure; a call or store throws it.
my $p = Failure.new("pos");
ok $p.AT-POS(3) === $p, 'AT-POS is the Failure itself';
ok $p.AT-KEY('k') === $p, 'AT-KEY is the Failure itself';
is $p.handled, False, '... and does not defuse it';
ok $p.Failure === $p, '.Failure is the Failure itself';
throws-like { $p.CALL-ME(1) }, X::AdHoc, 'CALL-ME throws it';

# Any other method throws an unhandled Failure and handles it.
my $m = Failure.new("method");
throws-like { $m.lines }, X::AdHoc, 'an undeclared method throws an unhandled Failure';
is $m.handled, True, '... handling it';
