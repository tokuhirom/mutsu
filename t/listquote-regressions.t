use Test;

plan 6;

lives-ok { my $s = join |<< <a x y z> }, 'hyper-prefix slip listquote parses and runs';
ok [1,2,3].join<abc> ~~ Failure, '.join<abc> parses and returns Failure';

my @y = ({:a<d>, :b(2)}<a b c>);
# TODO: use eqv once @-variable assignment consistently produces Array (mutable=true).
ok @y == ["d", 2, Any], 'hash angle subscript fills missing key with Any';

throws-like { EVAL '<>' }, X::Obsolete, 'bare <> is obsolete';
throws-like { EVAL '<STDIN>' }, X::Obsolete, '<STDIN> is obsolete';
throws-like { EVAL ':foo <3' }, X::Multi::NoMatch, ':foo <3 dies with X::Multi::NoMatch';
