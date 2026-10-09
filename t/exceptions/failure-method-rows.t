use Test;

# exception, handled, gist, raku, Str and Bool of a Failure are rows of the
# method table (ADR-11276 §9.51); the answers are the cascade's.

plan 14;

my $f = Failure.new("oops");
is $f.handled, False, 'a fresh Failure is not handled';
isa-ok $f.exception, X::AdHoc, '.exception is the wrapped exception';
is $f.exception.message, 'oops', 'the wrapped message';
is $f.raku, 'Failure.new("oops")', '.raku of an unhandled Failure';
is $f.gist, 'oops', '.gist of an unhandled Failure';
is $f.handled, False, '.gist, .raku and .exception do not defuse it';
throws-like { $f.Str }, X::AdHoc, '.Str throws the wrapped exception';

is $f.Bool, False, '.Bool is False';
is $f.handled, True, '.Bool defuses the Failure';
is $f.gist, '(HANDLED) oops', '.gist of a handled Failure';
like $f.raku, /'$f.Bool'/, '.raku of a handled Failure defuses its copy';

my $g = Failure.new("again");
is $g.defined, False, '.defined is False';
is $g.handled, True, '.defined defuses it';
is (try { $g.perl }), $g.raku, '.perl is .raku';
