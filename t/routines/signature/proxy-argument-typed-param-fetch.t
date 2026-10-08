use Test;

# A Proxy argument is type-checked by what its FETCH answers, for plain and
# `is raw` parameters alike (Red hands a column's Proxy to
# `sub deflate(Str $password is raw)`; RedX::HashedPassword).
plan 6;

my $store = "abc";
my $p := Proxy.new(FETCH => method { $store }, STORE => method (\v) { $store = v });

sub plain(Str $s) { $s }
sub raw(Str $s is raw --> Str) { $s }
sub untyped($s) { $s }

is plain($p), "abc", 'typed parameter FETCHes a Proxy';
is raw($p), "abc", 'typed `is raw` parameter FETCHes a Proxy';
is untyped($p), "abc", 'untyped parameter';
my &c = &raw;
is c($p), "abc", 'through a Callable variable';
is (&raw).($p), "abc", 'through .()';
class H { has $.v; method go { &raw.($!v) } }
is H.new(v => $p).go, "abc", 'a Proxy handed to the constructor is FETCHed into the attribute';
