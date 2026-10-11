use Test;

# An untyped positional parameter is an implicit `Any`: a candidate whose first
# parameter carries a `where` refinement beats one that spells the second
# parameter `Any` / `Str()` (CSS::Module's `parse-property`, #10310).
plan 7;

class M {
    has %!c = a => 1;
    multi method a(Str:D $n where (%!c{.lc}:exists), $v) { "where" }
    multi method a(Str:D $n, Str() $v) { "plain" }
    multi method b(Str:D $n where (%!c{.lc}:exists), $v) { "where" }
    multi method b(Str:D $n, Any $v) { "plain" }
    multi method c(Str:D $n where (%!c{.lc}:exists), $v) { "where" }
    multi method c(Str:D $n, Str:D $v) { "plain" }
    multi method d(Str:D $n where (%!c{.lc}:exists), $v) { "where" }
    multi method d(Str:D $n, Str:D() $v, Bool :$x) { "plain" }
}
my $m = M.new;
is $m.a('a', 'x'), 'where', 'against a Str() second parameter';
is $m.a('b', 'x'), 'plain', 'the refinement still has to hold';
is $m.b('a', 'x'), 'where', 'against an explicit Any';
is $m.b('b', 'x'), 'plain', 'explicit Any, refinement fails';
is $m.c('a', 'x'), 'where', 'against Str:D';
is $m.d('A', 'x'), 'where', 'with a named parameter, lower-cased topic';
is $m.d('z', 'x'), 'plain', 'and without a match';
