use Test;

# The `:x` adverb to `.subst` must be an Int / Range / Whatever. An invalid
# value (e.g. a class instance) is an X::Str::Match::x error, just like `.match`.

plan 7;

# valid forms keep working
is "aaaa".subst(/a/, "X", :x(2)),   "XXaa", ':x(Int) limits the substitution count';
is "aaaa".subst(/a/, "X", :x(*)),   "XXXX", ':x(*) substitutes all';
is "aaaa".subst(/a/, "X", :x(1..2)), "XXaa", ':x(Range) works';
is "aaaa".subst(/a/, "X", :x(5..6)), "aaaa", ':x(Range) past available matches yields no substitution';

# invalid value throws the typed exception
throws-like { "abc".subst(/\w/, "", :x(my class SomeInvalidXParam {}.new)) },
    X::Str::Match::x, 'invalid :x value throws X::Str::Match::x';

# the exception carries the offending value and a helpful message
{
    my $ex = (try { "abc".subst(/\w/, "", :x([1, 2])) }) // $!;
    isa-ok $ex, X::Str::Match::x, ':x(Array) also throws X::Str::Match::x';
    ok $ex.message.contains('Int or Range'), 'message mentions the expected types';
}
