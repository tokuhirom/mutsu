use Test;

# `.new` answers three class-shape questions from caches instead of walking
# the MRO on every call (#9291): whether a BUILD/TWEAK runs (the call site's
# purity), whether a user method must beat the native fast path, and which
# `has $x` alias attributes the instance carries. These pin that each answer
# still follows the class, including a change made after the first call.

plan 8;

# A TWEAK that writes a captured outer lexical: the construction is not pure,
# so the caller sees the write.
{
    my $n = 0;
    class Counted { has $.x; submethod TWEAK { $n++ } }
    Counted.new(x => $_) for ^3;
    is $n, 3, 'a class TWEAK writing an outer lexical is seen by the caller';
}

# The same through a role's TWEAK.
{
    my $n = 0;
    role Counts { submethod TWEAK { $n++ } }
    class RoleCounted does Counts { has $.x }
    RoleCounted.new(x => $_) for ^3;
    is $n, 3, 'a role TWEAK writing an outer lexical is seen by the caller';
}

# A user method added after the class has been constructed natively wins
# over the native path from then on.
{
    class Grows { has $.x }
    is Grows.new(x => 1).x, 1, 'constructed natively first';
    Grows.^add_method('new', method (*%_) { 'custom' });
    Grows.^compose;
    is Grows.new(x => 2), 'custom', 'a later user new is dispatched to';
}

# A later user method named like a native one wins on an instance too.
{
    class Late { has $.x }
    my $o = Late.new(x => 5);
    is $o.gist, 'Late.new(x => 5)', 'the native gist first';
    Late.^add_method('gist', method () { 'mine' });
    Late.^compose;
    is $o.gist, 'mine', 'a later user gist is dispatched to';
}

# A `has $x` (no twigil) attribute is readable by its alias in a method.
{
    class Aliased { has $x = 5; has $.y; method sum { $x + $!y } }
    is Aliased.new(y => 1).sum, 6, 'an alias attribute on the first construction';
    is Aliased.new(y => 2).sum, 7, 'and on a later one';
}
