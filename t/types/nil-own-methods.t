use Test;

# `Nil`'s own methods, as Rakudo's src/core.c/Nil.rakumod declares them
# (ADR-11276): the Cool text methods that warn and answer "", the numeric and
# string coercions, the subscript writers, `new` and the QuantHash coercions.

plan 21;

# Each text method warns "Use of Nil.<name> coerced to empty string".
my @warned;
my @answers;
{
    for <chars chomp flip lc tc tclc uc wordcase chop codes comb contains
         ends-with index indices lines rindex starts-with trans substr subst
         substr-eq words> -> $m {
        @answers.push: Nil."$m"();
    }
    @answers.push: Nil.indent(2);
    @answers.push: Nil.substr(1, 2);
    CONTROL { when CX::Warn { @warned.push: .message; .resume } }
}
is-deeply @answers.unique.List, ("",), 'every text method answers the empty string';
is +@warned, 25, 'every text method warns once';
is @warned[*-1], 'Use of Nil.substr coerced to empty string', 'the warning names the method';
ok @warned.grep('Use of Nil.indices coerced to empty string'), 'indices warns';
ok @warned.grep('Use of Nil.indent coerced to empty string'), 'indent warns';

# The coercions.
my @coerced;
{
    @coerced.push: Nil.Int;
    @coerced.push: Nil.Int(1);
    @coerced.push: Nil.Numeric;
    @coerced.push: Nil.Str;
    @coerced.push: Nil.Str(:x);
    CONTROL { when CX::Warn { @coerced.push: .message; .resume } }
}
is-deeply @coerced,
    ['Use of Nil in numeric context', 0, 'Use of Nil in numeric context', 0,
     'Use of Nil in numeric context', 0, 'Use of Nil in string context', '',
     'Use of Nil in string context', ''],
    'Int(|), Numeric() and Str() warn about their context';

# The subscript protocol's writers.
throws-like { Nil.STORE(1, 2) }, X::Assignment::RO,
    message => 'Cannot modify an immutable Nil value', 'STORE';
my $bound = Nil.BIND-POS(0, 1);
isa-ok $bound, Failure, 'BIND-POS answers a Failure';
isa-ok $bound.exception, X::Bind, '... of X::Bind';
is $bound.exception.message, 'Cannot bind to Nil', '... about Nil';
isa-ok Nil.BIND-KEY('a').exception, X::Bind, 'BIND-KEY';
throws-like { Nil.ASSIGN-POS(0) }, X::AdHoc,
    message => 'Use of Nil.ASSIGN-POS not allowed', 'ASSIGN-POS';
throws-like { Nil.ASSIGN-KEY('a', 1, 2) }, X::AdHoc,
    message => 'Use of Nil.ASSIGN-KEY not allowed', 'ASSIGN-KEY';
# A key and a value bind `Any`'s narrower candidates.
throws-like { Nil.ASSIGN-POS(0, 1) }, X::Assignment::RO, 'ASSIGN-POS with a value';
throws-like { Nil.BIND-KEY('a', 1) }, X::Parameter::RW, 'BIND-KEY with a value';

# `new` answers Nil; the QuantHash coercions hold the one Nil.
is-deeply Nil.new(1, 2), Nil, 'new';
is-deeply Nil.Set, Set.new(Nil), 'Set';
is-deeply Nil.Bag, Bag.new(Nil), 'Bag';
is-deeply Nil.MixHash, MixHash.new(Nil), 'MixHash';
is Nil.SetHash.elems, 1, 'SetHash';

# A hyper call reaches the same rows.
my @hyper;
{
    @hyper = (Nil, 'ab')».chars;
    CONTROL { when CX::Warn { .resume } }
}
is-deeply @hyper, ['', 2], 'a hyper call on a Nil element';
