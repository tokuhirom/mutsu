use Test;

# Two compile-time syntax errors on variable declarations:
#  * `my $x :a` — you can't adverb a declaration (X::Syntax::Adverb).
#  * `my Int (Str $x)` — an outer declaration type conflicting with an inner
#    element type is X::Syntax::Variable::ConflictingTypes.

plan 9;

throws-like 'my $x :a', X::Syntax::Adverb, 'scalar decl + adverb';
throws-like 'my @a :b', X::Syntax::Adverb, 'array decl + adverb';
throws-like 'my %h :c', X::Syntax::Adverb, 'hash decl + adverb';

throws-like 'my Int (Str $x);', X::Syntax::Variable::ConflictingTypes,
    :outer(Int), :inner(Str), 'outer Int vs inner Str';
throws-like 'my Numeric (Str $y);', X::Syntax::Variable::ConflictingTypes,
    'outer Numeric vs inner Str';

# Valid forms still parse / run.
lives-ok { EVAL 'my $x := 5' }, 'bind (`:=`) is not an adverb';
lives-ok { EVAL 'my Int:D $y = 3' }, 'definite smiley on the type lives';
lives-ok { EVAL 'my (Int $a, Str $b) = (1, "x")' }, 'per-var types without an outer type live';
lives-ok { EVAL 'my Int ($a, $b) = (1, 2)' }, 'outer type with untyped elements lives';
