use Test;

plan 8;

# A colonpair written with NO space directly after a parenless method name is an
# adverbial named argument (Raku's `.method:adverb` form), e.g. `$obj.git:so`.
# mutsu previously failed to parse it: `:so` was read as a colon-listop positional
# (`so` prefix op) which needs an operand, yielding a "condition expression" /
# "Confused" parse error. Some dists (e.g. Overwatch) use this on attributes:
# `$.git-interval = 5 if $.git ~~ Bool && $.git:so;`

# A genuine parse error surfaces as X::Syntax / "Confused"; EVAL of a class
# definition (never executed) is the cleanest way to assert "parses".
sub parses(Str $code) {
    my $err = '';
    try {
        EVAL $code;
        CATCH { default { $err = .Str } }
    }
    $err eq '' || !($err.contains('Confused') || $err.contains('expected'));
}

ok parses('class C { has $.git; method go() { my $x; $x = 5 if $.git ~~ Bool && $.git:so } }'),
    '$.attr:adverb inside an if-condition parses';

ok parses('my $o; $o.some-method:so;'),
    '$o.method:so parses (bareword colonpair adverb)';

ok parses('my $o; $o.some-method:foo(3);'),
    '$o.method:foo(3) parses (colonpair with value)';

ok parses('my $o; $o.some-method:$x;'),
    '$o.method:$x parses (variable colonpair adverb)';

# The colonpair adverb is absorbed by methods that ignore extra named args,
# matching Raku (`5.Str:foo` -> "5", `(1,2,3).elems:foo` -> 3).
is (5.Str:foo), '5', '.Str:adverb absorbs the colonpair (matches Raku)';
is ((1, 2, 3).elems:foo), 3, '.elems:adverb absorbs the colonpair (matches Raku)';

# `.method:{...}` (no space) is a colon-listop BLOCK argument, NOT a typed-hash
# colonpair — must not regress.
my @doubled = (1, 2, 3).map:{ $_ * 2 };
is-deeply @doubled, [2, 4, 6], '.map:{block} stays a block arg';

# `.method: {block}` WITH a space is a colon-listop positional, unaffected.
my @kept = <a b c>.grep: { .chars == 1 };
is-deeply @kept, ['a', 'b', 'c'], '.grep: {block} (space) still positional';
