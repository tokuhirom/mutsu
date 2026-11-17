use Test;

# Every X::Multi::NoMatch mutsu raises carries rakudo's "Cannot resolve
# caller" header and candidate list (#17859): built-in methods read rakudo's
# candidates from the generated table, user classes and mixins list their own.

plan 14;

sub message(&code) {
    code();
    CATCH { default { return .^name ~ ': ' ~ .message } }
    'no error';
}

my $junction = message { Junction.new(1, 2, 3) };
ok $junction.starts-with('X::Multi::NoMatch: Cannot resolve caller new(Junction:U: Int:D, Int:D, Int:D); none of these signatures matches:'),
    'Junction.new header';
ok $junction.contains('(Junction $:: Str:D \type, \values, *%_)'), 'Junction.new candidates';

my $pair = message { Pair.new(1) };
ok $pair.contains("Cannot resolve caller new(Pair:U: Int:D); none of these signatures matches:\n    (Pair \$:: Str:D \$key, Mu \\value, *%_)"),
    'Pair.new lists its candidates';

ok message({ Int.new(1, 2) }).contains('(Int $:: Int:D \value = 0, *%_ --> Int:D)'), 'Int.new lists its candidates';

ok message({ "a".subst }).starts-with('X::Multi::NoMatch: Cannot resolve caller subst(Str:D: ); none of these signatures matches:'),
    '.subst without a matcher';

ok message({ "a".match(Nil) }).starts-with('X::Multi::NoMatch: Cannot resolve caller match(Str:D: Nil:U);'),
    'a Nil argument is profiled as Nil:U';

is message({ "a".subst-mutate("a", "b") }).lines[0..1].join("\n"),
    "X::Multi::NoMatch: Cannot resolve caller subst-mutate(Str:D: Str:D, Str:D); the following candidates\nmatch the type but require mutable arguments:",
    'subst-mutate on a literal needs a mutable invocant';

ok message({ Buf.new(1).allocate(3) }).contains('(Blob:U $:: Int:D $elements, *%_)'),
    'a role-group method (Buf.allocate) lists its candidates';

ok message({ Lock.new.protect(1) }).starts-with('X::Multi::NoMatch: Cannot resolve caller protect(Lock:D: Int:D);'),
    'Lock.protect with a non-Callable';

ok message({ (1..4).batch }).starts-with('X::Multi::NoMatch: Cannot resolve caller batch('), '.batch without :elems';

{
    class A { multi method m(Int $x) { }; multi method m(Str $x) { } }
    is message({ A.new.+m(1.5) }),
        "X::Multi::NoMatch: Cannot resolve caller m(A:D: Rat:D); none of these signatures matches:\n    (A \$:: Int \$x, *%_)\n    (A \$:: Str \$x, *%_)",
        '.+ on a user class lists its candidates';
}

{
    role R { multi method m(Int $x) { }; multi method m(Str $x) { } }
    is message({ (5 but R).m(1.5) }),
        "X::Multi::NoMatch: Cannot resolve caller m(Int+\{R}:D: Rat:D); none of these signatures matches:\n    (Int+\{R} \$:: Int \$x, *%_)\n    (Int+\{R} \$:: Str \$x, *%_)",
        'a mixin lists the role candidates on the mixin type';
}

{
    class P { proto method new(|) {*} }
    is message({ P.new(1) }),
        'X::Multi::NoMatch: Cannot resolve caller new(P:U, Int:D); Routine does not have any candidates.  Is only the proto defined?',
        'a proto with no candidates';
}

like message({ $*OUT.printf }), /'Too few positionals passed; expected at least 1 arguments but got only 0'/,
    '.printf without a format';
