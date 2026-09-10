use Test;

# Parsing of user-defined operators and of terms that collide with a quote
# language. Section 6 declares symbols named after quote constructs (`Q`, `q`,
# `m`, `s`, ...), which permanently removes those quote languages for the rest
# of the compilation unit, so every test that needs a real quoter comes FIRST.

# ---------------------------------------------------------------------------
# 1. Undeclared quote constructs must still be quote constructs.
# ---------------------------------------------------------------------------
is q/hi/, 'hi', 'q// is a quoter when q is not declared';
is Q/a$b/, 'a$b', 'Q// is a quoter when Q is not declared';
{
    my $z = 5;
    is qq/v=$z/, 'v=5', 'qq// is a quoter when qq is not declared';
}
is qw/a b c/.join(','), 'a,b,c', 'qw// is a quoter when qw is not declared';
ok 'abc' ~~ m/b/, 'm// is a match when m is not declared';
{
    my $t = 'abc';
    $t ~~ s/b/X/;
    is $t, 'aXc', 's/// is a substitution when s is not declared';
}
{
    my $t = 'abc';
    my $y = S/b/X/ given $t;
    is $y, 'aXc', 'S/// is a substitution when S is not declared';
}
{
    my $t = 'abc';
    $t ~~ tr/b/X/;
    is $t, 'aXc', 'tr/// transliterates when tr is not declared';
}
{
    my $t = 'abc';
    my $y = TR/b/X/ given $t;
    is $y, 'aXc', 'TR/// transliterates when TR is not declared';
}
ok 'abc' ~~ rx/b/, 'rx// is a regex when rx is not declared';

# ---------------------------------------------------------------------------
# 2. Nested Unicode curly quotes.
# ---------------------------------------------------------------------------
is “here: “no problem” at all!”, 'here: “no problem” at all!',
    'curly double quotes nest';
is “a “b “c” d” e”, 'a “b “c” d” e', 'curly double quotes nest more than one level';
{
    my $z = 5;
    is “a “$z” b”, 'a “5” b', 'a nested curly double quote still interpolates';
}
is „a „b” c”, 'a „b” c', 'low curly double quotes nest on their opener';
is ‘a ‘b’ c’, 'a ‘b’ c', 'curly single quotes nest';
is ‚a ‚b’ c’, 'a ‚b’ c', 'low curly single quotes nest';
is ”rev”, 'rev', 'the reversed curly pair opens and closes with the same character';
is ｢raw $x｣, 'raw $x', 'corner brackets are still literal';

# ---------------------------------------------------------------------------
# 3. Custom infix precedence. A trait-less `sub infix:<...>` has ADDITIVE
#    precedence in Raku, so it binds tighter than `~`, `..`, `&&` and `?? !!`.
# ---------------------------------------------------------------------------
{
    sub infix:<zz>($a, $b) { "($a,$b)" }
    is 1 zz 2 ~ 3, '(1,2)3', 'a custom infix binds tighter than ~';
    is (1 zz 2 && 3), 3, 'a custom infix binds tighter than &&';
    is 1 zz 2 * 3, '(1,6)', 'a custom infix binds looser than *';
    is 1 zz 2 zz 3, '((1,2),3)', 'a custom infix is left-associative by default';
    is (1 zz 2 ?? 'y' !! 'n'), 'y', 'a custom infix binds tighter than ??';
    is (1 ?? 'y' !! 'n' zz 3), 'y', 'the else-branch of a ternary is tighter than a custom infix';
    my $x = 1 zz 2 ?? 'y' !! 'n';
    is $x, 'y', 'a custom infix followed by a ternary parses on an assignment RHS';
    sub f($a) { $a }
    is f(1 zz 2 ?? 'y' !! 'n'), 'y', 'a custom infix followed by a ternary parses as a call argument';
}
{
    sub infix:<amic>($m, $n) { $m == $n }
    my @pair = (2, 2);
    is (2 amic @pair[1]??" yes"!!"no"), " yes", 'the amicable-numbers doc example parses';
}
{
    sub infix:<tt>($a, $b) is tighter(&infix:<+>) { "($a,$b)" }
    is (1 tt 2 ?? 'y' !! 'n'), 'y', 'an `is tighter` custom infix still binds tighter than ??';
}
{
    sub infix:<ll>($a, $b) is looser(&infix:<+>) { "($a,$b)" }
    is 1 ll 2 + 3, '(1,5)', 'an `is looser` custom infix binds looser than +';
    is (1 ll 2 ?? 'y' !! 'n'), 'y', 'an `is looser` custom infix still binds tighter than ??';
}
{
    sub infix:<rr>($a, $b) is assoc<right> { "($a,$b)" }
    is 1 rr 2 rr 3, '(1,(2,3))', 'is assoc<right> right-folds';
}
# A bareword that is NOT a declared infix must never be claimed as one.
{
    my $o = 42 but role :: { method greet { 'hi' } };
    is $o.greet, 'hi', '`but` is still the mixin operator, not a speculative infix';
}

# ---------------------------------------------------------------------------
# 4. Custom circumfix / postcircumfix operators.
# ---------------------------------------------------------------------------
{
    sub circumfix:<α ω>($a) { $a * 2 }
    is (α 5 ω), 10, 'a circumfix with Unicode-letter delimiters is callable';
    is (α α 5 ω ω), 20, 'a Unicode-letter circumfix nests';
    is (α 5 + 3 ω), 16, 'a Unicode-letter circumfix takes a full expression';
}
{
    sub circumfix:<lo hi>($a) { $a * 3 }
    is (lo 5 hi), 15, 'a circumfix with lowercase ASCII word delimiters is callable';
}
{
    sub circumfix:<UP DOWN>($a) { $a * 4 }
    is (UP 5 DOWN), 20, 'a circumfix with uppercase ASCII word delimiters is callable';
}
{
    multi postcircumfix:<[- ]>(Str:D $str is copy, +@indices) {
        for @indices.reverse {
            when Int   { $str.substr-rw($_, 1) = '' }
            when Range { $str.substr-rw($_)    = '' }
        }
        return $str;
    }
    is '0123456789'[- 1..3, 8 ], '045679',
        'a custom postcircumfix beats the built-in subscript by longest token';
    is '0123456789'[- 8 ], '012345679',
        'a one-item custom postcircumfix subscript passes that item alone';
    is postcircumfix:<[- ]>('0123456789', (1..3, 8)), '045679',
        'the explicit call form agrees with the subscript form';
}

# ---------------------------------------------------------------------------
# 5. Loop labels are ordinary identifiers.
# ---------------------------------------------------------------------------
{
    my $out = '';
    MY-LABEL:
    for 1..10 {
        next MY-LABEL if $_ < 5;
        $out ~= "$_ ";
    }
    is $out, '5 6 7 8 9 10 ', 'next <hyphenated label> honours its statement modifier';
}
{
    my $out = '';
    MY-LABEL2:
    for 1..10 {
        last MY-LABEL2 if $_ > 5;
        $out ~= "$_ ";
    }
    is $out, '1 2 3 4 5 ', 'last <hyphenated label> honours its statement modifier';
}
{
    my $out = '';
    my-lowercase-label:
    for 1..3 {
        next my-lowercase-label if $_ < 2;
        $out ~= "$_ ";
    }
    is $out, '2 3 ', 'a lowercase loop label works';
}
{
    my $out = '';
    Outer:
    for 1..3 -> $i {
        In-ner:
        for 1..3 -> $j {
            next Outer if $j == 2;
            $out ~= "$i$j ";
        }
    }
    is $out, '11 21 31 ', 'next <outer label> from a nested loop';
}
{
    my $out = '';
    for 1..4 {
        next if $_ < 3;
        $out ~= "$_ ";
    }
    is $out, '3 4 ', 'a bare `next if` is still unlabelled';
}
{
    my $seen = 0;
    my $redone = False;
    RE-DO:
    for 1..3 {
        $seen++;
        if $_ == 2 {
            LEAVE $redone = True;
            redo RE-DO unless $redone;
        }
    }
    is $seen, 4, 'redo <hyphenated label> honours its statement modifier';
}

# ---------------------------------------------------------------------------
# 6. Variable traits: `is name[...]` argument sugar vs type parameterisation.
# ---------------------------------------------------------------------------
{
    my @a is Array[Int] = 1, 2;
    is @a.join(','), '1,2', 'is Array[Int] is still a type parameterisation';
}

# ---------------------------------------------------------------------------
# 7. A declared symbol shadows the quote language spelled the same way.
#    These declarations are unit-scoped, so nothing after them may use a
#    quote construct named `Q`/`q`/`qq`/`qw`/`m`/`s`/`S`/`tr`/`TR`/`rx`.
# ---------------------------------------------------------------------------
{
    enum QuoterNames <PLACE-HOLDER Q q qq qw m s S tr TR rx>;
    is (Q, 'x', 2).join('|'), 'Q|x|2', 'a declared Q is a term, not the Q quoter';
    is (q, 'x', 2).join('|'), 'q|x|2', 'a declared q is a term, not the q quoter';
    is (qq, 'x', 2).join('|'), 'qq|x|2', 'a declared qq is a term, not the qq quoter';
    is (qw, 'x', 2).join('|'), 'qw|x|2', 'a declared qw is a term, not the qw quoter';
    is (m, 'x', 2).join('|'), 'm|x|2', 'a declared m is a term, not the m// matcher';
    is (s, 'x', 2).join('|'), 's|x|2', 'a declared s is a term, not the s/// substitution';
    is (S, 'x', 2).join('|'), 'S|x|2', 'a declared S is a term, not the S/// substitution';
    is (tr, 'x', 2).join('|'), 'tr|x|2', 'a declared tr is a term, not tr///';
    is (TR, 'x', 2).join('|'), 'TR|x|2', 'a declared TR is a term, not TR///';
    is (rx, 'x', 2).join('|'), 'rx|x|2', 'a declared rx is a term, not rx//';
    ok 1 ~~ Q, 'a declared quoter-named enum value smartmatches as a term';
}
{
    constant Qc = 42;
    is Qc, 42, 'a constant named like a quoter prefix is a term';
    sub Qs() { 7 }
    is Qs, 7, 'a sub named like a quoter prefix is a term';
    my \Qt = 9;
    is Qt, 9, 'a sigilless term named like a quoter prefix is a term';
}

# ---------------------------------------------------------------------------
# 8. ...but an explicit adverb is unambiguously the quote language, so it wins
#    over a declaration of the same name (Raku: `s:g/.../.../` is always a
#    substitution, even with `sub s` in scope).
# ---------------------------------------------------------------------------
{
    sub sub-shadow-marker() { 1 }
    my $t = 'abab';
    $t ~~ s:g/a/X/;
    is $t, 'XbXb', 's:g/// is a substitution even though `s` is declared';
    ok 'AB' ~~ m:i/a/, 'm:i// is a match even though `m` is declared';
    is q:w/a b/.join(','), 'a,b', 'q:w// is a quoter even though `q` is declared';
}

done-testing;
