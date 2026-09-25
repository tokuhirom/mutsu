# Pins the by-name readers behind `~~` that #9293 stopped covering with a
# whole-frame locals -> env publish: code embedded in a regex or a
# substitution replacement that looks a name up *indirectly* (`EVAL`,
# `::($n)`), which now reads the store-side env mirror its chunk keeps, and a
# `Proxy` / lazy RHS value. Each case reads a lexical that was changed after
# its declaration, so a stale env entry would show up as a wrong answer.
use MONKEY-SEE-NO-EVAL;
use Test;

plan 16;

# --- EVAL / ::() inside code embedded in a regex ---
{
    my $w = "x";
    $w = "b";
    ok "ab" ~~ / (.) <?{ $0 eq EVAL(q[$w]) }> /, 'EVAL in <?{ }> reads a reassigned scalar';
    my $u = 5;
    $u++;
    ++$u;
    $u += 1;
    ok "x" ~~ / <?{ EVAL(q[$u]) == 8 }> /, 'EVAL sees ++ / += on a local';
    my @a = 1, 2;
    @a.push(3);
    ok "x" ~~ / <?{ EVAL(q[@a]).elems == 3 }> /, 'EVAL sees a pushed array';
    my %h;
    %h<k> = 4;
    ok "x" ~~ / <?{ EVAL(q[%h<k>]) == 4 }> /, 'EVAL sees a hash store';
    my $n = q[$u];
    ok "x" ~~ / <?{ ::($n) == 8 }> /, '::($n) in <?{ }> reads a local';
    my ($d1, $d2) = 3, 4;
    ok "x" ~~ / <?{ EVAL(q[$d1 + $d2]) == 7 }> /, 'EVAL sees list-assigned locals';
}

# --- loop parameters, inner blocks, routines ---
{
    my @got;
    for 1..2 -> $p { @got.push: so "ab" ~~ / <?{ EVAL(q[$p]) == $p }> / }
    is-deeply @got, [True, True], 'EVAL sees each loop parameter';
    my $cnt = 0;
    my @seen;
    for ^3 -> $i { $cnt++; @seen.push: so "x" ~~ / <?{ EVAL(q[$cnt]) == $i + 1 }> / }
    is-deeply @seen, [True, True, True], 'EVAL sees a counter bumped each iteration';
    { my $in = 7; $in *= 2; ok "x" ~~ / <?{ EVAL(q[$in]) == 14 }> /, 'EVAL sees an inner-block local' }
    sub s1($arg) { my $l = $arg ~ "!"; so "x" ~~ / <?{ EVAL(q[$l]) eq "q!" && EVAL(q[$arg]) eq "q" }> / }
    ok s1("q"), 'EVAL sees a routine local and parameter';
    my $v = 1;
    my &g = -> { "b" ~~ / <?{ EVAL(q[$v]) == 3 }> b / };
    $v = 3;
    ok g(), 'EVAL in a closure regex sees a later outer assignment';
}

# --- substitutions and computed regexes ---
{
    my $u = 1;
    $u = 8;
    my $s = "aXb";
    $s ~~ s/X/{ EVAL(q[$u]) }/;
    is $s, "a8b", 'EVAL in an s/// replacement';
    my $t = "zz";
    my $r = "p";
    $r = "q";
    $t ~~ s:g/z/{ ::(q[$r]) }/;
    is $t, "qq", '::() in an s:g/// replacement';
    my $w = 1;
    my $re = / <?{ EVAL(q[$w]) == 2 }> /;
    $w = 2;
    ok "x" ~~ $re, 'a computed regex whose code EVALs a reassigned local';
}

# --- Proxy and lazy RHS values ---
{
    my $w = "x";
    $w = "b";
    my $p := Proxy.new(FETCH => -> $ { /a$w/ }, STORE => -> $, $ { });
    ok "ab" ~~ $p, 'a Proxy RHS is FETCHed and its regex matched';
    nok "ab" ~~ (/a$w/, /zz/).lazy, 'a lazy list RHS is not reified';
}
