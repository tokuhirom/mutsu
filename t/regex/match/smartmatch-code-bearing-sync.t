# Pins the by-name readers behind `~~` that #9169's last slice stopped
# covering with a whole-frame locals -> env publish: code embedded in a regex
# (`{ }`, `<?{ }>`, `:my`), a substitution's replacement (interpolation and
# embedded code), and a junction / pair / hash RHS holding regexes. Each case
# reads an outer lexical that was reassigned after its declaration, so a stale
# env entry would show up as a wrong answer.
use Test;

plan 15;

# --- code embedded in a regex literal ---
{
    my $want = "x";
    $want = "b";
    ok  "abc" ~~ / (.) <?{ $0 eq $want }> /, '<?{ }> reads a reassigned scalar';
    my \sl = "c";
    ok  "abc" ~~ / (.) <?{ $0 eq sl }> /, '<?{ }> reads a sigilless variable';
    my sub is-wanted($c) { $c eq $want }
    ok  "abc" ~~ / (.) <?{ is-wanted(~$0) }> /, '<?{ }> calls a lexical sub';
    my @pool = <q r>;
    @pool.push("a");
    ok  "a" ~~ / (.) <?{ ~$0 (elem) @pool }> /, '<?{ }> reads a mutated array';
    my $seen = '';
    "ab" ~~ / a { $seen = $want } b /;
    is  $seen, "b", '{ } reads and writes outer lexicals';
    my $lim = 1;
    $lim = 2;
    ok  "aa" ~~ / ^ :my $n = 0; [ a { $n++ } ]+ <?{ $n == $lim }> $ /,
        ':my plus a reassigned outer limit';
}

# --- indirect lookups in embedded code still see the live value ---
{
    use MONKEY-SEE-NO-EVAL;
    my $want = "x";
    $want = "b";
    ok  "abc" ~~ / (.) <?{ $0 eq EVAL('$want') }> /, 'EVAL in <?{ }> reads the live value';
    ok  "abc" ~~ / (.) <?{ $0 eq ::('$want') }> /, '::() in <?{ }> reads the live value';
}

# --- substitution replacements ---
{
    my $suf = "x";
    $suf = "y";
    my $s = "abc";
    $s ~~ s/b/$suf/;
    is  $s, "ayc", 's/// replacement interpolates a reassigned scalar';
    $s = "abc";
    $s ~~ s/b/{ $suf ~ "!" }/;
    is  $s, "ay!c", 's/// replacement code reads a reassigned scalar';
    my $pat = "q";
    $pat = "c";
    $s = "abc";
    $s ~~ s/$pat/Z/;
    is  $s, "abZ", 's/// pattern interpolates a reassigned scalar';
    my $u = "abc";
    $u ~~ tr/a/z/;
    is  $u, "zbc", 'tr/// through ~~ still writes back';
}

# --- regexes held inside a computed RHS value ---
{
    my $z = "x";
    $z = "q";
    ok  "aq" ~~ any(/zz/, /a$z/), 'junction element regex interpolates';
    ok  (a => "aq") ~~ (a => /a$z/), 'pair value regex interpolates';
    my @res = /zz/, /a$z/;
    ok  "aq" ~~ any(@res), 'junction over an array of regexes';
}
