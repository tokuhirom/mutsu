use Test;

# The replacement half of s/// and S/// is a qq quote, so every qq
# interpolation form has to work there: capture references, named captures,
# variables with postcircumfixes, embedded { ... } code blocks (including ones
# glued directly onto literal text), and the full backslash-escape set.

plan 48;

# --- positional captures -----------------------------------------------------
{
    my $s = 'ab';
    $s ~~ s/(a)/[$0]/;
    is $s, '[a]b', 'positional $0 interpolates';
}
{
    my $s = 'ab';
    $s ~~ s/(a)(b)/$1$0/;
    is $s, 'ba', 'several positional captures interpolate';
}
{
    my $s = 'axbxc';
    $s ~~ s:g/(x)/<$0>/;
    is $s, 'a<x>b<x>c', ':g re-evaluates the replacement per match';
}

# --- named captures ----------------------------------------------------------
{
    my $s = 'ab';
    $s ~~ s/$<n>=(a)/[$<n>]/;
    is $s, '[a]b', 'named capture $<n> interpolates';
}
{
    my $_ = '2016-01-23 18:09:00';
    s/ $<y>=(\d+)\-$<m>=(\d+)\-$<d>=(\d+) /$<m>-$<d>-$<y>/;
    is $_, '01-23-2016 18:09:00', 'several named captures reorder a date';
}
{
    my $s = 'a1b';
    $s ~~ s/$<d>=(\d)/<$<d>>/;
    is $s, 'a<1>b', 'named capture between literal angle brackets';
}
{
    my $s = 'ab';
    $s ~~ s/$<n>=(a)/x/;
    is ~$/<n>, 'a', 'the $/ left behind by s/// carries named captures';
}

# --- $/ ----------------------------------------------------------------------
{
    my %h = a => 1, b => 2;
    my $s = 'abc';
    $s ~~ s:g/<[ab]>/{%h{$/}}/;
    is $s, '12c', 'the whole match indexes a hash inside a code block';
}
{
    my %h = a => 1, b => 2;
    is (S:g/<[ab]>/%h{$/}/ given 'abc'), '12c',
        'a hash subscript is an interpolation, not literal text';
}
{
    my %h = a => 1, b => 2;
    my @a = %h.keys.sort;
    is (S:g/@a/%h{$/}/ given 'abc'), '12c',
        'an array interpolated into the pattern is an alternation';
    is (S:g/@(%h.keys.sort)/%h{$/}/ given 'abc'), '12c',
        '@(EXPR) in the pattern is an alternation too';
}

# --- ordinary variables ------------------------------------------------------
{
    my $v = 'V';
    my $s = 'ab';
    $s ~~ s/a/[$v]/;
    is $s, '[V]b', 'a plain scalar interpolates';
}
{
    my @a = <x y>;
    my $s = 'ab';
    $s ~~ s/a/@a[1]/;
    is $s, 'yb', 'an array element interpolates';
}
{
    my @a = <p q>;
    my $s = 'ab';
    $s ~~ s/a/@a[0]@a[1]/;
    is $s, 'pqb', 'two adjacent array elements each interpolate';
}
{
    my @a = <x y>;
    my $s = 'ab';
    $s ~~ s/a/@a/;
    is $s, '@ab', 'a bare @a is NOT interpolated (needs a postcircumfix)';
}
{
    my %h = a => 1;
    my $s = 'ab';
    $s ~~ s/a/%h<a>/;
    is $s, '1b', 'a hash angle-subscript interpolates';
}
{
    my $s = 'ab';
    $s ~~ s/(a)/{$0.uc}/;
    is $s, 'Ab', 'a method call inside a code block sees the capture';
}

# --- embedded code blocks ----------------------------------------------------
{
    my $s = 'ab';
    $s ~~ s/(a)/d{lc $0}/;
    is $s, 'dab', 'literal text immediately followed by a code block';
}
{
    my $s = 'ab';
    $s ~~ s/(a)/{uc $0}d/;
    is $s, 'Adb', 'a code block immediately followed by literal text';
}
{
    my $s = 'ab';
    $s ~~ s/(a)/a{1}b/;
    is $s, 'a1bb', 'a code block between two literal words';
}
{
    my $s = 'ab';
    $s ~~ s/a/{ "x" }{ "y" }/;
    is $s, 'xyb', 'two adjacent code blocks';
}
{
    my $_ = '18:38';
    s/(\d+)\:(\d+)/{$0 % 12}\:$1 {$0 < 12 ?? 'AM' !! 'PM'}/;
    is $_, '6:38 PM', 'code blocks mixed with an escaped literal colon';
}
{
    my $s = 'ab';
    $s ~~ s/a/{ my $t = 1; $t + 1 }/;
    is $s, '2b', 'a multi-statement code block';
}

# --- backslash escapes -------------------------------------------------------
{
    my $s = 'ab';
    $s ~~ s/a/x\:y/;
    is $s, 'x:yb', 'an escaped non-alphanumeric is that character (no backslash)';
}
{
    my $s = 'ab';
    $s ~~ s/a/x\/y/;
    is $s, 'x/yb', 'an escaped delimiter is a literal delimiter';
}
{
    my $s = 'ab';
    $s ~~ s/a/x\ny/;
    is $s, "x\nyb", '\\n is a newline';
}
{
    my $s = 'ab';
    $s ~~ s/a/x\ty/;
    is $s, "x\tyb", '\\t is a tab';
}
{
    my $s = 'ab';
    $s ~~ s/a/x\\y/;
    is $s, 'x' ~ '\\' ~ 'yb', '\\\\ is one backslash';
}
{
    my $s = 'ab';
    $s ~~ s/(a)/x\$0y/;
    is $s, 'x$0yb', '\\$ suppresses interpolation';
}
{
    my @a = <x y>;
    my $s = 'ab';
    $s ~~ s/a/p\@a[0]q/;
    is $s, 'p@a[0]qb', '\\@ suppresses array interpolation';
}
{
    my $s = 'ab';
    $s ~~ s/a/p\{1}q/;
    is $s, 'p{1}qb', '\\{ suppresses code-block interpolation';
}
{
    my $s = 'ab';
    $s ~~ s/a/x\x[41]y/;
    is $s, 'xAyb', '\\x[..] is a codepoint';
}
{
    my $s = 'ab';
    $s ~~ s/a/\c[LATIN SMALL LETTER Z]/;
    is $s, 'zb', '\\c[NAME] is a named character';
}

# --- S/// (non-destructive) --------------------------------------------------
{
    my $s = 'ab';
    is (S/(a)/[$0]/ given $s), '[a]b', 'S/// interpolates captures too';
    is $s, 'ab', 'S/// leaves its subject alone';
}
{
    is (S:g/(<[ab]>)/{$0.uc}/ given 'abc'), 'ABc', 'S:g/// runs its code block per match';
}

# --- adverbs, with and without whitespace ------------------------------------
{
    my $s = 'abc';
    $s ~~ s:g:i/(B)/<$0>/;
    is $s, 'a<b>c', 'adverbs written tight against the s';
}
{
    my $s = 'abc';
    $s ~~ s :g :i/(B)/<$0>/;
    is $s, 'a<b>c', 'whitespace is allowed between s and its adverbs';
}
{
    my $str = 'foo muCKed into the lEn';
    $str ~~ s:2nd/o/x/;
    $str ~~ s :g :i/<[ML]> (\S+)/d{lc $0}/;
    is $str, 'fox ducked into the den', 'spaced adverbs plus a bareword-adjacent code block';
}
{
    my $s = 'abc';
    is (S :g/b/X/ given $s), 'aXc', 'whitespace before an adverb works for S/// too';
}
{
    my $s = 'abc';
    $s ~~ m :i/B/;
    is ~$/, 'b', 'whitespace before an adverb works for m// too';
}
{
    my $s = 'abc';
    $s ~~ tr :d/b//;
    is $s, 'ac', 'whitespace before an adverb works for tr/// too';
}

# --- :samecase / :nth / :x still see the interpolated replacement ------------
{
    my $s = 'aXb';
    $s ~~ s:samecase/x/qq/;
    is $s, 'aQQb', ':samecase applies to the interpolated result';
}
{
    my $s = 'aaa';
    $s ~~ s:2nd/(a)/<$0>/;
    is $s, 'a<a>a', ':nth picks one match and still interpolates';
}
{
    my $s = 'aaaa';
    $s ~~ s:x(2)/(a)/<$0>/;
    is $s, '<a><a>aa', ':x(2) interpolates each of its matches';
}

# --- :P5 replacements are qq quotes as well ---------------------------------
{
    my $s = 'ab';
    $s ~~ s:P5/(a)/[$0]/;
    is $s, '[a]b', 'a :P5 pattern still numbers its captures the Raku way';
}
{
    my $s = 'ab';
    $s ~~ s:P5/a/{1}/;
    is $s, '1b', 'a :P5 replacement evaluates code blocks';
}

# --- a replacement that throws propagates -----------------------------------
{
    my $s = 'ab';
    dies-ok { $s ~~ s/a/{ die "boom" }/ }, 'a die inside a replacement block escapes';
}
