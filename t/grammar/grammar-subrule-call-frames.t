use Test;

# ADR-0135 Slice D (#10254): a `<subrule>` call is a frame in the compiled
# engine's own loop. Every expectation below is rakudo's answer.

plan 28;

# A plain callee: ratchet commits to its first end, and the callee's Match is
# filed under its name with its own captures.
{
    grammar G {
        token TOP { <a> <b> }
        token a { \d+ }
        token b { <c> | 'x' }
        token c { <[a..c]>+ }
    }
    my $m = G.parse("12abc");
    ok $m.so, 'a chain of plain callees parses';
    is $m<a>.Str, '12', 'the first callee';
    is $m<b><c>.Str, 'abc', 'a callee two levels down';
    is G.parse("12x")<b>.Str, 'x', 'the other branch';
    nok G.parse("12d").so, 'no branch matches';
}

# A non-ratchet callee can be resumed after it returned: the caller's failure
# backtracks into the callee's own greedy choice.
{
    grammar H {
        regex TOP { <w> <w> }
        regex w { \w+ }
    }
    is H.parse("abcd")<w>.map(*.Str).join(","), 'abc,d', 'the first call gives back';
    is ("abcd" ~~ / <H::w> 'd' /).Str, 'abcd', 'a regex callee gives back in a match';

    grammar T {
        token TOP { <r> 'c' }
        regex r { \w+ }
    }
    nok T.parse("abc").so, 'a ratcheted call commits to the callee\'s first end';
}

# A block in a callee runs once per end the cursor enters, not once per end it
# could compute.
{
    my @log;
    grammar K {
        regex TOP { <r> 'c' }
        regex r { \w* { @log.push($/.Str) } }
    }
    ok K.parse("abc").so, 'a callee with a code block';
    is @log.join(","), 'abc,ab', 'the block ran for the two ends tried';
}

# A proto: the winning candidate's Match carries its :sym<...>, and actions
# dispatch under it.
{
    grammar P {
        token TOP { <value>+ % ',' }
        proto token value {*}
        token value:sym<num>  { \d+ }
        token value:sym<word> { <[a..z]>+ }
        token value:sym<list> { '[' ~ ']' <value>* % ',' }
        token value:sym<t>    { 'true' }
    }
    class A {
        method TOP($/) { make $<value>.map(*.made).join('|') }
        method value:sym<num>($/)  { make "N$/" }
        method value:sym<word>($/) { make "W$/" }
        method value:sym<list>($/) { make "L(" ~ $<value>.map(*.made).join(",") ~ ")" }
        method value:sym<t>($/)    { make "T" }
    }
    my $m = P.parse("12,ab,[1,2,[x]],true", :actions(A.new));
    ok $m.so, 'a proto grammar parses';
    is $m.made, 'N12|Wab|L(N1,N2,L(Wx))|T', 'actions fire under the candidate\'s :sym';
    is $m<value>.elems, 4, 'the quantified proto call is a list';
    nok P.parse("12,,3").so, 'a failing separator step';
    is P.subparse("12,ab,").Str, '12,ab', 'subparse stops before the trailing separator';
}

# Quantified calls keep their names as lists; `?` binds a Match or nothing.
{
    grammar R {
        token TOP { <a>+ <b>* <c>? <d> }
        token a { 'a' }
        token b { 'b' }
        token c { 'c' }
        token d { 'd' }
    }
    my $r = R.parse("aaabbcd");
    is $r<a>.elems, 3, '+ collects every iteration';
    is $r<b>.elems, 2, '* collects every iteration';
    ok $r<c>.defined, '? matched';
    nok R.parse("aad")<c>.defined, '? did not match';
    is R.parse("ad")<b>.elems, 0, '* matched zero times';
}

# `~` goal matching: the inner pattern, then the goal, captures in written order.
{
    grammar Q {
        token TOP { <list> }
        rule list { '[' ~ ']' <item>* % ',' }
        token item { <num> | <word> | <list> }
        token num { \d+ }
        token word { <[a..z]>+ }
    }
    my $m = Q.parse("[1, ab, [2,3], c]");
    ok $m.so, 'a bracketed list';
    is $m<list><item>.elems, 4, 'four items';
    is $m<list><item>[2]<list><item>.map(*.Str).join("+"), '2+3', 'a nested list';
    nok Q.parse("[1, ab").so, 'the goal is missing';
}

# Recursion: a frame per level, no Rust stack per level.
{
    grammar N {
        token TOP { <list> }
        token list { '[' <list>* ']' }
    }
    ok N.parse("[" x 300 ~ "]" x 300).so, 'three hundred levels deep';
    nok N.parse("[" x 300 ~ "]" x 299).so, 'one closer missing';
}

# Subrule captures inside a capture group and under aliases.
{
    grammar S {
        token TOP { (<a> <b>) <x=a> $<y>=<b> [ <a> <b> ]+ }
        token a { 'a' }
        token b { 'b' }
    }
    my $m = S.parse("ababababab");
    ok $m.so, 'a group, aliases and a quantified group';
    is $m[0]<a>.Str ~ $m[0]<b>.Str, 'ab', 'the group\'s own Match holds its subrules';
}
