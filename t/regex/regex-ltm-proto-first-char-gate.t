# A proto's candidates are left out of the ranking, without measuring them,
# when their first character cannot begin a match at the position; a lone
# candidate left is tried as is (#12917). These pin that the match (which
# candidate wins, what it captures, which code and actions run) stays rakudo's,
# in the cases the shortcut takes and in the ones it leaves to the measurement.
# Values are rakudo's.
use Test;

plan 16;

grammar P {
    token TOP { <item>+ % ',' }
    proto token item {*}
    token item:sym<num>   { \d+ }
    token item:sym<word>  { <[a..z]>+ }
    token item:sym<quote> { "'" <-[']>* "'" }
    token item:sym<neg>   { '-' \d+ }
}

my $m = P.parse(q{12,ab,'x y',-3});
ok $m, 'every candidate kind parses';
is $m<item>.map(*.Str).join('|'), q{12|ab|'x y'|-3}, 'each item is its own candidate';
{
    class PA {
        method item:sym<num>($/)   { make 'num' }
        method item:sym<word>($/)  { make 'word' }
        method item:sym<quote>($/) { make 'quote' }
        method item:sym<neg>($/)   { make 'neg' }
    }
    is P.parse(q{12,ab,'x y',-3}, :actions(PA))<item>.map(*.made).join(','),
        'num,word,quote,neg', 'each item is made by its own candidate';
}
nok P.parse('12,%'), 'no candidate can start at the bad item';
nok P.parse('12,'), 'no candidate can start at the end';
nok P.parse(q{'open}), 'the lone starter fails later';

# Several candidates can start: the longest still wins.
grammar L {
    token TOP { <op> }
    proto token op {*}
    token op:sym<lt> { '<' }
    token op:sym<le> { '<=' }
    token op:sym<sh> { '<<' }
    token op:sym<eq> { '==' }
}
is L.parse('<=')<op>.Str, '<=', 'longest of the candidates that can start';
is L.parse('<<')<op>.Str, '<<', 'another longest';
is L.parse('==')<op>.Str, '==', 'a lone starter';

# A candidate that can match empty has no first character: it stays.
{
    my @seen;
    grammar E {
        token TOP { <thing> 'z' }
        proto token thing {*}
        token thing:sym<a>     { 'a' { @seen.push('a') } }
        token thing:sym<empty> { <?> { @seen.push('empty') } }
    }
    E.parse('az');
    is @seen.join(','), 'a', 'the starter wins over the empty candidate';
    @seen = ();
    E.parse('z');
    is @seen.join(','), 'empty', 'the empty candidate matches where none starts';
}

# Only the tried candidate's code and action run.
{
    my @seen;
    grammar C {
        token TOP { <c> }
        proto token c {*}
        token c:sym<x> { 'x' { @seen.push('x') } }
        token c:sym<y> { 'y' { @seen.push('y') } }
    }
    class CA {
        method c:sym<x>($/) { @seen.push('act-x') }
        method c:sym<y>($/) { @seen.push('act-y') }
    }
    C.parse('y', :actions(CA));
    is @seen.join(','), 'y,act-y', 'only the matching candidate runs code and its action';
}

# Candidates that begin with rule calls.
grammar R {
    token TOP { <v> }
    proto token v {*}
    token v:sym<n> { <digits> }
    token v:sym<w> { <letters> '!' }
    token digits { \d+ }
    token letters { <[a..z]>+ }
}
is R.parse('123')<v><digits>.Str, '123', 'a candidate through a call';
is R.parse('hey!')<v><letters>.Str, 'hey', 'another candidate through a call';
nok R.parse('hey'), 'the lone starter through a call fails later';

# A proto in a regex (backtracking) caller.
grammar B {
    regex TOP { <s> '9' }
    proto regex s {*}
    regex s:sym<d> { \d* }
    regex s:sym<a> { 'a'+ }
}
is B.parse('129')<s>.Str, '12', 'a regex caller backtracks into the lone candidate';
