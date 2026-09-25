use Test;

# Issue #9258: `Regex.Bool` matches against the `$_` of the scope the regex
# literal was written in, not the `$_` visible where it is boolified. A block
# whose tail is a bare regex (`{ /foo/ }`) returns the Regex itself, and that
# Regex must match the argument the block was called with. mutsu used the
# caller's `$_`, so `so f("foo")` was False and `List::MoreUtils`'s
# `after { /foo/ }, ...` never fired.

plan 26;

my &f = { /foo/ };

is f("foo").^name, 'Regex', 'a bare-regex tail returns the Regex itself';
ok  (so f("foo")), 'so: matches against the block argument';
nok (so f("bar")), 'so: fails for a non-matching block argument';
ok  ?f("foo"),     'prefix ?: matches against the block argument';
ok  f("foo").Bool, '.Bool on a returned regex uses its lexical topic';
ok  f("foo").so,   '.so on a returned regex uses its lexical topic';

{
    my $r = f("foo");
    $_ = "bar";
    ok $r.Bool, '.Bool through a variable ignores the caller topic';
    ok (f("foo") ?? True !! False), 'ternary condition boolifies via the lexical topic';
    my $hit = False;
    if f("foo") { $hit = True }
    ok $hit, 'if condition boolifies via the lexical topic';
}

is-deeply (<foo bar>.map: { so f($_) }).List, (True, False), 'called from a map block';
is-deeply <bar foo baz>.grep({ /foo/ }).List, ('foo',), 'grep with a bare-regex block';
is <bar foo baz>.first({ /foo/ }), 'foo', 'first with a bare-regex block';
is-deeply <bar foo baz>.map({ /foo/ }).map(*.Bool).List, (False, True, False),
    'regexes returned by a map block keep their own topics';

sub mk($_) { /foo/ }
ok  mk("xfoo").Bool, 'a sub with a $_ parameter returns a regex bound to it';

# The shape List::MoreUtils's `after` uses.
sub after(&c, *@l) {
    my $found = False;
    gather for @l { take $_ if $found; $found = True if c($_) }
}
is-deeply after({ /foo/ }, <bar baz>).List, (), 'after: no match yields the empty list';
is-deeply after({ /foo/ }, <bar foo baz>).List, ('baz',), 'after: yields the tail after the match';

# Issue #9263: List::MoreUtils's `occurrences` test filters words out of a
# `.comb` with `.grep: { /\w+/ }`. Treating the returned Regex as plainly true
# kept the punctuation, so the Bag-based occurrence sum came out 142, not 124.
{
    my @tokens = "a b, a. c, b a.".comb(/ \w+ | <[,.]> /);
    is-deeply @tokens.grep({ /\w+/ }).List, <a b a c b a>,
        'grep with a bare-regex block drops the non-matching punctuation';
    my @o;
    @o[.value].push(.key) for @tokens.grep({ /\w+/ }).Bag.pairs;
    is @o.pairs.grep(*.value.defined).map({ .key * .value }).sum, 6,
        'occurrence sum counts only the words the grep kept';
}

# Issue #9396: the regex captures its defining scope's `$_` *container*, not a
# value snapshot. An assignment to that `$_` after the literal was created is
# seen; a routine's own `$_` and a `for` loop's rebinding are not.
{
    my $q = /zz/;
    sub h { $_ = "zz"; $q.Bool }
    $_ = "a";
    nok h(), "a routine's own \$_ does not reach a mainline regex";
    my @seen = do for <zz> { $q.Bool };
    is-deeply @seen, [False], "a for loop's topic rebind does not reach it";
    $_ = "zz";
    ok $q.Bool, 'a later assignment to the defining $_ is seen';
    $_ = "a";
    nok ?$q, 'and re-assigning it back is seen too';
}
{
    sub g { my $r = /foo/; $_ = "foo"; $r.Bool }
    ok g(), 'an assignment after the literal, in the same routine, is seen';
    sub g2 { my $r = /foo/; $_ = "bar"; $r }
    nok g2().Bool, 'the routine-local $_ is kept after the routine returns';
}
{
    # Boxing `$_` for the capture must not break `given $x`'s live
    # write-back of `$_ = ...` into `$x`.
    my $t = "hello";
    given $t { my $g = /ell/; $_ = "no" }
    is $t, 'no', 'given $x still writes $_ assignments back to $x';
    my @a = 1, 2;
    for @a { my $g = /1/; $_ = 7 if $g }
    is-deeply @a, [7, 2], 'a for element topic still writes back';
}
