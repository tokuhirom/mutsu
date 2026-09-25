use Test;

# Issue #9258: `Regex.Bool` matches against the `$_` of the scope the regex
# literal was written in, not the `$_` visible where it is boolified. A block
# whose tail is a bare regex (`{ /foo/ }`) returns the Regex itself, and that
# Regex must match the argument the block was called with. mutsu used the
# caller's `$_`, so `so f("foo")` was False and `List::MoreUtils`'s
# `after { /foo/ }, ...` never fired.

plan 16;

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
