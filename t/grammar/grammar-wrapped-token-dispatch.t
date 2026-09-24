use Test;

# A `.wrap` on a grammar token must still be honoured by the regex engine.
# The engine skips the wrap-chain lookup (and the per-subrule backtrace frame)
# while the method wrap table is empty, so this parses once with no wrap
# anywhere and then checks that the first wrap installed switches the lookup
# back on for a grammar that has not parsed yet.

plan 21;

grammar Plain {
    token TOP  { <word>+ % ' ' }
    token word { \w+ }
}

grammar Wrapped {
    token TOP  { <word>+ % ' ' }
    token word { \w+ }
}

ok Plain.parse('ab cd'), 'parse with no wrap installed anywhere';

my @seen;
Wrapped.^find_method('word').wrap(-> |c { @seen.push('word'); callsame });

my $m = Wrapped.parse('ab cd ef');
ok $m, 'parse succeeds through the wrapped token';
is $m<word>.elems, 3, 'wrapped token still produces its captures';
# The wrapper is user code: it runs once per real subrule call, never during
# the LTM prefix measurement or the failure-position probe (#9151).
is @seen.elems, 3, 'wrapper ran exactly once per word';

@seen = ();
nok Wrapped.parse('ab  cd'), 'wrapped token still fails where the grammar fails';

grammar Failing {
    token TOP  { <word>+ % ' ' }
    token word { \w+ }
}
my $failing = 0;
Failing.^find_method('word').wrap(-> |c { $failing++; callsame });
nok Failing.parse('ab cd ef!'), 'parse with trailing garbage fails';
is $failing, 3, 'failure-position probe does not re-run the wrapper';

# Plain subrule calls go through the streamed (lazy) subrule driver, which has
# to hand a wrapped token back to the dispatching path.
grammar Spaced {
    rule  TOP { <w> <w> }
    token w   { \w+ }
    token ws  { \s* }
}
my $ws = 0;
Spaced.^find_method('ws').wrap(-> |c { $ws++; callsame });
ok Spaced.parse('ab cd'), 'rule with a wrapped ws parses';
is $ws, 2, 'wrapped ws runs for each implicit <.ws> call';

grammar Proto {
    proto token TOP {*}
    token TOP:sym<a> { <x> 'a' }
    token TOP:sym<b> { <x> 'b' }
    token x { \d+ }
}
my $x = 0;
Proto.^find_method('x').wrap(-> |c { $x++; callsame });
ok Proto.parse('12b'), 'proto candidates calling a wrapped token parse';
is $x, 1, 'LTM ranking of proto candidates does not run the wrapper';

# A wrapper can name the rule that called it from a Backtrace, which needs
# each real subrule call to carry its routine frame (Grammar::PrettyErrors'
# `lastrule`).
grammar Caller {
    rule  TOP   { <inner> }
    rule  inner { 'a' 'b' }
    token ws    { <!ww> \s* }
}
my @callers;
Caller.^find_method('ws').wrap(-> $m, |c {
    my $bt = Backtrace.new;
    @callers.push: $bt[$bt.next-interesting-index(:named)].code.name;
    callsame;
});
ok Caller.parse('a b'), 'rules calling a wrapped ws parse';
is-deeply @callers.unique.List, <inner TOP>, 'wrapper sees the calling rule name';

# `.parse` / `.subparse` enter the start rule directly; a wrap on it must still
# run (#9190), with the regular parse (full-match check, :actions) inside it.
grammar Start {
    token TOP  { <word>+ % ' ' }
    token word { \w+ }
}
my @start;
Start.^find_method('TOP').wrap(-> |c { @start.push('TOP'); callsame });
my $sm = Start.parse('ab cd');
ok $sm, 'parse through a wrapped TOP succeeds';
is $sm<word>.elems, 2, 'wrapped TOP keeps its captures';
is-deeply @start, ['TOP'], 'wrapper on TOP ran once';
nok Start.parse('ab cd!'), 'wrapped TOP still requires a full match';
class StartActions { method TOP($/) { make 42 } }
is Start.parse('x y', :actions(StartActions)).made, 42, ':actions run under a wrapped TOP';
is @start.elems, 3, 'wrapper ran once per parse';

grammar StartRule { token TOP { <word> }; token word { \w+ } }
my @rule;
StartRule.^find_method('word').wrap(-> |c { @rule.push('word'); callsame });
is ~StartRule.subparse('ab cd', :rule<word>), 'ab', 'subparse :rule<word> through a wrapped word';
is-deeply @rule, ['word'], 'wrapper on a :rule start rule ran';
