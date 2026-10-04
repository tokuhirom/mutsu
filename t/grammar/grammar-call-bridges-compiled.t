use Test;

# Calls the compiled regex engine used to hand back to the walk's producer
# (ADR-0135 §8, Slice E, twentieth part): a rule whose `"…"` atoms read qq
# thunks runs as a frame, a wrapped token and a proto with a wrapped candidate
# run their wrappers, and a call of no rule with arguments is a builtin call.
# Expected values are rakudo's.

plan 8;

my $x = 'q';
my $n = 0;
grammar QQ {
    regex TOP { <t> 'ab' }
    regex t { "$x" a+ { $n++ } }
}
ok QQ.parse('qaaab'), 'a rule whose "…" atom interpolates a lexical matches';
is $n, 2, 'it runs as a frame: its code runs once per end the caller backtracks to';

grammar W {
    token TOP { <word> '!' }
    token word { \w+ }
}
my @log;
W.^find_method('word').wrap(-> |c { @log.push('word'); callsame });
ok W.parse('hi!'), 'a grammar with a wrapped token parses';
is @log, ['word'], 'the wrapper runs once';

grammar P {
    token TOP { <thing> }
    proto token thing {*}
    token thing:sym<a> { a+ }
    token thing:sym<b> { b+ }
}
my @plog;
P.^find_method('thing:sym<b>').wrap(-> |c { @plog.push('b'); callsame });
is ~P.parse('bbb'), 'bbb', 'a proto with a wrapped candidate parses';
is @plog, ['b'], 'the candidate\'s wrapper runs';

grammar T {
    token TOP { <w(42)> }
    token w(Str $s) { $s }
}
throws-like { T.parse('42') }, X::TypeCheck::Binding::Parameter,
    'a call whose arguments no candidate binds raises the type error';

grammar U {
    token TOP { 'a' <wb> }
}
ok U.parse('a'), 'a builtin call';
