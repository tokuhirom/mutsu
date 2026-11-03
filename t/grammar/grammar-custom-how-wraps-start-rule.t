use v6;
use Test;
use lib 't/lib';
use GrammarProfilerFixture;

# A custom GrammarHOW's `find_method` wraps the start rule as it wraps every
# subrule, and the cursor it hands the wrapper is an instance of the grammar.
# The call counts are what rakudo reports (checked with `raku`); found via
# Grammar::Profiler::Simple t/csv.rakutest, whose per-rule counts were keyed
# under `Match` and never counted `TOP`.

plan 11;

grammar CSV {
    token TOP { ^ <line>+ % "\n" $ }
    token line { <value>+ % ',' }
    token value { <-[,]>+ }
}

sub calls(%t, $rule) { %t<CSV>{$rule}<calls> // 0 }

reset-timing;
ok ?CSV.parse("a,b\nc"), 'the parse still succeeds';
my %t = get-timing();
is %t.keys.sort.List, ('CSV',), 'timings are keyed by the grammar name';
is calls(%t, 'TOP'), 1, 'the start rule is counted';
is calls(%t, 'line'), 1, 'line counted once';
is calls(%t, 'value'), 2, 'value counted per call';

reset-timing;
ok ?CSV.parse("alpha"), 'a single value parses';
is calls(get-timing(), 'TOP'), 1, 'the start rule is counted again';

# A failed parse runs the rules once: the diagnostic probe that finds the
# furthest position must not run the profiling wrapper again.
reset-timing;
nok ?CSV.parse(""), 'the empty string does not parse';
%t = get-timing();
is-deeply (calls(%t, 'TOP'), calls(%t, 'line'), calls(%t, 'value')), (1, 1, 1),
    'a failed parse counts each rule once';

# The wrapper's cursor is an instance of the grammar, for the start rule and
# for a subrule alike.
reset-timing;
ok ?CSV.parse("a"), 'parse once more';
is get-cursor-names().keys.sort.List, ('CSV',), "the wrapper's cursor is a CSV, not a Match";
