use Test;

# A `|` alternation enters its branches in LTM rank order, and a lower-ranked
# branch only after every branch above it has failed against the rest of the
# pattern (ADR-0022, ADR-0135 D4; #9922). So a losing branch's code block runs
# only when the winner fails, and then after the winner's. Expected logs are
# raku's.

plan 5;

my @log;

"abc" ~~ / [ a { @log.push: "A" } | ab { @log.push: "AB" } ] c /;
is-deeply @log, ["AB"], 'the winning branch runs alone when it succeeds';

@log = ();
"ab" ~~ / [ a { @log.push: "A" } | ab { @log.push: "AB" } ] b /;
is-deeply @log, ["AB", "A"], 'a losing branch runs after the winner fails';

@log = ();
"abd" ~~ / [ a { @log.push: "A" } | ab { @log.push: "AB" } | abc { @log.push: "ABC" } ] d /;
is-deeply @log, ["AB"], 'a branch that cannot match runs nothing';

@log = ();
"ab" ~~ / [ a { @log.push: "A" } | ab { @log.push: "AB" } ] /;
is-deeply @log, ["AB"], 'without a continuation, only the winner runs';

@log = ();
"ab" ~~ / :r [ a { @log.push: "A" } | ab { @log.push: "AB" } ] b /;
is-deeply @log, ["AB"], 'under ratchet the alternation commits to the winner';
