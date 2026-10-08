use Test;

# From the CSS::Grammar distribution (t/error-handling.t): an action fires the
# moment its rule reduces, so a subrule that matched inside an alternative the
# grammar later backtracked out of (and recovered with a fallback) still ran.
plan 4;

class Log {
    has @.log;
    method a($/) { @!log.push: ~$/ }
}

grammar Fallback {
    rule TOP { <e> || <junk> }
    rule e { <w> '(' <a> ')' }
    rule a { <['\w]>+ }
    token w { \w+ }
    token junk { .* }
}

my $act = Log.new;
ok Fallback.parse("f('abc", :actions($act)), 'fallback alternative parses';
is-deeply $act.log, ["'abc"], 'action of subrule in the abandoned alternative ran';

grammar FallbackAfterTerm {
    rule TOP { <e> 'z' || <junk> }
    rule e { <w> '(' <a> ')' }
    rule a { <['\w]>+ }
    token w { \w+ }
    token junk { .* }
}

my $act2 = Log.new;
ok FallbackAfterTerm.parse("f(abc)", :actions($act2)), 'parses via fallback';
is-deeply $act2.log, ["abc"], 'action ran once for the abandoned alternative';
