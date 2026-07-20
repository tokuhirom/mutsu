use v6;
use Test;

# Attribute traits (`is`, `does`, `handles`, `will`) may appear in any order and
# any number. Previously a trait AFTER `handles` (e.g. `handles <..> is required`)
# was left unparsed. Regression pin for dist TAP.

class Status {
    method exit  { 'e' }
    method signal { 's' }
    method wait  { 'w' }
}

class Result {
    has Status $.exit-status handles <exit signal wait> is required;
    has Int:D  $.tests  handles <succ pred>            is required;
}

my $r = Result.new(exit-status => Status.new, tests => 5);
is $r.exit,   'e', 'handles <...> before `is required` still delegates (exit)';
is $r.signal, 's', 'handles delegation (signal)';
is $r.wait,   'w', 'handles delegation (wait)';
is $r.tests,  5,   'the `is required` after handles took effect (attribute present)';

# `is required` genuinely enforced when placed after handles
dies-ok { Result.new(tests => 1) },
    'is-required after handles is enforced (missing exit-status dies)';

# handles interleaved with other traits, and `is rw` before handles
class Box {
    has $.inner is rw handles <Str gist> = 'x';
}
lives-ok { Box.new }, 'is rw before handles parses and constructs';

# multiple handles clauses plus a trailing trait
class Multi {
    has $.a handles <one> handles <two> is required;
}
lives-ok { Multi.new(a => 1) }, 'multiple handles clauses then a trailing trait';

done-testing;
