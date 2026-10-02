# ADR-0135 Slice E: a `<subrule>` call whose callee needs a binding window --
# its `$*` parameters, or an object or closure argument that baking cannot
# carry into its code blocks -- runs as a frame of the compiled engine. The
# window is installed while the callee runs, removed when it returns or fails,
# and installed again when backtracking resumes inside the callee. Values are
# rakudo's.
use Test;

plan 12;

class Pick {
    has $.want;
    method ok($s) { $s eq $!want }
}

# Backtracking into a non-ratchet callee after it returned: its object
# argument must be bound again for the assertion that decides the new end.
grammar Back {
    regex TOP { <w(Pick.new(want => 'ab'))> 'c' }
    regex w($o) { (\w+) <?{ $o.ok(~$0) }> }
}
is ~Back.parse('abc')<w>, 'ab', 'an object argument is re-bound when backtracking into the callee';

grammar BackClosure {
    regex TOP { <w(-> $s { $s.chars == 2 })> \w }
    regex w(&f) { \w+ <?{ f(~$/) }> }
}
is ~BackClosure.parse('xyz')<w>, 'xy', 'a closure argument is re-bound when backtracking into the callee';

# A `$*` parameter reaches the subrules the callee calls, and ends with it.
my @seen;
grammar Dyn {
    token TOP { <x('q')> <z> }
    token x($*W) { <y> }
    token y { . { @seen.push: $*W } }
    token z { . { @seen.push: $*W // 'none' } }
}
ok Dyn.parse('ab'), 'a `$*` parameter call parses';
is @seen.join(','), 'q,none', 'the `$*` binding is visible in the callee subtree only';

# Nested windows shadow and restore.
my @order;
grammar Nest {
    token TOP { <a('outer')> }
    token a($*N) { <rec> <b('inner')> <rec> }
    token b($*N) { <rec> }
    token rec { . { @order.push: $*N } }
}
ok Nest.parse('abc'), 'nested `$*` windows parse';
is @order.join(','), 'outer,inner,outer', 'an inner window shadows and its return restores the outer one';

# A callee that fails removes its window before the next alternative runs.
my @alt;
grammar Fail {
    token TOP { [ <f('first')> 'X' || <g> ] }
    token f($*F) { . { @alt.push: $*F } }
    token g { . . { @alt.push: $*F // 'unset' } }
}
ok Fail.parse('ab'), 'the second alternative parses';
is @alt.join(','), 'first,unset', 'a failed call leaves no binding behind';

# A proto's candidates, ranked and entered inside a window, all see it.
grammar Proto {
    token TOP { <x('k')> }
    token x($*K) { <p> }
    proto token p {*}
    token p:sym<a> { a <?{ $*K eq 'nope' }> }
    token p:sym<b> { a <?{ $*K eq 'k' }> }
}
ok Proto.parse('a'), 'a later proto candidate sees the window';

# The action of a rule still reads its `$*` parameter after the match ended.
grammar Act {
    token TOP { <u('hello')> }
    token u($*U) { \w+ }
}
class ActA {
    method u($/) { make $*U }
    method TOP($/) { make $<u>.made }
}
is Act.parse('abc', :actions(ActA)).made, 'hello', 'an action reads its rule parameter';

# The window is gone once the parse finishes.
my $*AFTER = 'outer';
grammar Done {
    token TOP { <d('inner')> }
    token d($*AFTER) { . }
}
ok Done.parse('a'), 'parses';
is $*AFTER, 'outer', 'the caller binding is restored after the parse';
