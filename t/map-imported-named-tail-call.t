use Test;

# A block whose tail statement is a call with a NAMED argument (`f(k => v)`)
# must return the call's value. Such a call parses as a bare `Stmt::Call` with a
# `CallArg::Named` (notably for imported subs, which are unknown at parse time);
# the block-value compilation previously only preserved all-positional tail calls
# and discarded the result of a named/slip-arg call, so `.map` fell back to
# returning the loop topic. The subs below return a value DISTINCT from the topic
# so a regression (returning the topic) is detectable.

sub tagged(:$x) { "got-$x" }

is-deeply (1, 2, 3).map({ tagged(x => $_) }).List, ('got-1', 'got-2', 'got-3'),
    'map block tail call with named arg returns call value, not topic';

sub two(:$a, :$b) { "$a:$b" }
is-deeply <p q>.map({ two(a => $_, b => 'Z') }).List, ('p:Z', 'q:Z'),
    'map block tail call with multiple named args';

sub takeslip(*@xs) { 'slip:' ~ @xs.join(',') }
is-deeply (1, 2).map({ takeslip(|($_, $_)) }).List, ('slip:1,1', 'slip:2,2'),
    'map block tail call with slip arg';

# Same when the call is the implicit return of a plain sub.
sub wrap($v) { tagged(x => $v) }
is wrap(42), 'got-42', 'sub tail call with named arg returns call value';

done-testing;
