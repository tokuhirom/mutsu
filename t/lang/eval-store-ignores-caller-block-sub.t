use Test;

# A `sub` declared in a routine's body is that body's lexical. A callee that
# reads `&name` reflectively (EVAL) must not find the CALLER's (ADR-12529).

plan 6;

sub reads-code-var { (try EVAL q[&helper]) // 'not visible' }
sub reads-call     { (try EVAL q[helper()]) // 'not visible' }
sub reads-amp-call { (try EVAL q[&helper()]) // 'not visible' }

sub caller-my-sub { my sub helper { 'H' }; reads-code-var() }
is caller-my-sub(), 'not visible', 'a caller\'s `my sub` is not read as &name';

sub caller-sub { sub helper { 'H' }; reads-code-var() }
is caller-sub(), 'not visible', 'a caller\'s `sub` is not read as &name';

sub caller-call { my sub helper { 'H' }; reads-call() }
is caller-call(), 'not visible', 'a caller\'s `my sub` is not called by name';

sub caller-amp-call { my sub helper { 'H' }; reads-amp-call() }
is caller-amp-call(), 'not visible', 'a caller\'s `my sub` is not called as &name()';

# The declaring routine still sees its own.
sub own { my sub helper { 'own' }; EVAL q[helper()] }
is own(), 'own', 'the declaring routine reads its own sub through EVAL';

sub own-closure { sub helper { 'closure' }; -> { helper() } }
is own-closure()(), 'closure', 'a closure that outlives the routine still calls it';
