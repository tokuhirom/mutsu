use lib 't/lib';
use Test;
use NamedArgStmtCall;

# A statement-level call carrying a named argument dispatches through the
# interpreter's statement-call path, which writes each aliased container
# parameter back to the caller on return. When the caller's variable is a
# closure-captured lexical it lives in a shared container cell, and the
# writeback used to *replace* that cell with the callee's own alias cell —
# severing the caller's local slot from the name. A later whole-container
# assignment then wrote the old cell while a by-name read found the new one,
# so the assignment looked silently lost.

plan 6;

my @arr;
my sub push-one() { @arr.push('x') }
push-one();

takes-container @arr, ['x'], 'reason', opt => 1;
@arr = ();
is @arr.elems, 0, 'whole-array assignment after a named-arg statement call takes effect';

push-one();
is-deeply @arr, ['x'], 'the closure and the caller still share one container';

@arr = <p q>;
is-deeply @arr, ['p', 'q'], 'a later list assignment is visible by name too';

# The same for a hash.
my %h;
my sub set-one() { %h<a> = 1 }
set-one();

takes-container %h, {a => 1}, 'reason', opt => 1;
%h = ();
is %h.elems, 0, 'whole-hash assignment after a named-arg statement call takes effect';

set-one();
is-deeply %h, {a => 1}, 'the closure and the caller still share one hash';

# A positional-only statement call was always fine; keep it pinned.
my @plain;
my sub push-plain() { @plain.push('y') }
push-plain();
takes-container @plain, ['y'], 'reason';
@plain = ();
is @plain.elems, 0, 'positional statement call keeps the caller container assignable';
