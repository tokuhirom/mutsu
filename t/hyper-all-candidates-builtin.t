use Test;

plan 7;

# `.+method` / `.*method` on a builtin returns one result per MRO level that
# declares the native method (e.g. List.elems + Any.elems), matching Rakudo's
# all-candidate dispatch — not a single result.
is-deeply <a b>.+elems, (2, 2), '.+elems on a List has two candidates';
is-deeply <a b>.*elems, (2, 2), '.*elems on a List has two candidates';

# Through the hyper `»` operator, per element.
my @a := <a b>, <c d e>;
is-deeply @a».+elems, ((2, 2), (3, 3)), '».+elems distributes all-candidates';
is-deeply @a».*elems, ((2, 2), (3, 3)), '».*elems distributes all-candidates';

# Quoted / interpolated method name goes through the dynamic hyper path.
is-deeply @a».+"elems"(), ((2, 2), (3, 3)), '».+""() all-candidates (dynamic)';
is-deeply @a».*"elems"(), ((2, 2), (3, 3)), '».*""() all-candidates (dynamic)';

# The result is a List, not an Array.
ok @a».+elems[0] ~~ List, '.+ inner result is a List';
