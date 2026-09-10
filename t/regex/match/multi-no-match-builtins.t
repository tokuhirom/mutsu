use Test;

# Pins roast APPENDICES/A02-some-day-maybe/multi-no-match.t: a handful of
# builtins must throw X::Multi::NoMatch (not silently succeed, hang, or throw a
# vaguer error) when called with argument shapes that match no candidate.

plan 18;

throws-like { [].splice: 0, [] }, X::Multi::NoMatch,
    '.splice(offset, array) throws';
throws-like { [].splice: 0e0, 0 }, X::Multi::NoMatch,
    '.splice(Num offset) throws';
throws-like { [1,2,3].splice("1", 1) }, X::Multi::NoMatch,
    '.splice(Str offset) throws';

throws-like ｢Lock.protect: %()｣, X::Multi::NoMatch,
    'Lock.protect on the type object throws';
throws-like ｢Lock::Async.protect: %()｣, X::Multi::NoMatch,
    'Lock::Async.protect on the type object throws';
throws-like { Lock.new.protect: %() }, X::Multi::NoMatch,
    'Lock.protect with a non-Callable arg throws';

throws-like { Proc::Async.new }, X::Multi::NoMatch,
    'Proc::Async.new with no command throws';

throws-like ｢"".subst｣, X::Multi::NoMatch, '.subst with no arguments throws';

throws-like { $*OUT.printf }, X::Multi::NoMatch, '.printf with no format throws';

throws-like { "".match: Nil }, X::Multi::NoMatch,
    '.match with a Nil matcher throws';

throws-like { Pair.new: <a b c>, <d>, 42 }, X::Multi::NoMatch,
    'Pair.new with three positionals throws';
throws-like { Pair.new: :42a }, X::Multi::NoMatch,
    'Pair.new with a stray named arg throws';

throws-like { Junction.new: 42 }, X::Multi::NoMatch,
    'Junction.new with a lone positional throws';
throws-like { Junction.new }, X::Multi::NoMatch,
    'Junction.new with no args throws';

throws-like { Int.new: <a b c>, 42, 'x' }, X::Multi::NoMatch,
    'Int.new with multiple positionals throws';

dies-ok { Date.new(Int, 1, 1) },
    'Date.new with a type-object year dies';

# Sanity: the valid forms still work.
is Pair.new("k", "v").raku, ':k("v")', 'Pair.new(key, value) still works';
is Junction.new("any", (1, 2)).raku, 'any(1, 2)',
    'Junction.new(type, values) still works';
