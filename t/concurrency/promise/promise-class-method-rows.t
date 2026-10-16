use Test;

# ADR-11276 §9.120: `Promise`'s class methods are rows of the method table,
# found by their owner for the `Promise` type object or a subclass's. Answers
# checked against Rakudo 2026.09.

plan 11;

is Promise.start({ 3 }).result, 3, 'Promise.start';
is Promise.in(0.01).result, True, 'Promise.in';
is Promise.kept.result, True, 'Promise.kept without a value';
is Promise.kept(4).result, 4, 'Promise.kept($value)';
is Promise.broken.cause.message, 'Died', 'Promise.broken without a reason';
is Promise.broken('x').cause.message, 'x', 'Promise.broken($reason)';
is Promise.allof(Promise.kept(1), Promise.kept(2)).result, True, 'Promise.allof';
is Promise.anyof(Promise.kept(1), Promise.new).result, True, 'Promise.anyof';
is Promise.allof.status, Kept, 'Promise.allof of nothing is kept';

class P is Promise { }
is P.kept(1).^name, 'P', 'a subclass type object builds its own class';
ok Promise.^can('allof'), '.^can sees Promise.allof';
