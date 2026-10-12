use Test;

# The natively implemented `&code` / `&producer` parameters bind against
# Callable, as rakudo's signature binder does: `{ }` in argument position is an
# empty Hash, not a block (#12801).

plan 10;

throws-like { Promise.start({ }) }, X::TypeCheck::Binding::Parameter,
    message => /"parameter '&code'; expected Callable but got Hash"/,
    'Promise.start rejects an empty Hash';
throws-like { Promise.start(42) }, X::TypeCheck::Binding::Parameter,
    'Promise.start rejects an Int';
throws-like { Promise.kept(1).then({ }) }, X::TypeCheck::Binding::Parameter,
    'Promise.then rejects an empty Hash';
throws-like { Promise.kept(1).andthen({ }) }, X::TypeCheck::Binding::Parameter,
    'Promise.andthen rejects an empty Hash';
throws-like { Promise.kept(1).orelse({ }) }, X::TypeCheck::Binding::Parameter,
    'Promise.orelse rejects an empty Hash';
throws-like { Supply.on-demand({ }) }, X::TypeCheck::Binding::Parameter,
    message => /"parameter '&producer'"/,
    'Supply.on-demand rejects an empty Hash';
throws-like { $*SCHEDULER.cue({ }) }, X::Multi::NoMatch,
    '$*SCHEDULER.cue matches no candidate for an empty Hash';

# Callables still bind.
is Promise.start(sub { 41 + 1 }).result, 42, 'Promise.start takes a sub';
my class Doer does Callable { method CALL-ME() { 'called' } }
is Promise.start(Doer.new).result, 'called', 'Promise.start takes a Callable object';
is Promise.kept(1).then(-> $p { $p.result + 1 }).result, 2, 'Promise.then takes a pointy block';
