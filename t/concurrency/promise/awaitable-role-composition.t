use Test;

plan 11;

# `Awaitable` is a core role — `raku -e 'say Awaitable.^name'` resolves it with
# no `use` — and real code composes it onto its own classes (`TAP.rakumod`:
# `class Parser does Awaitable`, delegating `get-await-handle` to a Promise
# attribute). It was missing from mutsu's core role list, so the name decayed
# to a bareword and the composition died with `Invalid typename 'Awaitable'`.
is Awaitable.^name, 'Awaitable', 'Awaitable names itself';
is Awaitable.HOW.^name, 'Perl6::Metamodel::ParametricRoleGroupHOW',
    'and it is a role, not a class';

class Handler does Awaitable {
    has Promise $.promise = Promise.kept(42);
    method get-await-handle() { $!promise.get-await-handle }
}

ok Handler ~~ Awaitable, 'a class that composes it does it';
ok Handler.new ~~ Awaitable, 'and so does an instance';
is Handler.^roles.map(*.^name).sort.join(','), 'Awaitable', 'it shows up in .^roles';
nok Int ~~ Awaitable, 'an unrelated type does not do it';
ok Promise ~~ Awaitable, 'Promise composes Awaitable';
ok Channel ~~ Awaitable, 'Channel composes Awaitable';
is Promise.^roles.map(*.^name).sort.join(','), 'Awaitable', 'Promise exposes Awaitable in .^roles';
is Channel.^roles.map(*.^name).sort.join(','), 'Awaitable', 'Channel exposes Awaitable in .^roles';
is await(Handler.new), 42, 'await dispatches through get-await-handle';
