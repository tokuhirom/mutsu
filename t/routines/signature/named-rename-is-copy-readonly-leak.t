use v6;
use Test;

# Regression (FunctionalParsers 0.1.10, tokuhirom/mutsu#8526): a renamed named
# parameter's `is copy`/`is rw`/`is raw` trait — `:target(:$actions) is copy`
# — parses onto the alias WRAPPER (external key `target`), never onto the
# LEAF variable it binds (`$actions`). The signature binder's per-candidate
# readonly-marking loop walks only the top-level parameter list, so for a
# renamed param it marked/unmarked `target`, a symbol nothing ever reads or
# assigns — `actions` was left exactly as the shared, symbol-keyed readonly
# table already had it.
#
# That is invisible as long as `actions` was never marked readonly by
# anything else. It breaks the moment the CALLER also happens to declare an
# (unrelated, unsupplied) named parameter of the same external name: the
# caller's own `:$actions` marks bare symbol `actions` readonly for its
# frame, and — because the callee's `is copy` never reached that symbol —
# the mark leaks straight through into the callee's `$actions`, which then
# fails "Cannot assign to a readonly variable" despite its own `is copy`.

plan 3;

sub inner(:target(:$actions) is copy = 'DEFAULT') {
    $actions = 'CHANGED';
    $actions;
}

{
    # No name collision: works even before the fix.
    is inner(actions => 'X'), 'CHANGED',
        'is-copy renamed leaf is writable with no caller collision';
}

sub outer(:$actions = Whatever) {
    inner(actions => 'X');
}

is outer(), 'CHANGED',
    'is-copy renamed leaf stays writable when the caller has its own same-named param';

# The proto/multi-dispatched shape FunctionalParsers actually hit: the
# collision reaches the leaf through a recursive proto call as well.
proto sub p-inner(|) {*}
multi sub p-inner(Str $x, *%args) {
    p-inner($x.comb.Array, |%args);
}
multi sub p-inner(@x, :target(:$actions) is copy = 'DEFAULT', *%args) {
    $actions = 'CHANGED';
    $actions;
}
proto sub p-outer($g, |) {*}
multi sub p-outer(Str $ebnf, :$actions = Whatever) {
    p-inner($ebnf, actions => 'X');
}

is p-outer("abc"), 'CHANGED',
    'is-copy renamed leaf survives a proto-dispatched recursive forwarding call';
