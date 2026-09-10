use v6;
use Test;

# A `where { ... }` block-constraint is a "malformed double closure" ONLY when the
# block *leads* with a bare `*` (`{ * > 2 }`), which curries the whole block into a
# WhateverCode. A `*` used as a call/method ARGUMENT inside the block
# (`{ .grep: * > 2 }`, `{ $_ > * }`) forms its own inner WhateverCode and must NOT
# trip the guard. mutsu used to reject any block that contained a `*` anywhere.
# Found via the real-dist compat sweep (Data::Dump's
# `:%overrides where { !$_.values.grep: * !~~ Sub }`). See docs/dist-compat-sweep.md.

plan 6;

# --- the dist shape: `*` as a grep argument inside the where block ---
{
    sub only-subs(:%o where { !$_.values.grep: * !~~ Sub } = {}) { 'ok' }
    is only-subs(o => { a => sub {} }), 'ok', '`grep: *` arg in where block is accepted';
}

# --- `*` as a method-call argument; block leads with the topic method ---
{
    sub big(@x where { .grep: * > 2 }) { @x.elems }
    is big([3, 4, 5]), 3, '`.grep: * > 2` arg in where block is accepted';
}

# --- `*` as the RIGHT operand (block leads with `$_`) ---
{
    sub gt(:$n where { $_ > * .Int - 1 }) { $n }
    # (`$_ > *...` leads with `$_`, so the block is a normal `$_` closure)
    ok gt(n => 5).defined, '`$_ > *` (leads with $_) is accepted';
}

# --- the constraint actually runs and constrains ---
{
    sub pos($x where { $_.grep(* > 0) }) { $x }
    is pos(7), 7, 'where-block with a `*`-arg predicate enforces (pass)';
    dies-ok { pos(-3) }, 'where-block with a `*`-arg predicate enforces (fail)';
}

# --- a genuine leading-`*` double closure is still rejected ---
{
    my $err = '';
    {
        EVAL 'sub bad($x where { * > 2 }) { }';
        CATCH { default { $err = .message } }
    }
    ok $err.contains('double closure'),
        'leading `{ * > 2 }` is still rejected as a malformed double closure';
}
