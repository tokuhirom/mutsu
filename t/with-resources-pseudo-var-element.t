use v6;
use lib 't/lib/ResElemTopic/lib';
use Test;

plan 2;

# `%?RESOURCES` is a compiler-synthesized pseudo-hash (rebuilt fresh on every
# plain read from the current package's distribution), not a real container
# stored in locals/env. `with`/`given`'s "element-source" writeback
# optimization for a subscripted lvalue topic (`with %h<k> -> $v {...}`)
# resolves the base container by name in the locals store — which finds
# nothing for `%?RESOURCES` and silently binds the topic to Nil instead of
# falling through to a plain (read-only, but correct) element read.
# Cro::HTTP::Router's bundled-resource routes (`sub resource` in
# Router.rakumod) use exactly this `with %?RESOURCES{$path} -> $resource
# {...}` shape.

use ResElemTopic;

is ResElemTopic.new.greet, 'hello from the ResElemTopic resources',
    'with %?RESOURCES{key} -> $v topicalizes the real resource entry, not Nil';

# An ordinary hash-variable element topic must keep resolving to the real
# element value (no regression in the writeback optimization this fix
# carves a narrow exception out of).
my %h = a => 1, b => 2;
with %h<a> -> $v { is $v, 1, 'an ordinary hash-variable element topic still binds the real value' }
