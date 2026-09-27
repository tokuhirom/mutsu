use Test;

# `$_` declared or defaulted in a NESTED routine's or pointy block's signature
# belongs to that closure, so it does not turn the enclosing `{ ... }` into a
# block. LLM::Graph's test suite writes node specs as
# `{ eval-function => sub ($_ = Whatever) { ... } }` and needs a Hash.

plan 8;

is { a => sub ($_ = 1) { $_ } }.^name, 'Hash', 'sub ($_ = default) value keeps a hash composer';
is { a => sub ($x = $_) { 1 } }.^name, 'Hash', 'a $_ default in a nested sub signature';
is { a => -> $_ { 1 } }.^name, 'Hash', 'a pointy block declaring $_';
is { a => -> $x = $_ { 1 } }.^name, 'Hash', 'a $_ default in a pointy signature';
is { a => <-> $_ { 1 } }.^name, 'Hash', 'an rw pointy block declaring $_';

is { a => $_ }.^name, 'Block', 'a direct $_ still forces a block';
is { a => sub ($x) { 1 }, b => $_ }.^name, 'Block', 'a $_ after a nested signature still counts';

my %spec = poet => { eval-function => sub ($_ = Whatever) { $_.defined ?? $_ !! 'out' } };
is %spec<poet><eval-function>(), 'out', 'the nested sub is callable from the hash';
