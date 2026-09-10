use Test;

plan 6;

# The method-lookup twin of t/signature-parameter-object-identity.t:
# `classhow_lookup_impl` (`.^lookup`/`.^find_method`) builds the `Method`
# Instance -- and its `Signature` -- straight from the AST `MethodDef` on
# every call, with neither the `Arc<CompiledFunction>` nor
# `Arc<CompiledCode>` `sub_signature_value`'s cache keys on, so every
# `.^find_method(...).signature` read fell back to a fresh, un-mixin-able
# object. todo/tickets/method-lookup-signature-has-no-stable-identity.md

role Q { }

class C {
    method m(:$mp) { }
}
my $p = C.^find_method('m').signature.params[1]; # 0 is the invocant
$p does Q;
ok $p ~~ Q, 'does mutates the object';
ok C.^find_method('m').signature.params[1] ~~ Q,
    'the mixin persists across a fresh .^find_method read';
ok C.^find_method('m').signature.params[1] === C.^find_method('m').signature.params[1],
    'repeated .^find_method(...).signature.params reads share identity';

# Different declarations, and different classes' same-named methods, must
# never share a cache entry.
class D {
    method m1(:$a) { }
    method m2(:$b) { }
}
is D.^find_method('m1').signature.params[1].name, '$a',
    'an unrelated method keeps its own params';
is D.^find_method('m2').signature.params[1].name, '$b',
    'and does not collide with a same-shaped sibling method';

# A different CLASS's same-named method must not collide either (owner is
# part of the cache key, not just the method name).
class E {
    method m1(:$c) { }
}
is E.^find_method('m1').signature.params[1].name, '$c',
    'a same-named method on a different class keeps its own params too';
