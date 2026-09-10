use v6;
use Test;

# `cell_store_preserving_container_identity` (vm_var_assign_ops.rs) mutates
# an existing ContainerRef cell's backing container IN PLACE when a whole
# array/hash reassignment reaches it, so aliases (`my @b := @a; @a = ...`)
# observe the update. Anonymous containers (`my @ = EXPR`, `my % = EXPR`)
# all compile to the single shared slot name `@__ANON_ARRAY__` /
# `%__ANON_HASH__` -- each successive declaration is a DISTINCT logical
# variable that merely reuses that name, so in-place-reassigning through a
# cell there silently aliased two unrelated declarations together whenever
# escape analysis had promoted the slot to a cell (reachable via a
# recursively-invoked closure that reads/writes a captured free variable --
# CBOR::Simple's `decode-array`, decoding a definite-length array nested
# inside an indefinite-length one, hit this exactly).

plan 3;

# Minimal reproduction of the CBOR::Simple bug: a closure declared and
# invoked once per recursive call of an outer sub, whose last expression is
# a naked `my @ = ...` built from a captured free variable, must not leak
# into a SIBLING invocation's result once both are collected together.
{
    sub cbor_like_decode(@bytes, $pos is rw) {
        my &decode = {
            my $b = @bytes[$pos++];
            $b == 0 ?? decode-array() !! $b
        };
        my &decode-array = {
            my $n = @bytes[$pos++];
            False ?? Nil !! my @ = (^$n).map(&decode)
        };
        decode()
    }
    my @bytes = 0, 2, 10, 20, 0, 2, 30, 40;
    my $pos = 0;
    my @result;
    @result.push(cbor_like_decode(@bytes, $pos));
    @result.push(cbor_like_decode(@bytes, $pos));
    is @result.raku, [[10, 20], [30, 40]].raku,
        'sibling recursive calls collecting a naked my @ = ... result do not alias';
}

# The actual CBOR::Simple shape that surfaced this (decode-array.rakumod:690):
# an indefinite-length array whose two elements are themselves definite-length
# arrays, each decoded through a separate recursive `cbor-decode()` call.
{
    use CBOR::Simple;
    sub hex-decode(Str:D $hex, $buf-type = buf8) {
        $buf-type.new($hex.comb(2).map(*.parse-base(16)))
    }
    # Encoding an inline `my @ = ...` array first is what makes the anonymous
    # slot escape-analysis-promote to a cell in the failing case (see the
    # ticket for why -- not otherwise load-bearing for the fix itself).
    cbor-encode((my @ = 1, 2, 3));
    is-deeply cbor-decode(hex-decode('9f01820203820405ff')), $[1, [2, 3], [4, 5]],
        'CBOR::Simple decode-array nested-array elements are not aliased';
}

# The same bug's downstream symptom (todo/tickets/cbor-simple-...): once the
# aliasing corrupted the first result, decoding continued into a stack
# overflow later in the same file. Run the actual upstream test file's
# equivalent assertion sequence leading up to it to guard against a
# regression reintroducing either half.
{
    use CBOR::Simple;
    sub hex-decode(Str:D $hex, $buf-type = buf8) {
        $buf-type.new($hex.comb(2).map(*.parse-base(16)))
    }
    is-deeply cbor-decode(hex-decode('9f018202039f0405ffff')), $[1, [2, 3], [4, 5]],
        'a second, differently-shaped nested-indefinite decode right after the first stays correct';
}
