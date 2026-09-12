use Test;

plan 5;

# The container half of ADR-0055's dichotomy (`needs_cell_unvouched_containers`)
# is delivered at the DECLARATION site -- `exec_set_local_op` ->
# `box_decl_local_container_cell` -- rather than at each capture, because boxing
# per capture is what ADR-0039 measured as too expensive.
#
# A PARAMETER has no declaration store, so that delivery site never ran for one:
# an `@`/`%` parameter that an escaping closure captures, and that the frame
# cannot vouch for (it was handed to a call, so an `is rw` param could write it
# back), reached NEITHER half of the dichotomy. Its capture then resolved by
# name up the live frame chain and a same-named container in the calling frame
# won.
#
# Found in Algorithm::LCS 0.1.1 (t/01-basic.rakutest): `lcs`'s default
# `&compare-i` closes over `lcs`'s `@a`/`@b` parameters, but runs inside
# `strip-prefix`, whose own `@a`/`@b` hold the REVERSED arrays -- so the
# comparator indexed the wrong sequences and 6 of 17 assertions failed.

{
    my sub inner(@a, &cb) { &cb() }
    my sub top(@a) {
        my &cb = -> { @a.join(' ') };
        inner(@a, &cb);                      # makes @a a call-arg source
        inner(@a.reverse, -> { &cb() });     # callee's @a is a DIFFERENT array
    }
    is top(<A B C D>), 'A B C D',
        'an `@` parameter capture is not hijacked by a same-named callee parameter';
}

{
    my sub inner(%h, &cb) { &cb() }
    my sub top(%h) {
        my &cb = -> { %h<k> };
        inner(%h, &cb);
        inner({ k => 'WRONG' }, -> { &cb() });
    }
    is top({ k => 'CORRECT' }), 'CORRECT',
        'a `%` parameter capture is not hijacked by a same-named callee parameter';
}

# An ordinary `my @a` already took the decl-site cell -- pin that it still does.
{
    my sub inner(@a, &cb) { &cb() }
    my sub top() {
        my @a = <A B C D>;
        my &cb = -> { @a.join(' ') };
        inner(@a, &cb);
        inner(@a.reverse, -> { &cb() });
    }
    is top(), 'A B C D',
        'a `my @` declaration keeps resolving to its own binding';
}

# The cell must not freeze the array: a later push is visible to the capture.
{
    my sub inner(@a, &cb) { &cb() }
    my sub top(@a) {
        my &cb = -> { @a.join(' ') };
        inner(@a, &cb);
        @a.push('E');
        inner(@a.reverse, -> { &cb() });
    }
    is top([<A B C D>]), 'A B C D E',
        'the captured `@` parameter still observes a later push';
}

# Two levels of closure nesting between the capture and the call -- the shape
# Algorithm::LCS actually has (lcs -> strip-suffix -> strip-prefix).
{
    my sub inner(@a, @b, &cb) { &cb() }
    my sub mid(@a, @b, &cb) { inner(@a.reverse, @b.reverse, -> { &cb() }) }
    my sub top(@a, @b) {
        my &cb = -> { @a.join('') ~ '/' ~ @b.join('') };
        inner(@a, @b, &cb);
        mid(@a[1 .. *], @b[1 .. *], -> { &cb() });
    }
    is top(<A B C D>, <P Q R>), 'ABCD/PQR',
        'the capture survives two nested closures over same-named containers';
}
