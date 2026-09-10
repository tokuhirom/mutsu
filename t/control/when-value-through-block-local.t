use Test;

# A `when` that matches leaves its topicalizer with the block's value. When the
# enclosing `when`/`default`/`given` body happens to declare a `my`, that body
# runs under the block-local scope opcode — which used to swallow the succeed
# signal, so the whole construct evaluated to Nil instead of the matched value.

plan 8;

sub in-when($d) {
    given 'section' {
        when 'section' {
            my $extra = 1;
            if $d -> $_ {
                when Associative { 'ASSOC' }
                when Iterable | Positional { 'ITER' }
                default { 'DEF' }
            }
            else {
                'EMPTY'
            }
        }
    }
}

is in-when({ a => 1 }), 'ASSOC', 'when body with a `my`: Associative arm';
is in-when([1, 2]), 'ITER', 'when body with a `my`: Iterable arm';
is in-when('s'), 'DEF', 'when body with a `my`: default arm';
is in-when(0), 'EMPTY', 'when body with a `my`: else branch still wins';

sub in-given() {
    given 'x' {
        my $extra = 1;
        when 'x' { 'GIVEN' }
    }
}
is in-given(), 'GIVEN', 'given body with a `my` keeps the when value';

sub in-default() {
    given 'x' {
        default {
            my $extra = 1;
            if 5 -> $_ {
                when Int { 'DEFAULT' }
            }
        }
    }
}
is in-default(), 'DEFAULT', 'default body with a `my` keeps the when value';

sub direct() {
    given 'x' {
        when 'x' {
            my $extra = 1;
            'DIRECT'
        }
    }
}
is direct(), 'DIRECT', 'a when body with a `my` still yields its last value';

# The block-local scope must still restore a shadowed outer lexical.
my $shadowed = 'outer';
given 'x' {
    when 'x' {
        my $shadowed = 'inner';
    }
}
is $shadowed, 'outer', 'a when-body `my` does not leak out';
