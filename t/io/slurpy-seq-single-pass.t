use Test;

plan 8;

# Single-argument-rule slurpy (`+a` / `+@a`) type preservation + Seq single-pass
# consumption (S06-signature/slurpy-params.t 70-77).

# `+a` (sigilless single-arg-rule) passes a lone Seq through unchanged.
sub fa(+a) { a }
is fa((1, 2, 3).grep({ $_ })).WHAT.^name, 'Seq', '+a keeps a lone Seq a Seq';
is-deeply fa((1, 2, 3).grep({ $_ })), (1, 2, 3), '+a Seq has the right values';

# `+@a` (array single-arg-rule) exposes a lone Seq as a re-iterable List.
sub fat(+@a) { @a }
is fat((1, 2, 3).grep({ $_ })).WHAT.^name, 'List', '+@a turns a lone Seq into a List';
is-deeply fat((1, 2, 3).grep({ $_ })), (1, 2, 3), '+@a List has the right values';

# A bare Seq is single-shot: a second `for` over the same Seq throws
# X::Seq::Consumed; the first pass still completed.
my @result;
my $err;
{
    my \seq = fa((1, 2, 3).grep({ $_ }));
    push @result, $_ for seq;
    CATCH { default { $err = $_ } }
    push @result, $_ for seq;   # second pass dies
}
is-deeply @result, [1, 2, 3], 'first Seq pass completed before the second died';
ok $err.defined && $err.WHAT.^name eq 'X::Seq::Consumed', 'second Seq pass throws X::Seq::Consumed';

# The List from `+@a` is re-iterable (NOT single-shot).
my @r2;
my @l := fat((1, 2, 3).grep({ $_ }));
push @r2, $_ for @l;
push @r2, $_ for @l;
is-deeply @r2, [1, 2, 3, 1, 2, 3], '+@a List re-iterates fine';

# A Seq reified into an `@`-array is re-iterable too.
my @r3;
my @a = (1, 2, 3).grep({ $_ });
push @r3, $_ for @a;
push @r3, $_ for @a;
is-deeply @r3, [1, 2, 3, 1, 2, 3], 'Seq assigned into @-array is re-iterable';
