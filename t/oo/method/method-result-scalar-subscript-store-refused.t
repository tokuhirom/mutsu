use Test;

# #9256: an element store into a method result that is a plain non-container
# value (`$s.Str[0] = 1`) is refused the way rakudo refuses it, instead of
# silently doing nothing.

plan 9;

{
    my $s = "ab";
    throws-like { $s.Str[0] = 1 }, X::Assignment::RO,
        message => 'Cannot modify an immutable Str (ab)', 'Str result, positional store';
    is $s, "ab", 'the receiver is unchanged';
}

{
    my $n = 5;
    throws-like { $n.Int[0] = 1 }, X::Assignment::RO,
        message => 'Cannot modify an immutable Int (5)', 'Int result, positional store';
    throws-like { $n.Rat[0] = 1 }, X::Assignment::RO,
        message => 'Cannot modify an immutable Rat (5)', 'Rat result, positional store';
    throws-like { $n.Str<k> = 1 }, Exception,
        message => 'Type Str does not support associative indexing.', 'Str result, associative store';
    is $n, 5, 'the receiver is unchanged';
}

# The container-returning forms keep working (#9208).
{
    my @a = 1, 2;
    @a.self[0] = 9;
    is-deeply @a, [9, 2], '.self on an Array stores into the receiver';
    my %h = a => 1;
    %h.Hash<b> = 2;
    is-deeply %h, {a => 1, b => 2}, '.Hash on a Hash stores into the receiver';
    my @c = 1, 2;
    @c.Array[0] = 9;
    is-deeply @c, [1, 2], '.Array stores into a discarded copy';
}
