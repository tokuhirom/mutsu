use Test;

plan 12;

# Slice F (env<->locals coherence, docs/env-locals-coherence.md): a mutating
# method on a value-typed receiver reassigns the receiver in env by name
# (e.g. `$s.push` on an `is Array`-backed instance, a Str `.=` style mutator).
# The CallMethodMut op now writes the updated receiver straight through to the
# caller's local slot, so subsequent reads of the receiver are coherent without
# the reverse `sync_locals_from_env` pull.

# --- is Array-backed instance: native array mutators -----------------------
{
    class Stack is Array {}
    my $s = Stack.new;
    $s.push(1);
    is $s.elems, 1, 'push on an Array-backed instance grows the receiver';
    $s.push(2, 3);
    is $s.join(','), '1,2,3', 'further push is coherent on the receiver';
    is $s.pop, 3, 'pop returns the last element';
    is $s.elems, 2, 'pop shrinks the receiver coherently';
    $s.unshift(0);
    is $s.join(','), '0,1,2', 'unshift prepends coherently';
}

# --- is Hash-backed instance: native hash mutator --------------------------
{
    class Bag2 is Hash {}
    my $h = Bag2.new;
    $h{'a'} = 1;
    $h{'b'} = 2;
    is $h<a>, 1, 'hash-backed instance element assign is coherent (a)';
    is $h.elems, 2, 'hash-backed instance grows coherently';
}

# --- repeated mutation then read interleaved -------------------------------
{
    class Stack2 is Array {}
    my $q = Stack2.new;
    $q.push(10);
    my $first = $q.elems;
    $q.push(20);
    my $second = $q.elems;
    is $first, 1, 'receiver coherent after first push';
    is $second, 2, 'receiver coherent after second push';
}

# --- plain array variable mutators (regression guard) ----------------------
{
    my @a = 1, 2, 3;
    @a.push(4);
    is @a.join(','), '1,2,3,4', 'plain array .push still propagates';
    @a.pop;
    is @a.join(','), '1,2,3', 'plain array .pop still propagates';
}

# --- mutating method whose receiver is read again in the caller ------------
{
    class Stack3 is Array {}
    my $st = Stack3.new;
    $st.append(5, 6, 7);
    is $st.elems, 3, 'append is coherent on the receiver';
}
