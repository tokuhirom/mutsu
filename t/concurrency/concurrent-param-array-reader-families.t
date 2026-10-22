use Test;

# The `@x` twin of concurrent-param-hash-reader-families.t (#13079, #13632).
# Eight threads share one parameter array that a `start` captures. Half of
# them restructure it: push and pop, overwrite, unshift and shift, append and
# splice. The other half read it, each family both directly and through a
# routine call. Unsynchronized concurrent mutation has no defined answer in
# Raku, so these pin MUTSU's guarantee, not a Raku one: every shape finishes,
# never a panic or a corrupted heap. `.gist` walked a node another thread was
# shrinking (`gist_head` tested the length, then sliced from a `head` the
# writer had moved past the new end) and died with "range start index 51 out
# of range for slice of length 0" in about one release run in six.

my @families =
    'boolean test'     => -> @x { so @x },
    'numeric coercion' => -> @x { +@x },
    'stringification'  => -> @x { ~@x },
    'iteration'        => -> @x { my $n = 0; $n++ for @x; $n },
    'element read'     => -> @x { @x[3] },
    'last element'     => -> @x { @x[*-1] },
    'exists'           => -> @x { @x[3]:exists },
    'slice'            => -> @x { @x[1, 2, 3].elems },
    'zen slice'        => -> @x { @x[*].elems },
    '.elems'           => -> @x { @x.elems },
    '.end'             => -> @x { @x.end },
    '.sort'            => -> @x { @x.sort.elems },
    '.grep'            => -> @x { @x.grep(* > 3).elems },
    '.map'             => -> @x { @x.map(* + 1).elems },
    '.reverse'         => -> @x { @x.reverse.elems },
    '.Array'           => -> @x { @x.Array.elems },
    '.List'            => -> @x { @x.List.elems },
    '.clone'           => -> @x { @x.clone.elems },
    'copy assignment'  => -> @x { my @c = @x; @c.elems },
    'slip into a call' => -> @x { sub n(*@n) { @n.elems }; n(|@x) },
    '.raku'            => -> @x { @x.raku.chars },
    '.gist'            => -> @x { @x.gist.chars },
    'eqv'              => -> @x { @x eqv @x },
    '.sum'             => -> @x { @x.sum },
    '.max'             => -> @x { @x.max },
    '.join'            => -> @x { @x.join(",").chars },
    'sort with cmp'    => -> @x { @x.sort({ $^a <=> $^b }).elems },
    'sort by'          => -> @x { @x.sort(*.Int).elems },
    'index assign'     => -> @x { @x[2] = 5; @x.elems },
    'index assign end' => -> @x { @x[@x.end] = 1; @x.elems },
    'index assign var' => -> @x { my $i = 3; @x[$i] = 7; @x.elems },
    'index assign cmp' => -> @x { my $i = @x.elems - 1; @x[$i] = 2; @x.elems },
    'multi assign'     => -> @x { (@x[0], @x[1]) = (1, 2); @x.elems },
    'append'           => -> @x { @x.append(1); @x.elems },
    'rotate'           => -> @x { @x.rotate(1).elems },
    'first'            => -> @x { @x.first(* > 3).defined },
    'any'              => -> @x { so @x.any > 3 },
;

plan @families.elems + 1;

sub through-call(@x, &read) { read(@x) }

sub drive(&read) {
    sub go(@x) {
        await (^8).map: -> $i {
            start {
                for ^120 -> $k {
                    if $i %% 2 {
                        read(@x);
                        through-call(@x, &read);
                    }
                    elsif $i == 1 { @x.push($k); @x.pop }
                    elsif $i == 3 { @x[$k % 50] = $k }
                    elsif $i == 5 { @x.unshift($k); @x.shift }
                    else          { @x.append($k, $k); @x.splice(0, 2) }
                }
            }
        }
    }
    my @b = ^20;
    go(@b);
    @b.elems
}

for @families -> $family {
    ok drive($family.value).defined, $family.key;
}

# A plain Array stays the array itself once threads exist: the guarded reads
# must not answer a copy where identity is observable.
{
    my @a = 1, 2;
    await start { 1 };
    @a.push(3);
    is-deeply @a, [1, 2, 3], 'an Array is still the array after threads exist';
}
