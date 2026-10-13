use Test;

# #13016's acceptance harness, widened by #13079 to the shape of its `%h`
# twin (concurrent-param-hash-reader-families.t). Eight threads share one
# parameter array that reaches them through a nested routine's closure (a
# `:=`-style cell once a `start` captures it). Half of them restructure it
# (`push`/`shift`, `splice`, element `:delete`, `unshift`/`pop`); the other
# half run one reader family, both directly and through a routine call whose
# `@x` parameter writes back into the caller's cell on return. Unsynchronized
# concurrent mutation has no defined answer in Raku, so these pin MUTSU's
# guarantee, not a Raku one: every shape finishes, never a panic or a
# corrupted heap. Before ADR-0068 §20 made a snapshot the default for a read
# of a shared container, 22 of these families died with `range start index N
# out of range for slice of length 0`, nanbox `classify` panics or a `sort_by`
# total-order panic.

my @families =
    'boolean test'     => -> @x { so @x },
    'numeric coercion' => -> @x { +@x },
    'stringification'  => -> @x { ~@x },
    'iteration'        => -> @x { my $n = 0; $n++ for @x; $n },
    'element read'     => -> @x { @x[0] },
    'computed index'   => -> @x { my $i = 1 + 2; @x[$i] },
    'range slice'      => -> @x { @x[1..3].elems },
    'list slice'       => -> @x { @x[0,1,2].elems },
    'whatever index'   => -> @x { @x[*-1] },
    'zen slice'        => -> @x { @x[*].elems },
    ':exists'          => -> @x { @x[3]:exists },
    '.elems'           => -> @x { @x.elems },
    '.List'            => -> @x { @x.List.elems },
    '.Array'           => -> @x { @x.Array.elems },
    '.list'            => -> @x { @x.list.elems },
    '.sort'            => -> @x { @x.sort.elems },
    '.head'            => -> @x { @x.head(2).elems },
    '.tail'            => -> @x { @x.tail(2).elems },
    '.kv'              => -> @x { @x.kv.elems },
    '.pairs'           => -> @x { @x.pairs.elems },
    '.keys'            => -> @x { @x.keys.elems },
    '.values'          => -> @x { @x.values.elems },
    '.grep'            => -> @x { @x.grep(* > 3).elems },
    '.map'             => -> @x { @x.map(* + 1).elems },
    '.first'           => -> @x { @x.first(* > 3) },
    '.max'             => -> @x { @x.max },
    '.min'             => -> @x { @x.min },
    '.sum'             => -> @x { @x.sum },
    '.reverse'         => -> @x { @x.reverse.elems },
    '.join'            => -> @x { @x.join(",").chars },
    '.clone'           => -> @x { @x.clone.elems },
    '.Set'             => -> @x { @x.Set.elems },
    '(elem)'           => -> @x { 3 (elem) @x },
    'copy assignment'  => -> @x { my @c = @x; @c.elems },
    'slip into a call' => -> @x { sub n(*@n) { @n.elems }; n(|@x) },
    '.raku'            => -> @x { @x.raku.chars },
    '.gist'            => -> @x { @x.gist.chars },
    'eqv'              => -> @x { @x eqv @x },
    '==='              => -> @x { @x === @x },
    'smartmatch'       => -> @x { @x ~~ Positional },
    'reduction'        => -> @x { [+] @x },
    'hyper operator'   => -> @x { (@x >>+>> 1).elems },
    'zip'              => -> @x { (@x Z @x).elems },
    'slip into a list' => -> @x { (|@x, 1).elems },
;

plan @families.elems + 4;

sub through-call(@x, &read) { read(@x) }

sub drive(&read) {
    sub go(@x) {
        await (^8).map: -> $i {
            start {
                for ^300 -> $k {
                    if $i %% 2 {
                        read(@x);
                        through-call(@x, &read);
                    }
                    elsif $i == 1 { @x.push($k); @x.shift }
                    elsif $i == 3 { @x.splice(1, 1, $k) }
                    elsif $i == 5 { @x[2]:delete; @x[2] = $k }
                    else          { @x.unshift($k); @x.pop }
                }
            }
        }
    }
    my @b = ^20;
    go(@b);
    @b.elems
}

for @families -> $family {
    ok drive($family.value), $family.key;
}

# The writeback itself: a routine that returns without touching its `@x`
# must not put back an older copy over another thread's pushes.
{
    my @b = ^20;
    sub rd(@x) { @x.elems }
    sub go(@x) {
        await (^8).map: -> $i {
            start { for ^250 -> $k { if $i %% 2 { rd(@x) } else { @x.push($k) } } }
        }
    }
    go(@b);
    is @b.elems, 20 + 4 * 250, 'a non-mutating callee loses no concurrent push';
}

# Sequential use of the same shape still answers exactly.
{
    my @solo = ^5;
    sub bump(@x) { @x.push(5); @x.splice(0, 1) }
    bump(@solo);
    is-deeply @solo, [1, 2, 3, 4, 5], 'single-threaded, the same calls answer exactly';
}

# Once a thread exists, the reads that answer the receiver itself, or hand
# out its element containers, still do: they run on the live array, not on a
# snapshot of it.
{
    my @a = 1, 2, 3;
    await start { 1 };
    ok @a.list === @a, '.list on an Array is still the array itself';
    $_ *= 2 for @a.values;
    for @a.kv -> $k, $v is rw { $v++ }
    is-deeply @a, [3, 5, 7], '.values and .kv still alias the live elements';
}
