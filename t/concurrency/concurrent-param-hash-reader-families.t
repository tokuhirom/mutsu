use Test;

# The `%h` twin of concurrent-param-array-reader-families.t (#13079). Eight
# threads share one parameter hash that a `start` captures. Half of them
# restructure it: insert and delete, overwrite, delete and re-add, `.push`.
# The other half read it, each family both directly and through a routine
# call. ADR-12814 follows Rakudo: reads racing with structural writes have no
# memory-safety guarantee. Protect each access to pin the guarantee for a
# synchronized program, including each read family through a parameter.

my @families =
    'boolean test'     => -> %x { so %x },
    'numeric coercion' => -> %x { +%x },
    'stringification'  => -> %x { ~%x },
    'iteration'        => -> %x { my $n = 0; $n++ for %x; $n },
    'element read'     => -> %x { %x<k3> },
    ':exists'          => -> %x { %x<k3>:exists },
    ':k adverb'        => -> %x { %x<k1>:k },
    'computed key'     => -> %x { my $k = "k" ~ 3; %x{$k} },
    'key slice'        => -> %x { %x<k1 k2 k3>.elems },
    'zen slice'        => -> %x { %x{*}.elems },
    '.keys'            => -> %x { %x.keys.elems },
    '.values'          => -> %x { %x.values.elems },
    '.kv'              => -> %x { %x.kv.elems },
    '.pairs'           => -> %x { %x.pairs.elems },
    '.antipairs'       => -> %x { %x.antipairs.elems },
    '.elems'           => -> %x { %x.elems },
    '.sort'            => -> %x { %x.sort.elems },
    '.grep'            => -> %x { %x.grep(*.value > 3).elems },
    '.map'             => -> %x { %x.map(*.key).elems },
    '.Hash'            => -> %x { %x.Hash.elems },
    '.Map'             => -> %x { %x.Map.elems },
    '.clone'           => -> %x { %x.clone.elems },
    '%(...)'           => -> %x { %(%x).elems },
    'copy assignment'  => -> %x { my %c = %x; %c.elems },
    'slip into a call' => -> %x { sub n(*%n) { %n.elems }; n(|%x) },
    '.raku'            => -> %x { %x.raku.chars },
    '.gist'            => -> %x { %x.gist.chars },
    'eqv'              => -> %x { %x eqv %x },
    'smartmatch'       => -> %x { %x ~~ Associative },
    '.values.sum'      => -> %x { %x.values.sum },
    '.keys.max'        => -> %x { %x.keys.max },
;

plan @families.elems + 1;

sub through-call(%x, &read) { read(%x) }

sub drive(&read) {
    sub go(%x) {
        my $lock = Lock.new;
        await (^8).map: -> $i {
            start {
                for ^300 -> $k {
                    if $i %% 2 {
                        $lock.protect: { read(%x) };
                        $lock.protect: { through-call(%x, &read) };
                    }
                    else {
                        $lock.protect: {
                            if    $i == 1 { %x{"k$k"} = $k; %x{"k$k"}:delete }
                            elsif $i == 3 { %x{"n" ~ ($k % 50)} = $k }
                            elsif $i == 5 { %x<a>:delete; %x<a> = $k }
                            else          { %x.push("p$k" => $k); %x{"p$k"}:delete }
                        };
                    }
                }
            }
        }
    }
    my %b = (^20).map({ "k$_" => $_ });
    go(%b);
    %b.elems
}

for @families -> $family {
    ok drive($family.value), $family.key;
}

# A plain Hash coerces to itself, also once threads exist: the guarded
# `.Hash` must not answer a copy.
{
    my %h = a => 1;
    await start { 1 };
    %h.Hash<b> = 2;
    is-deeply %h, %(a => 1, b => 2), '.Hash on a Hash is still the hash itself';
}
