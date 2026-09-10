use v6;
use Test;

plan 4;

# A bare type object used as a key on a plain (Str-keyed) hash ATTRIBUTE
# accessor must coerce to "" (with the "uninitialized value in string
# context" warning), matching the lookup path — not the gist form "(Str)".
# DBDish::Pg keys its Converter hash by type objects ($dbh.Converter{YesNo}).

quietly {
    class K { has %.c; }
    my $k = K.new;
    $k.c{Str} = 42;
    is $k.c{Str}, 42, 'assign and lookup via type-object key agree';
    is $k.c.keys.raku, '("",).Seq', 'type-object key stores as the empty string';

    # The DBIish converter shape: store a sub under a type-object key, call it.
    my $e = "Yes";
    $k.c{Int} = sub ($v) { "$v-$e" };
    $e = "No";
    is $k.c{Int}("x"), "x-No", 'sub stored under type-object key is found and sees live captures';

    $k.c{Str}:delete;
    is $k.c{Str}:exists, False, 'delete via type-object key removes the "" entry';
}
