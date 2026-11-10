use Test;

# From the Red distribution (t/038-equal.rakutest): once a thread has been
# spawned, a `Mu $value` parameter bound to `Nil` read back a stale value of
# an unrelated variable named `value` from the name-keyed cross-thread store.

plan 4;

my $value = 1;
class Writer { method set { $value = 9 } }
await start { 1 };

sub snap(Mu $value) {
    return $value unless $value.DEFINITE;
    if $value ~~ Positional {
        my @items;
        @items.push(snap($_)) for $value.list;
        return @items;
    }
    $value
}
snap((5, 4, 0));

sub typed(Mu $value) { $value.raku }
sub untyped($value) { $value.raku }
is typed(Nil), "Nil", "Mu param bound to Nil stays Nil";
is untyped(Nil), "Nil", "untyped param bound to Nil stays Nil";
is typed(Any), "Any", "a type object is unchanged";
is-deeply snap((5, 4, 0)), [5, 4, 0], "recursion still sees real values";
