use v6;
use Test;

# An array-destructuring sub-signature (`-> [$a, $b, $c] { ... }`) must see
# through `$`-itemization the same way it already does for an itemized Array
# literal (`$(3, 4)`). An array/hash ELEMENT read wraps a value that has no
# `ArrayKind`-style itemized flag of its own (a `Seq` in particular) in a
# `Value::Scalar` box to mark it itemized -- `positional_values_from_unpack_target`
# used to stop at that box instead of looking through it, reporting "Too few
# positional arguments in sub-signature binding" for a value that plainly had
# three elements. Found via Acme::Insult::Lala's
# `%?RESOURCES<lala.txt>.lines>>.split(/\s+/).map(-> [$a, $b, $c] { ... })`.

plan 4;

{
    my $f = -> [$a, $b, $c] { "$a-$b-$c" };
    my $s = "a b c".split(/\s+/);
    is $f($s), 'a-b-c', 'destructure a plain (non-itemized) Seq';
}

{
    my $f = -> [$a, $b, $c] { "$a-$b-$c" };
    my @arr;
    @arr[0] = "a b c".split(/\s+/);
    is $f(@arr[0]), 'a-b-c', 'destructure an itemized Seq read from an array element';
}

{
    my @lines = ("a b c", "d e f")>>.split(/\s+/);
    my @pairs = @lines.map(-> [$a, $b, $c] { a => $a, b => $b, c => $c });
    is @pairs.map(*.raku).join(' '), '$(:a("a"), :b("b"), :c("c")) $(:a("d"), :b("e"), :c("f"))',
        'destructuring map over a hyper-split result (Acme::Insult::Lala TWEAK shape)';
}

{
    # `$(3, 4)` (itemized Array) already worked -- pinning it alongside the
    # itemized-Seq case above so a future regression shows both at once.
    my $f = -> [$a, $b] { $a + $b };
    is $f($(3, 4)), 7, 'destructure an itemized Array literal still works';
}
