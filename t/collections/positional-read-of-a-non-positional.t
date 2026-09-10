use Test;

plan 36;

# `$c[0]` and `$c{0}` are different questions. The `:exists` and value-adverb
# opcodes learned to carry the subscript's bracket to the runtime; the plain
# *read* did not, so it answered every `[...]` on an Associative as a key
# lookup: `(<a b>.Set)[0]` was the membership of the key `0`, not the Set.
#
# raku reads `[...]` on a value that does not do `Positional` through
# `Any.AT-POS`: the value is a one-element list holding itself, so index 0 is
# the value and every other index is an X::OutOfRange Failure.
#
# Every assertion here also passes unmodified under rakudo.

# --- a hash under a positional subscript is the hash itself ---
{
    my $h = { a => 1 };
    is-deeply $h[0], { a => 1 }, '$h[0] is the hash';
    # `.defined` on a Failure both answers False and marks it handled, so the
    # exception can be inspected afterwards without an unhandled-Failure warning.
    my $oor = $h[1];
    nok $oor.defined, '$h[1] is out of range (an undefined Failure)';
    is $oor.exception.^name, 'X::OutOfRange', 'carrying X::OutOfRange';

    # The `{...}` spelling is untouched: it is still a key lookup.
    nok $h{0}.defined, '$h{0} is still the (absent) key 0';
    is $h<a>, 1, 'and a real key still reads';
}

{
    my %h = a => 1;
    is-deeply %h[0], { a => 1 }, '%h[0] is the hash too';
    nok %h[1].defined, 'and %h[1] is out of range';
}

# The value comes back decontainerized, as `Any.AT-POS` hands it over: the
# itemization the `$` confers is not part of the element.
{
    my $c = { a => 1 };
    is $c[0].raku, '{:a(1)}', 'the read is decontainerized';
}

# --- the quanthashes are Associative but not Positional ---
{
    my $s = <a b>.Set;
    is-deeply $s[0], <a b>.Set, '$s[0] is the Set';
    nok $s[1].defined, '$s[1] is out of range';
    # `{...}` still asks about membership of the key.
    nok $s{0}, '$s{0} is still the membership of the key 0';
    ok $s<a>, 'and a real element is still a member';
}

{
    my $b = <a a b>.Bag;
    is-deeply $b[0], <a a b>.Bag, '$b[0] is the Bag';
    nok $b[1].defined, '$b[1] is out of range';
    is $b{'a'}, 2, 'and $b{"a"} is still the weight';
}

{
    my $m = (a => 1.5).Mix;
    is-deeply $m[0], (a => 1.5).Mix, '$m[0] is the Mix';
    nok $m[1].defined, '$m[1] is out of range';
}

# --- a plain scalar keeps the behaviour it already had ---
{
    my $i = 5;
    is $i[0], 5, '$i[0] is the value';
    nok $i[1].defined, '$i[1] is out of range';
}

# --- `[*]` asks a different question: every element of the value's own list ---
{
    my $h = { a => 1 };
    is-deeply $h[*].List, (a => 1,), '$h[*] is the hash pairs, not the hash';
    my $i = 5;
    is-deeply $i[*].List, (5,), 'and a scalar lists as itself';
    my $s = <a b>.Set;
    is $s[*].elems, 2, 'a Set lists its elements';
}

# --- a slice reads each index the same way ---
{
    my $h = { a => 1 };
    my @slice = $h[0..0];
    is-deeply @slice[0], { a => 1 }, 'a range slice reads slot 0';
    is @slice.elems, 1, 'and has one element';
}

# --- a `[...]` index is a NUMBER, so a string index numifies ---
# This is the other half of "the bracket decides": `$h["a"]` is not the key `a`
# under `[...]`, it is a failed `.Int` coercion.
{
    my @a = 10, 20, 30;
    is @a["1"], 20, 'a numeric string index numifies';
    is @a["1.9"], 20, 'and truncates the way .Int does';
    # A string that does not numify reads nothing. The two runtimes differ on
    # *when* the coercion failure surfaces — rakudo throws as the Failure is
    # bound, mutsu answers the Failure — so assert only what both agree on.
    nok (try @a["x"]).defined, 'a non-numeric string index reads nothing';

    my $h = { a => 1, '1' => 'one' };
    nok (try $h["a"]).defined, '$h["a"] is a coercion failure, not the key';
    nok (try $h["1"]).defined,
        'and a numeric string index reads the one-element list, not the key "1"';
    is $h<1>, 'one', 'while the associative spelling still finds that key';

    my $i = 5;
    is $i["0"], 5, 'a scalar reads slot "0" as slot 0';
}

# `[...]` on a type name is parameterization, not a subscript, so a string
# argument must not be numified: `role R[Str $s]` invoked as `R["x"]` still
# composes.
{
    my $composed = 'no';
    my role R[Str:D $s] { method tag { $s } }
    my class C does R["x"] { }
    $composed = C.tag;
    is $composed, 'x', 'a string role parameter is not read as an index';
}

# --- the native AT-POS mirrors its EXISTS-POS sibling ---
{
    my $h = { a => 1 };
    is-deeply $h.AT-POS(0), { a => 1 }, '.AT-POS(0) is the hash';
    nok $h.AT-POS(1).defined, '.AT-POS(1) is out of range';
    nok $h.EXISTS-POS(1), 'as .EXISTS-POS(1) already reported';
    is-deeply <a b>.Set.AT-POS(0), <a b>.Set, '.AT-POS(0) on a Set is the Set';
}
