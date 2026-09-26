use Test;

# A `whenever` with a declared signature binds the emitted value only
# through that signature: `$_` in the body stays the enclosing lexical
# topic, as for the same block passed to `.tap` (#9595). A bare block
# still gets the value as its topic.

plan 8;

{
    $_ = 'outer';
    my @seen;
    react whenever Supply.from-list(1, 2) -> $x { @seen.push: "$_:$x" }
    is-deeply @seen, ['outer:1', 'outer:2'], 'pointy param leaves the outer $_ alone';
}

{
    $_ = 'outer';
    my @seen;
    react whenever Supply.from-list(7) { @seen.push: $^v; @seen.push: $_ }
    is-deeply @seen, [7, 'outer'], 'placeholder param leaves the outer $_ alone';
}

{
    $_ = 'outer';
    my @seen;
    react whenever Supply.from-list(3) { @seen.push: $_ }
    is-deeply @seen, [3], 'bare block still topicalizes the emitted value';
}

{
    my @seen;
    react whenever Supply.from-list((a => 1),) -> $p { @seen.push: $p }
    is-deeply @seen, [a => 1], 'an emitted Pair binds positionally to -> $p';
}

{
    my @seen;
    react whenever Supply.from-list((a => 1), (b => 2)) -> Pair $p, $q? {
        @seen.push: $p.key;
        @seen.push: $q.defined;
    }
    is-deeply @seen, ['a', False, 'b', False], 'an emitted Pair binds to a typed param with an optional';
}

{
    my @seen;
    react whenever Supply.from-list((k => 'v'),) -> (:$key, :$value) {
        @seen.push: "$key=$value";
    }
    is-deeply @seen, ['k=v'], 'an emitted Pair unpacks through a sub-signature';
}

{
    my @seen;
    react whenever Supply.from-list((a => 1),) { @seen.push: $_ }
    is-deeply @seen, [a => 1], 'an emitted Pair is a bare block topic';
}

{
    sub first-value() {
        react whenever Supply.from-list(1, 2) -> $x { return $x }
        'fell through'
    }
    is first-value(), 1, 'return in a whenever body returns from the enclosing routine';
}
