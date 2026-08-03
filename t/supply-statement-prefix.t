use Test;

plan 5;

# `supply` is a statement prefix (S06): `supply STATEMENT` is `supply { STATEMENT }`,
# so an `emit` in that statement belongs to the new supply rather than escaping.
# Cro's Cro::HTTP::Router::DelegateHandler.invoke opens its pipeline with
# `my $current = supply emit $req;`.

{
    my $s = supply emit 42;
    my @got;
    $s.tap(-> $v { @got.push($v) });
    is @got, [42], 'supply emit EXPR emits once per tap';
}

{
    my $s = supply for 1..3 { emit $_ * 10 }
    my @got;
    $s.tap(-> $v { @got.push($v) });
    is @got, [10, 20, 30], 'supply for LOOP emits each iteration';
}

{
    # On-demand: the body runs again for every tap.
    my $runs = 0;
    my $s = supply emit ++$runs;
    my @first;
    $s.tap(-> $v { @first.push($v) });
    my @second;
    $s.tap(-> $v { @second.push($v) });
    is @first, [1], 'first tap runs the body';
    is @second, [2], 'second tap runs the body again';
}

{
    # The statement-prefix form composes with a downstream transform.
    my $src = supply emit 'REQ';
    my $out = supply whenever $src -> $v { emit "wrapped($v)" };
    my @got;
    $out.tap(-> $v { @got.push($v) });
    is @got, ['wrapped(REQ)'], 'a statement-prefix supply feeds a whenever chain';
}
