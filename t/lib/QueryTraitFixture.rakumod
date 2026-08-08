role Query { }

multi trait_mod:<is>(Parameter:D $p, :$query! --> Nil) is export {
    $p does Query;
}
