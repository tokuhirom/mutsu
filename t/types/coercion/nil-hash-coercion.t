use Test;

# `.hash` is `Any`'s Associative coercion, which `Nil` (an `Any`-derived
# undefined value) genuinely inherits: `Nil.hash` is the empty Hash `{}`, not
# an absorbed Nil the way an undeclared method would be. Found via
# Router::Right's `url()`, which calls `%( Nil )` (the parser's lowering of
# hash-context-paren-syntax to `.hash`) as a "no extra params" default.
is Nil.hash.WHAT, Hash, 'Nil.hash is a Hash';
is-deeply Nil.hash, {}, 'Nil.hash is the empty hash';

{
    my $x := Nil;
    is $x.hash.WHAT, Hash, 'a bound Nil also coerces .hash to an empty Hash';
}

sub takes-hash(Str $name, Hash $argh) {
    $argh;
}
is-deeply takes-hash('x', %( Nil )), {}, '%( Nil ) binds a Hash-typed parameter';

done-testing;
