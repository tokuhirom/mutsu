# From the Protocol distribution: a Metamodel::SubsetHOW + ClassHOW metaclass
# whose new_type refines `Any` with a method-set predicate.
unit class ProtoHOW is Metamodel::SubsetHOW is Metamodel::ClassHOW;
method new_type(:$name = "<anon>") {
    my $type;
    $type := callwith :$name, :refinee(Any), :refinement{ .^methods>>.name.any eq $type.^methods>>.name.all };
    $type
}
