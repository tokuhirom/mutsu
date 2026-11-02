unit module BuildThunkCaller;
# Calls an attribute's `.build` thunk from a unit that never imported the
# declaring file's types (Red's MetamodelX::Red::Dirtable does this).
sub run-attr-build($attr) is export {
    my $b = $attr.build;
    $b ~~ Callable ?? $b.(Mu, Mu) !! $b
}
