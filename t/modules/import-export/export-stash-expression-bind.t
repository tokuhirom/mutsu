use Test;

# A routine bound into an export tag's stash through an expression
# (`$WHO<DEFAULT>.WHO<&f> := ...`) is an export of that tag, as
# `EXPORT::DEFAULT::<&f> := ...` makes it (#15371, CompUnit::Util's
# `set-unit('EXPORT::DEFAULT::&f', ...)`). Checked against rakudo.

plan 3;

use lib $?FILE.IO.parent(2).sibling('lib').Str;

use StashExpressionExport;
is stash-exported(), 'from the stash', 'a DEFAULT export bound through .WHO';

{
    use StashExpressionExport :extra;
    is tagged-exported(), 'tagged', 'a tagged export bound through .WHO';
}

{
    my $pkg := Metamodel::PackageHOW.new_type(:name<Holder>);
    my $WHO := $pkg.WHO;
    $WHO<&inner> := sub { 'held' };
    is $pkg.WHO<&inner>(), 'held', 'a plain package stash still binds';
}
