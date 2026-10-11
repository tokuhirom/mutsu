unit class PkgRelType;
use PkgRelType::Leaf;
use PkgRelType::List;

sub leaf is export { Leaf.new.hi }
sub list-elems is export { List.new(1, 2, 3).elems }
