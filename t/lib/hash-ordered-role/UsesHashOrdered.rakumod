unit module UsesHashOrdered;
use Hash::Ordered;
sub role-name is export { Hash::Ordered.^name }
