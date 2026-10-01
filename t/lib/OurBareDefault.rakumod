unit module OurBareDefault;
our $x;
sub get-x() is export { $x }
our sub get-qualified() { $OurBareDefault::x }
