# Fixture (#17035): a plain `my @` lazy declaration as a module's last statement.
unit module LazyOurArrayMy;
our sub first-ten is export { my @m = 1 ... *; @m[^10] }
my @private = 1, 2, * + * ... *;
