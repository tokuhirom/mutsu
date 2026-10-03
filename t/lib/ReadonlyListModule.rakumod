unit module ReadonlyListModule;

my @names is List = <a b>;

sub assign-module-names() is export { @names = 1 }
