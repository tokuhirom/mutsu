unit module TagExp;
sub always() is export { return "always" }
sub internal($x) is export(:internals) { return "internal-$x" }
