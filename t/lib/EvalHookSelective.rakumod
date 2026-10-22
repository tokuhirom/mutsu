my $runs = 0;
my sub after(Str $x) { "a$x" }
my sub before(Str $x) { "b$x" }
sub hook-runs() is export { $runs }
my sub EXPORT(*@names) {
    $runs++;
    Map.new: @names.map: { if UNIT::{"&$_"}:exists { UNIT::{"&$_"}:p } }
}
