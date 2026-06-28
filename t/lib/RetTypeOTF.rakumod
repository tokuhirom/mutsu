unit module RetTypeOTF;

my @LOG;

# A module-private (non-exported) sibling sub with a return type.
sub helper(Int $n --> Int:D) { @LOG.push($n); $n * 2 }

# An exported sub with a non-coercion return type that calls the private
# sibling and reads a module-level lexical. Both must OTF-compile and still
# resolve `helper` / `@LOG`, and the `--> Int:D` return-check must hold.
sub pub(Int $n --> Int:D) is export {
    my $r = helper($n);
    $r + @LOG.elems
}
