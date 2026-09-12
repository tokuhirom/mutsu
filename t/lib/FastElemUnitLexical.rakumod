unit module FastElemUnitLexical;

# A compunit's own file-scope `@` lives in the `unit_lexicals` cell, not in the
# bare env key of whatever scope loaded this module. `set-slot` must therefore
# write THIS array, even when the loading script has a same-named `@roster` of
# its own.
my @roster = 0 xx 3;

sub set-slot($i, $v) is export {
    @roster[$i] = $v;
}

sub read-slot($i) is export {
    @roster[$i];
}
