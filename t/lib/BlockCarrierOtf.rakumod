unit module BlockCarrierOtf;

# A module sub with its OWN nested sub, whose compiled `CompiledFns` table
# is what a naive dispatch would substitute for a captured block's table
# too (ADR-0019 C6e-3c, the SubData carrier gap).
sub helper-in-module {
    "from-module";
}

sub run-it(&blk) is export {
    helper-in-module();
    blk();
}
