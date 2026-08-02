unit module NamedArgStmtCall;

# A module routine that takes a container into a plain `$` parameter and also
# accepts named arguments, so a statement-level call to it carries a Pair and
# compiles to the named-argument statement-call opcode.
sub takes-container($got, $expected, $reason = '', *%opts) is export {
    $got.elems
}
