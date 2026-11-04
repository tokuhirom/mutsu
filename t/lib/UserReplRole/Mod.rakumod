use nqp;

# What the REPL distribution does: declare its own `role REPL`, and reach
# `nqp::getcomp("Raku")` (through CodeUnit) while it is in use.
role REPL:ver<0.0.1>:auth<zef:test> {
    has $.header;
    method hi { "hi from the user's REPL" }
}

sub compiler-name is export { nqp::getcomp("Raku").language_name }
sub make-repl(:$header = "h") is export { REPL.new(:$header) }
