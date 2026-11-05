use lib 't/lib';
use Test;
use UserReplRole::Mod;

# Found via the REPL distribution (`role REPL`, which also calls
# `nqp::getcomp("Raku")` through CodeUnit): mutsu registers rakudo's core `REPL`
# class on first use of that name or of `nqp::getcomp`, and that registration
# collided with the program's own `role REPL` -> "Redeclaration of symbol 'REPL'".
# The program's own `REPL` wins, as it does in rakudo.

plan 5;

is REPL.^name, 'REPL', 'REPL names the user role';
is make-repl.hi, "hi from the user's REPL", 'REPL.new builds the user role';
is make-repl(:header<x>).header, 'x', "and the user role's attribute works";
is compiler-name, 'Raku', 'nqp::getcomp("Raku") still answers the compiler object';
isa-ok make-repl, REPL, 'an instance does the user role';
