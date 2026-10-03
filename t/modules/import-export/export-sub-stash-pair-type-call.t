use v6;
use lib 't/lib';
use Test;

# A `sub EXPORT` that returns `UNIT::{$_}:p` pairs hands each value over in
# its stash container; the imported type must still be the type object
# itself, so a call-form `Lic("x")` dispatches to its CALL-ME.

use ExportStashPairType;

plan 3;

is Lic("0BSD"), 'called Lic(0BSD)', 'call form of an EXPORT-ed my class';
is Ack("x"), 'called Ack(x)', 'every exported class';
is Lic.^name, 'Lic', 'the import is the type object';
