use v6;
use Test;
use lib 't/lib-regex-late-classified';

# Regression (ValueClass, #16389): a unit that imports a module with
# `sub EXPORT` classifies its barewords late, and a user type named inside the
# `{ ... }` of a quoted atom of a regex literal reached the compiler still
# wrapped in `Expr::Spelled` (a panic in debug builds).
use ExportSubOnly;

plan 4;

class Q {}
class Bla {}

ok "(Q)" ~~ /"({ Q.^name })"/, 'type method call in a quoted block';
ok "Bla" ~~ /"{ Bla.^name }"/, 'second type, same shape';
my $m = "x" ~~ /"{ Q.^name }"/;
nok $m, 'a non-matching subject does not match';
is ("Q" ~~ /"{ Q.^name }"/).Str, 'Q', 'the block value interpolates';
