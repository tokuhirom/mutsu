use v6;
use Test;

# A package-qualified scalar (`$Pkg::var`, `$OUR::var`, `$GLOBAL::var`) is
# interpolated into a regex like any other `$var` (ADR-12917).

plan 7;

our $pk = "q";
package P { our $v = "q" }

ok "q" ~~ / $OUR::pk /, '$OUR::pk';
ok "q" ~~ / $GLOBAL::pk /, '$GLOBAL::pk';
ok "q" ~~ / $P::v /, '$Pkg::v';
nok "r" ~~ / $P::v /, '$Pkg::v does not match another string';
ok "q" ~~ / $P::v { } /, '$Pkg::v before a code block';
ok "qq" ~~ / ^ $P::v+ $ /, 'quantified';
is ("xqy" ~~ / x $P::v y /).Str, "xqy", 'inside a sequence';
