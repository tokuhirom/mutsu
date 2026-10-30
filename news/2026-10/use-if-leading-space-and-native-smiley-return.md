# `:if( cond)` with a leading space, and `--> buf8:D`

Two parse gaps found loading Net::BGP:

- the `if` pragma's adverb took no whitespace after its `(`: `use Mod:if( $*PERL.compiler.version < v2018.12)`
  failed "Undeclared routine ... :if" once `use if;` was active;
- a lowercase native type with a smiley as the return constraint (`--> buf8:D`) was read as a definite
  return value, so any `return` in the body died "No return arguments allowed".

Net::BGP still needs #15309 (`byte` dispatch) and #15311 (`Net::BGP::Peer` attribute constraint).
