use Test;
use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;

# A `.= new` initializer constructs the declared type itself: the definiteness
# smiley qualifies the variable, not the constructed type, and a parenthesised
# anonymous `$` declaration is one expression (not a one-element list).
# Checked against rakudo, as source and as EVAL($ast).

plan 6;

for ('source' => -> $code { EVAL($code) }, 'AST' => -> $code { EVAL($code.AST) }) {
    my ($how, &run) = .key, .value;
    is-deeply run(Q[(my Int:D \b .= new: 42)]), 42, "sigilless Int:D term ($how)";
    is-deeply run(Q[(my Int $  .= new: 42)]), 42, "parenthesised anonymous scalar ($how)";
    is-deeply run(Q[(my Int:D $  .= new: 7)]), 7, "anonymous :D scalar ($how)";
}
