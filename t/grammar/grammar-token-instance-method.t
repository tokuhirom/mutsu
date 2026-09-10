use v6;
use Test;

plan 6;

# Calling a grammar token as an instance method runs it against an
# empty-string cursor; the result smartmatches by the cursor's success
# (URI's missing-components: `'foo' ~~ IETF::...URI.new().TOP-non-empty`).
grammar G {
    token maybe-empty { a* }
    token non-empty   { a+ }
}

ok "anything" ~~ G.new.maybe-empty,
    'a token matching the empty string smartmatches True';
nok "anything" ~~ G.new.non-empty,
    'a token NOT matching the empty string smartmatches False';
ok "other" ~~ G.new.maybe-empty,
    'the LHS string does not affect the cursor verdict';

# The URI shape: qualified cross-module grammar rules.
grammar H {
    token TOP-non-empty { <.alpha>+ }
    token any-at-all { <.alpha>* }
}
nok 'foo' ~~ H.new().TOP-non-empty, 'non-empty rule rejects (URI shape)';
ok '#foo' ~~ H.new().any-at-all, 'empty-matching rule accepts (URI shape)';

# Real methods still win over tokens of the same name.
grammar K {
    token t { a }
    method regular() { 'method-result' }
}
is K.new.regular, 'method-result', 'real methods dispatch normally';
