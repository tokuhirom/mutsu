use v6;
use Test;

# Regressions found while making Zef::Identity's `value` grammar rule parse.
# Two independent regex-engine bugs:
#   (1) The LTM separator (`%%`/`%`) pre-pass stripped ALL whitespace, corrupting
#       significant whitespace inside `<...>` assertions (`<!before X>` became the
#       bogus subrule `<!beforeX>`), so any `%%`-separated quantifier whose atom
#       held a lookahead failed to match.
#   (2) The native separated-quantifier matcher took only the single
#       highest-priority match per atom, so a frugal atom (`[[.]+?]`) could not
#       expand to satisfy a following anchor/goalpost when the whole thing was
#       wrapped in `<( ... )>` capture markers.

plan 19;

# --- (1) lookahead inside a separated quantifier ---
ok "abc" ~~ / [ <!before X> . ]+ %% 'Y' /,  "lookahead atom + %% matches";
is ~$/, "a", "  ... and matches one atom (no separator present)";

ok "abc" ~~ / <!before X> . +% 'Y' /, "leading lookahead + separated quantifier matches";
is ~$/, "a", "  ... matches the single atom";

ok "a.b.c" ~~ / [ <!before X> . ]+ %% '.' /, "lookahead atom + %% with present separator";
is ~$/, "a.b.c", "  ... consumes all separated items";

# --- (2) capture markers + separated quantifier + trailing delimiter ---
ok "<abc>" ~~ / '<' ~ '>' [<( [[.]+?]* %% 'Y' )>] /, "goalpost + <(...)> + frugal %% quantifier";
is ~$/, "abc", "  ... capture markers restrict to the inner text";

ok "<abc>" ~~ / '<' [<( [[.]+?]* %% 'Y' )>] '>' /, "literal delims + <(...)> + %% quantifier";
is ~$/, "abc", "  ... inner text captured";

# capture markers + separator WITHOUT a following delimiter still works
ok "abc" ~~ / [<( . +% 'Y' )>] /, "capture markers + separated quantifier, no trailing anchor";
is ~$/, "a", "  ... single frugal atom";

# --- the actual Zef::Identity `value` rule, standalone ---
my regex value { '<' ~ '>' [<( [[ <!before \>|\<|\\> . ]+?]* %% ['\\' . ]+ )>] }
ok "<0.0.1>" ~~ &value, "Zef::Identity value regex matches <0.0.1>";
is ~$/, "0.0.1", "  ... captures the inner version string";
ok "<>" ~~ &value, "Zef::Identity value regex matches empty <>";
is ~$/, "", "  ... captures empty inner";

# --- the full Zef::Identity grammar TOP ---
grammar Ident {
    regex TOP { ^^ <name> [':' <key> <value>]* $$ }
    regex name  { <-restricted +name-sep>+ }
    token key   { <-restricted>+ }
    regex value { '<' ~ '>' [<( [[ <!before \>|\<|\\> . ]+?]* %% ['\\' . ]+ )>] }
    token restricted { [':' | '<' | '>' | '(' | ')'] }
    token name-sep   { < :: > }
}
my $m = Ident.parse("Foo::Bar:ver<0.0.1>:auth<>:api<>");
ok $m, "identity string parses";
is ~$m<name>, "Foo::Bar", "  ... name captured";
is $m<value>.join("|"), "0.0.1||", "  ... value list captured with empties";
