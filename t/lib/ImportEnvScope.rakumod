unit module ImportEnvScope;

# A `&`-sigil sub and a `$`-sigil constant, both exported as VARIABLES (not
# plain `sub` declarations) so they are imported by writing into `env` under
# their bare names (`&greet`, `$GREETING`) — the code path
# `use-inside-a-block-leaks-to-the-enclosing-scope.md` describes as the "env
# half" of lexical import scoping.
our &greet is export = -> { "hello" };
our $GREETING is export = "hi-const";
