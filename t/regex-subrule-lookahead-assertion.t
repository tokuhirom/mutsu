use v6;
use Test;

# `<?subrule>` / `<!subrule>` — a general zero-width lookahead assertion that a
# named subrule matches (or, negated, does not match) at the current position.
# This is the general twin of `<?before …>` / `<?[…]>` / `<?alpha>`; before the
# fix only those special forms were recognised, so `<?break>` (as used by the
# YAMLish grammar's `list-entry`: `'-' <?break> …`) fell through to a literal
# string match and never asserted.

plan 8;

grammar P { token TOP { 'a' <?bee> \w }; token bee { 'b' } }
ok  P.parse("ab").defined, 'positive <?bee> succeeds when bee matches ahead';
nok P.parse("ax").defined, 'positive <?bee> fails when bee does not match ahead';

grammar N { token TOP { 'a' <!bee> \w }; token bee { 'b' } }
ok  N.parse("ax").defined, 'negative <!bee> succeeds when bee does not match ahead';
nok N.parse("ab").defined, 'negative <!bee> fails when bee matches ahead';

# A `<?subrule>` is zero-width: the following atom re-consumes from the same spot.
grammar Dot { token TOP { 'a' <?.bee> \w }; token bee { 'b' } }
ok Dot.parse("ab").defined, 'non-capturing <?.bee> works and is zero-width';

# Subrule lookahead with an argument.
grammar Arg { token TOP { 'a' <?letter('b')> \w }; token letter($c) { $c } }
ok Arg.parse("ab").defined, 'parameterised subrule lookahead <?letter(...)>';

# The YAMLish list-entry shape: `-` followed by a break (zero-width), then the
# element consumes the space and the value.
grammar Seq {
    token TOP { <entry>+ % "\n" }
    token entry { '-' <?break> <.sp> <[\d]>+ }
    token break { " " | "\t" | "\n" }
    token sp { " "* }
}
ok Seq.parse("- 1\n- 2").defined, 'list-entry style `- <?break> value` parses';

# Known character-class assertions still resolve to the class, not a subrule.
grammar Cls { token TOP { <?alpha> \w } }
ok Cls.parse("q").defined, '<?alpha> still asserts the builtin class';
