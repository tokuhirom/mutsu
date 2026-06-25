use Test;

plan 16;

# `@<name>=(...)` array-sigil capture alias forces list context.

# Quantified capturing group: list of one Match per iteration.
ok "  a b\tc" ~~ m/@<chars>=( \s+ \S+ )+/, 'quantified @ array alias matches';
is join("|", @<chars>), "  a| b|\tc", 'quantified @ alias captures a list';

# Scalar `$<name>=(...)+` on a capturing group is ALSO a per-iteration list
# (the `wrap_named_quant` exclusion for capturing groups).
ok "  a b\tc" ~~ m/$<chars>=( \s+ \S+ )+/, 'quantified $ alias on group matches';
is join("|", @<chars>), "  a| b|\tc", '$ alias on capturing group is a list too';

# A scalar alias on a *non-grouping* quantified atom captures the whole span.
ok "abc" ~~ m/$<x>=\w+/, 'scalar alias on bare quantified atom matches';
is $<x>.^name, "Match", 'bare quantified atom alias is a single Match';
is ~$<x>, "abc", 'bare quantified atom alias captures the whole span';

# Non-quantified `@<foo>=(...)` still forces list context (one-element List).
ok "abcd" ~~ m/a @<foo>=(.(.)) d/, 'non-quantified @ array alias matches';
is @<foo>.elems, 1, 'non-quantified @ alias yields a one-element list';
is ~@<foo>, "bc", 'non-quantified @ alias stringifies to the group match';
is ~@<foo>[0][0], "c", 'nested inner capture preserved under @ alias';

# Nested array alias inside an array alias, plus hyper named access.
ok "  a b\tc" ~~ m/@<chars>=( @<spaces>=[\s+] (\S+))+/, 'nested array alias matches';
is ~@<chars>, "  a  b \tc", 'outer nested array capture';
is join("|", @<chars>».<spaces>), "  | |\t", 'hyper named-capture access on Match list';

# `@<name>=[...]+` on a non-capturing group: whole-span single Match (wrapped),
# but accessible the same way.
my $m = "c" ~~ /@<from>=[.]+/;
ok $m, 'non-capturing group array alias matches';
is ~$m<from>, "c", 'non-capturing group alias captures the run';
