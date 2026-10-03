use Test;

# A sigil alias binds ONE quantified atom (`$<name>=<quantified_atom>`), so a
# second quantifier after an already-quantified aliased atom quantifies the
# whole alias: `$<w>=.*? +%% X` is `[$<w>=.*?]+ %% X`. From CSS::Writer's
# README splitter `/^ $<waffle>=.*? +%% ["```" \n? $<code>=.*? "```" \n?] $/`.

plan 8;

ok "aXbXc" ~~ /^ $<w>=.*? +%% X $/, 'second quantifier with %% separator parses and matches';
is $<w>.elems, 3, 'the alias collects one Match per iteration';
is $<w>.map(~*).join(','), 'a,b,c', 'each iteration captured its own span';

ok "a1b2" ~~ /^ [ $<l>=\w ** 1 <:N> ] + $/, 'bracketed aliased quantified atom still works';

ok "aa-bb" ~~ /^ $<p>=\w+ +% '-' $/, 'alias on \w+ with an outer +% separator';
is $<p>.map(~*).join('|'), 'aa|bb', 'outer quantifier yields per-iteration matches';

# A single quantifier on the aliased atom keeps capturing the whole span.
ok "aaa" ~~ /^ $<q>=a+ $/, 'single quantifier on an aliased atom';
is ~$<q>, 'aaa', '... captures the whole run as one Match';
