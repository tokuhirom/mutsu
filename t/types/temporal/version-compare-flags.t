use v6;
use Test;

plan 26;

# Ordering (<=>): a trailing `+` sorts *after* the bare version, a `*`
# (Whatever) part sorts *before* any concrete part.
is v1.0.1+ <=> v1.0.1, More, 'v1.0.1+ sorts after v1.0.1';
is v1.0.1  <=> v1.0.1+, Less, 'v1.0.1 sorts before v1.0.1+';
is v1+ <=> v1, More, 'v1+ > v1';
is v1  <=> v1+, Less, 'v1 < v1+';
is v1.* <=> v1.0, Less, 'v1.* sorts before v1.0';
is v1.0 <=> v1.*, More, 'v1.0 sorts after v1.*';
is v1.*.* <=> v1.0.0, Less, 'v1.*.* < v1.0.0';
is v1.2.* <=> v1.2.3, Less, 'Whatever part < concrete tail';
is v1.2.* <=> v1.1.9, More, 'earlier concrete part still decides';
is v1.0.1+ <=> v1.0.2, Less, '+ never overrides an earlier concrete diff';

# The `+` flag as an ordering tie-breaker also drives <= / <.
ok v1.* <= v1.0, 'v1.* <= v1.0';
ok v1.*+ <= v1.0, 'v1.*+ <= v1.0 (Whatever still sorts first)';
ok v1.0.1 < v1.0.1+, 'v1.0.1 < v1.0.1+';

# Smart-match (~~): the RHS is the matcher. `vX+` accepts anything >= vX,
# a `*` matcher part accepts the rest, and a bare version compares its parts.
ok  v1.2 ~~ v1.0+, 'v1.2 ~~ v1.0+';
nok v1.2 ~~ v1.3+, 'v1.2 !~~ v1.3+';
ok  v2.0 ~~ v1.0+, 'v2.0 ~~ v1.0+';
ok  v1.0 ~~ v1.0+, 'v1.0 ~~ v1.0+ (equal is accepted)';
nok v0.5 ~~ v1.0+, 'v0.5 !~~ v1.0+';
ok  v0.and.anything.else ~~ v0+, 'extra pre-release parts still match v0+';
ok  v1.0 ~~ v1.*, 'v1.0 ~~ v1.*';
nok v2.0 ~~ v1.*, 'v2.0 !~~ v1.*';
ok  v1.5.9 ~~ v1.*, 'v1.5.9 ~~ v1.*';
ok  v1 ~~ v1.*, 'v1 ~~ v1.*';
ok  v1.* ~~ v1.0, 'Whatever on the LHS matches a concrete matcher';
nok v1.2 ~~ v1.0, 'exact matcher requires the parts to match';
ok  v1.2 ~~ v1, 'a shorter matcher ignores extra LHS parts';

done-testing;
