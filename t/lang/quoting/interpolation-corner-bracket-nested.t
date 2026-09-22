use Test;

# The `{ ... }` block-interpolation scanner inside a double-quoted string
# tracks `'...'`/`"..."` regions so their braces don't unbalance the outer
# brace-matching scan (`"{ '}' }"` carries a literal `}`). It did not know
# about `｢...｣` (corner-bracket) literals, which nest and never escape: a
# lone `'` INSIDE a `｢...｣` region (e.g. `｢'｣`, a one-character literal
# string) was wrongly read as opening ASCII single-quote tracking, which then
# swallowed the block's real closing `}` and the whole string as
# "unterminated" (SixPM's `ZefInstaller.rakumod`:
# `"'{.trans( [｢'｣] => [｢\'｣] )}'"`, reduced here).

plan 3;

my $s = "'{ ｢'｣ }'";
is $s, "'''", 'a bare corner-bracket literal inside {...} does not swallow the closing brace';

my @argv = <a b c>;
my $quoted = @argv.map({"'{.trans( [｢'｣] => [｢\'｣] )}'"});
is-deeply $quoted.list, ("'a'", "'b'", "'c'"), 'nested corner-bracket quoting inside a {...} interpolation block';

my $nested = "{ ｢a｢b｣c｣ }";
is $nested, "a｢b｣c", 'nested corner-bracket literals inside a {...} interpolation block';

done-testing;
