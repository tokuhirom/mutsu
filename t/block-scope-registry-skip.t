use v6;
use Test;

# eval_block_value skips restoring the block-scope routine registry when the
# monotonic `registry_write_gen` shows the block wrote nothing to it (the common
# grammar `token` body, which is just a regex, declares nothing). This test pins
# that the skip is sound: a block that DOES declare a lexical sub/operator/&-var
# must still isolate it, and a declaration-free grammar parse with actions must
# still work. (The `require`-inside-a-block case — a registry write reached via a
# call, not a direct declaration opcode — is pinned by roast S06-other/main.t,
# which the compile-time-opcode-scan version of this optimization regressed.)

plan 9;

# --- a block-scoped named sub must not leak out of its block ---
{ sub blk-sub { 1 } };
nok defined(&blk-sub), 'block-scoped sub does not leak';

# --- a sub nested in an inline if-branch must not leak ---
{ if True { sub if-sub { 2 } } };
nok defined(&if-sub), 'sub in inline if-branch does not leak';

# --- a sub in a nested bare block must not leak ---
{ { sub bare-sub { 3 } } };
nok defined(&bare-sub), 'sub in nested bare block does not leak';

# --- a block-scoped operator must not leak (still callable inside) ---
my $inside;
{ sub infix:<blkop> ($a, $b) { $a * $b }; $inside = 4 blkop 5 };
is $inside, 20, 'block-scoped operator works inside the block';
nok (try EVAL '6 blkop 7').defined, 'block-scoped operator does not leak';

# --- a `my &foo` binding must not leak ---
{ my &loc-code = -> { 99 } };
nok defined(&loc-code), 'block-scoped &-var binding does not leak';

# --- a grammar parse with actions (declaration-free token bodies) still works ---
grammar Ident {
  token TOP  { <name> <verpart>? }
  token name { <part>+ % '::' }
  token part { \w+ }
  token verpart { ':ver<' $<v>=[<-[>]>+] '>' }
}
class IdentActions {
  method TOP($/) { make ~$<name> }
}
my $m = Ident.parse('Foo::Bar:ver<1.2.3>', :actions(IdentActions));
ok $m.defined, 'grammar with actions parses';
is ~$m<name>, 'Foo::Bar', 'named capture is correct';
is ~$m<verpart><v>, '1.2.3', 'nested capture is correct';
