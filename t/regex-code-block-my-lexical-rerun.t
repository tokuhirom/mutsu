use Test;

# A `{ ... }` block inside a regex runs again whenever the engine reaches it
# again -- on a backtracked retry, or on the next iteration of a quantified
# atom. Its own `my` declarations are lexical to the block, so every run must
# see its own freshly-initialized binding.
#
# mutsu lost them from the second run on. The block's `my` names were logged
# into `pending_local_updates` (the caller-slot writeback an embedded block uses
# to publish a write to an OUTER lexical). That made the VM treat the
# block-local name as a caller lexical and refresh it *from env* at the block's
# next call -- `writeback_match_locals` -- so on the re-run the freshly assigned
# slot was overwritten with the outer binding of the same name, or with `Any`
# when there was none. A method call was enough to trigger it (`$/.pos` in the
# original report, but also `"x".chars`), because the refresh rides on the call.
# The same leftover log also made `eval_regex_inline_code` panic on
# `split_off`: the body's own call had already drained the list.

plan 9;

# --- backtracked re-run ------------------------------------------------------
my @seen;
if "aaab" ~~ / (\w)+ { my $v = "Q"; my $p = $/.pos; @seen.push($v) } b / { }
is @seen.elems, 2, 'the block ran twice (once per backtrack)';
is @seen.join(','), 'Q,Q', 'a block-local `my` survives a backtracked re-run';

# `$/` is not special here -- any method call used to trigger it.
my @seen2;
if "aaab" ~~ / (\w)+ { my $v = "Q"; my $p = "x".chars; @seen2.push($v) } b / { }
is @seen2.join(','), 'Q,Q', 'a plain method call does not lose the block-local';

# --- forward re-run through a quantifier ------------------------------------
my @seen3;
if "ab" ~~ / [ \w { my $v = "Q"; my $p = "x".chars; @seen3.push($v) } ]+ / { }
is @seen3.join(','), 'Q,Q', 'a block-local survives a quantifier re-run';

# --- `make` reading such a variable -----------------------------------------
if "aaab" ~~ / (\w)+ { my $v = "Q"; my $p = $/.pos; make $v } b / {
    is $/.made, 'Q', '`make` of a block-local reads the value of this run';
}

# --- a block-local must NOT touch a same-named outer lexical -----------------
my $v = 'OUT';
my @inner;
if "aaab" ~~ / (\w)+ { my $v = "Q"; my $p = "x".chars; @inner.push($v) } b / { }
is @inner.join(','), 'Q,Q', 'a block-local shadows the outer binding on every run';
is $v, 'OUT', 'the outer lexical is untouched by the block-local declaration';

# --- an outer lexical write from the block still reaches the caller ----------
# This is what `pending_local_updates` exists for, so the filter above must not
# break it.
my $seen;
if "123" ~~ / (\d) { $seen = $/.Str } \d+ / { }
is $seen, '1', 'a write to an outer lexical still reaches the caller slot';

my $count = 0;
if "aaab" ~~ / (\w)+ { my $t = "x"; $count++ } b / { }
is $count, 2, 'an outer lexical is still updated on every re-run';

done-testing;
