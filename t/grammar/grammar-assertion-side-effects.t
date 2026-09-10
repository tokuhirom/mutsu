use v6;
use Test;

plan 8;

# A `<?{ … }>` assertion runs inline on the real interpreter, so its side effects
# are real: visible to later assertions in the same match, and surviving a match
# that ultimately fails. See docs/adr/0009 part B. (mutsu used to decide the
# assertion in a scratch interpreter and replay it in the parent only on the
# winning path, which gave neither.)
#
# Each case declares its variables in a block alongside the grammar, as
# advent2013-day18 does. A *file-scope* declaration is not visible to a code
# assertion at all — it lives in the top frame's VM locals and never reaches the
# env the regex engine reads. That is the pre-existing `env_dirty` dual-store gap
# (it behaves identically before and after part B), not something these pin.

# 1. An array push from an assertion reaches the outer array.
{
    our @seen = ();
    my grammar Push {
        token TOP  { <item>**3 }
        token item { (\w) <?{ @seen.push(~$0); True }> }
    }
    ok Push.parse("abc").defined, 'push: parses';
    is-deeply @seen, ["a", "b", "c"], 'an assertion pushes to an outer array';
}

# 2. The side effects survive a parse that FAILS. This is what
#    advent2013-day18 test 10 turns on: its duplicate cards are pushed during
#    parses that do not match.
{
    our @tried = ();
    my grammar Fails {
        token TOP  { <item>**3 }
        token item { (\w) <?{ @tried.push(~$0); ~$0 ne 'c' }> }   # rejects 'c'
    }
    nok Fails.parse("abc").defined, 'failing parse: does not match';
    is-deeply @tried, ["a", "b", "c"], 'side effects survive a failing parse';
}

# 3. An assertion's write is visible to a LATER assertion in the same match —
#    day18's `! %*PLAYED{$card}++` duplicate check depends on exactly this.
{
    our %played = ();
    my grammar Dup {
        token TOP  { <card>**3 }
        token card { (\w) <?{ ! %played{~$0}++ }> }   # fails on a repeat
    }
    ok Dup.parse("abc").defined, 'distinct items parse';
    %played = ();
    nok Dup.parse("aba").defined, 'a repeated item is rejected by the earlier write';
}

# 4. An assertion's own `my` is lexical to it and must not clobber the enclosing
#    scope (day18's assertion declares `my $card`).
{
    my $card = 'outer';
    my grammar Scoped {
        token TOP  { <item> }
        token item { (\w) <?{ my $card = ~$0; $card eq 'x' }> }
    }
    ok Scoped.parse("x").defined, 'assertion with a `my` declaration parses';
    is $card, 'outer', "an assertion's `my` does not leak into the enclosing scope";
}
