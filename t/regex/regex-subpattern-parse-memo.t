use Test;

plan 20;

# The regex parser is recursive: a group, a lookaround body, an alternation
# branch, a conjunction part and a `%`-separator atom each re-enter the parse
# entry point for their own text. That entry point memoizes now, so the inside
# of a pattern is parsed once instead of once per parse of the enclosing
# pattern (10,958 parses of 247 distinct patterns down to 257 on a 60-row
# `benchmarks/bench-yaml-parse.raku` document, issue #7576).
#
# The key is the pattern text *after* `$`/`@`/`%` interpolation, which is where
# the parse actually becomes a function of its input, plus the current package
# and the grammar-token registry generation. These tests drive each of those
# three axes, and the one pattern shape deliberately left out of the memo.
# Every expectation here was verified against rakudo v2026.07.

# --- the interpolated text is the key, not the source text ------------------
# The same source pattern with a different variable value must parse to a
# different tree, including when the variable sits inside a nested group (a
# separate, recursive parse of its own).
{
    my $x = 'ab';
    ok 'zabz' ~~ / z [ $x ] z /, 'interpolated group matches its first value';
    $x = 'cd';
    ok 'zcdz' ~~ / z [ $x ] z /, 'interpolated group follows the new value';
    nok 'zabz' ~~ / z [ $x ] z /, 'the old value is not still cached';
}

# A `$<name>` capture form is *not* a variable read, so the interpolated text
# is stable and these do share a memo entry. Repeating the same capturing
# sub-pattern must still capture per-match, not replay the first match.
{
    my @got;
    for 'k1: v1', 'k2: v2' -> $line {
        $line ~~ / $<key> = [ \w+ ] ':' \s* $<val> = [ \w+ ] /;
        @got.push: "$<key>=$<val>";
    }
    is @got[0], 'k1=v1', 'a repeated capture sub-pattern captures the first match';
    is @got[1], 'k2=v2', 'and the second, rather than replaying the first';
}

# --- a parse step that reads runtime state is never stored --------------------
# `<$var>` takes the variable's VALUE at parse time and recompiles it as a
# regex, so the same pattern text means different things under different
# values. (Unlike a bare `$var`, which is substituted into the text first, so
# the memo key already reflects it.) This is the shape that caught the first
# version of this memo, which derived its exclusions from reading the parser
# instead of from the parser reporting its own reads:
# `roast/S05-metasyntax/litvar.t`, where `$var = '$i'` and `$var = '<$i>'` are
# two consecutive matches of the identical source pattern `/<$var>/`.
{
    my $one = 'aa';
    my $var = '$one';
    my $two = 'bb';
    ok 'aa' ~~ / ^ <$var> $ /, 'a <$var> assertion compiles the current value';
    $var = '$two';
    ok 'bb' ~~ / ^ <$var> $ /, 'and follows a new value under the same pattern text';
    nok 'aa' ~~ / ^ <$var> $ /, 'rather than reusing the first parse';
}

# `<@var>` reads the array's elements at parse time, the same way.
{
    my @alts = <cat dog>;
    ok 'cat' ~~ / ^ <@alts> $ /, 'a <@var> assertion matches an element';
    nok 'emu' ~~ / ^ <@alts> $ /, 'and only an element';
}

# A `<@var>` match BEFORE a later reassignment in the same scope must see the
# array's value as it stood at that point, not the array's eventual final
# content (issue #8040): `self.env.get` returns a lexical the compiler boxed
# into a shared `ContainerRef` cell because of the later reassignment, and
# that cell was read without dereferencing it first — so the whole cell
# stringified as ONE alternation element instead of iterating the array's
# actual elements. A single-element array happened to stringify the same way
# either way, which is why only the *first* of a pair of matches around a
# reassignment ever showed the bug.
{
    my @later = <cat dog>;
    ok 'cat' ~~ / ^ <@later> $ /, 'a <@var> match before a later reassignment sees the current value';
    @later = <emu>;
    ok 'emu' ~~ / ^ <@later> $ /, 'and a later match sees the reassigned value';
}

# --- the current package is part of the key ---------------------------------
# `<+digit +thing>` folds `thing`'s body into the character class at parse
# time, so two grammars whose TOP bodies are character-for-character identical
# must still get different trees.
{
    grammar MemoA { token TOP { <+digit +thing>+ }; token thing { <[x]> } }
    grammar MemoB { token TOP { <+digit +thing>+ }; token thing { <[y]> } }
    ok  MemoA.parse('1x2'), 'a folded token body resolves in its own grammar';
    nok MemoA.parse('1y2'), 'and not against the other grammar of the same body text';
    ok  MemoB.parse('1y2'), 'the identical body text folds the other grammar"s token';
    nok MemoB.parse('1x2'), 'without inheriting the first grammar"s fold';
}

# --- a token (re)definition invalidates entries ------------------------------
# The memo records the token-registry generation it was built under, so a
# pattern parsed before a token exists must not keep its pre-definition tree.
{
    ok 'q' ~~ / <[q]> /, 'a plain class matches before the grammar is declared';
    grammar MemoC { token TOP { <+alpha +punct>+ } }
    ok MemoC.parse('ab,cd'), 'a grammar declared later still parses';
}

# --- the shape deliberately left out of the memo -----------------------------
# `<~~>` records the *enclosing* regex's source, so the same sub-pattern text
# can mean different things under different outer patterns. Such patterns skip
# the memo entirely; this checks recursion still works.
{
    my regex balanced { '(' [ <-[()]> | <~~> ]* ')' };
    ok  '(a(b)c)' ~~ /^ <balanced> $/, '<~~> recursion matches a nested pair';
    nok '(a(b c)'  ~~ /^ <balanced> $/, '<~~> recursion rejects an unbalanced one';
}
