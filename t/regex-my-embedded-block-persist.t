use Test;

# A regex-declared `:my`/`:constant` lexical is a plain declaration, not a
# backtracking-sensitive rebinding: per raku-doc/doc/Language/regexes.rakudoc
# ("C<:my> helps scoping the C<$c> variable within the regex and beyond"),
# its value (including any mutation an embedded `{ ... }` code block makes)
# persists into the caller's enclosing lexical scope -- unconditionally,
# whether the overall match succeeds or fails. This is unlike `:let`
# (restore-on-fail, persist-on-success) and unlike `:temp` (always restored
# at the end of the match). Verified against real `raku` for every case here.

# A non-quantified embedded code block mutating a `:my` var.
{
    my $s = "abc";
    $s ~~ / :my $c = 0; { $c = 1 } /;
    is $c, 1, 'non-quantified embedded block mutation persists';
}

# A plain `*` quantifier running the embedded block once per iteration.
{
    my $s = "aaa";
    $s ~~ / :my $counter = 0; ( a { ++$counter } )* /;
    is $counter, 3, ':my counter survives a plain * quantifier';
}

# A plain `+` quantifier.
{
    my $s = "aaa";
    $s ~~ / :my $counter = 0; ( a { ++$counter } )+ /;
    is $counter, 3, ':my counter survives a plain + quantifier';
}

# The `:my` declaration itself (no mutation) persists after a match that
# fails outright.
{
    my $s = "xyz";
    my $r = $s ~~ / :my $c = 42; a /;
    nok $r, 'match itself fails as expected';
    is $c, 42, ':my initializer still persists after a failed match';
}

# `:constant` behaves the same way as `:my` for persistence.
{
    my $s = "afoo";
    $s ~~ / :constant $x = 'foo'; a $x /;
    is $x, 'foo', ':constant persists to the enclosing scope';
}

# `:temp`, by contrast, is always restored -- confirms this fix did not
# widen persistence to declarators that must stay regex-local.
{
    my $a = 1;
    my regex ta { :temp $a = 5; a };
    ok 'a' ~~ &ta, 'temp-declaring rule matches';
    is $a, 1, ':temp does not leak its value into the caller';
}

# `:let` keeps its own (unaffected) restore-on-fail / persist-on-success
# semantics.
{
    my $a = 1;
    my regex la { :let $a = 5; <&lma> };
    my regex lma { $a $a };
    nok '23' ~~ / ^ <la> $ /, 'let: non-match detected';
    is $a, 1, 'let: unsuccessful match does not affect the variable';

    ok '55' ~~ / ^ <la> $ /, 'let: successful match with changed value';
    is $a, 5, 'let: successful match preserves the new value';
}

done-testing;
