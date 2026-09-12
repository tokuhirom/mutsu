use Test;

# `:my $*VAR = …;` inside a grammar rule body declares a dynamic variable
# scoped to that rule's own match, in rakudo's model. mutsu instead
# establishes every such declaration in `self.env` before the parse starts,
# and until now failed to fully undo it afterward for a SCALAR (`$*`)
# variable: `set_env_with_main_alias_inner` keeps a `$*x`/`*x` alias PAIR in
# sync (a bare env key `*x` and a sigil-kept `$*x`), but the establish/restore
# save-and-restore only tracked one half of that pair -- so restoring left the
# other half's stale value in place, and `$*LAST` was still visible to the
# caller after `.parse` returned (GH #8096).
#
# The narrower "established for the whole parse rather than only the
# declaring rule's own match" half of #8096 (a `:my` in a losing LTM
# candidate still runs and is visible to actions of unrelated/sibling rules)
# is NOT fixed here and is NOT asserted below -- see the issue for that
# remaining, deeper gap.

plan 4;

grammar H {
    proto token sigil { * }
    token sigil:sym<dollar> { :my $*LAST = 'dollar'; '$' }
    token sigil:sym<at>     { '@' }
    token TOP { <sigil> }
}
class B { method TOP($/) { make $*LAST // 'none' } }

H.parse('$', :actions(B));
is ($*LAST // 'unset'), 'unset', 'a $*-sigil grammar dynvar does not leak past .parse (1)';

H.parse('@', :actions(B));
is ($*LAST // 'unset'), 'unset', 'and not after a second .parse either (2)';

# A pre-existing outer `$*LAST` must survive a parse of the same name
# unharmed (the restore puts back what was there, not just removes it).
{
    my $*LAST = 'outer';
    H.parse('$', :actions(B));
    is $*LAST, 'outer', 'a pre-existing same-named dynvar is restored after .parse';
}

# The `@*`/`%*`-sigil case (no alias pair -- only ever had one key) keeps
# working: no regression from the `$*`-specific alias-pair fix above.
grammar P {
    token TOP { :my @*SEEN = (); <item>+ % ',' }
    token item { \w+ }
}
class PA {
    method item($/) { @*SEEN.push(~$/) }
    method TOP($/) { make @*SEEN.join('|') }
}
is P.parse('a,b,c', :actions(PA)).made, 'a|b|c', 'an @*-sigil grammar dynvar still accumulates across sibling matches';
