use Test;

# `.map`/`.grep`/`.first`'s callable checks and invocation matched
# `func.view()` directly against `ValueView::Sub(_) | ValueView::Routine {..}`,
# so a Sub mixed with a role (`&foo but R1`) -- a `ValueView::Mixin` wrapping
# a `Sub` -- failed the check (`.map` threw X::Cannot::Map) or, once past a
# widened check, failed differently ("Callable expected") because the actual
# per-element invocation didn't look through the Mixin either. Verified
# against raku directly.

plan 5;

role R1 { method zz(--> True) { } }
sub double($x) { $x * 2 }
sub is-even($x) { $x %% 2 }

is-deeply (1, 2, 3).map(&double but R1).List, (2, 4, 6),
    '.map invokes a role-mixed Sub';

is-deeply (1, 2, 3, 4).grep(&is-even but R1).List, (2, 4),
    '.grep invokes a role-mixed Sub';

is (1, 2, 3, 4).first(&is-even but R1), 2,
    '.first invokes a role-mixed Sub';

# A lazy pipeline stage (infinite source) must not force-materialize but
# still invoke the role-mixed Sub correctly per element.
is-deeply (1..*).map(&double but R1)[^3].List, (2, 4, 6),
    '.map on an infinite Range still invokes a role-mixed Sub lazily';

# The role's own method is still reachable on the mixed callable itself.
ok (&double but R1).zz, 'the mixed-in role method is still callable on the Sub itself';
