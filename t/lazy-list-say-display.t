use Test;

# `say`/`note`/`put`/`print` on a LazyList must render its contents, not the
# bare type name "LazyList". An eager gather is materialized to its elements;
# a genuinely lazy/infinite sequence or map/grep pipeline renders as raku's
# placeholder (`(...)` for gist-style, `...` for Str-style) rather than being
# reified.

plan 12;

# --- eager gather: say uses .gist => (elements) ---
is (gather { take 1; take 2; take 3 }).gist, '(1 2 3)', 'gather.gist';
is (gather { take 1; take 2; take 3 }).Str, '1 2 3', 'gather.Str';

# say/put/note funnel through the same gist/Str rendering as above. We assert
# the underlying methods rather than capturing stdout, but cover all forms via
# their rendering method:
{
    my $g = gather { take $_ * 10 for 1 .. 3 };
    is $g.gist, '(10 20 30)', 'gather-with-loop gist';
}

# An empty gather renders as the empty list.
is (gather { }).gist, '()', 'empty gather.gist';

# --- genuinely lazy: infinite sequence renders as a placeholder ---
is (1, 2, 4 ... *).gist, '(...)', 'infinite sequence .gist is (...)';
is (1, 2, 4 ... *).Str, '...', 'infinite sequence .Str is ...';

# --- lazy map/grep pipeline over an infinite source ---
is (1 .. *).map(* + 1).gist, '(...)', 'lazy map pipeline .gist is (...)';
is (1 .. *).grep(* %% 2).Str, '...', 'lazy grep pipeline .Str is ...';

# --- a FINITE sequence is already a Seq, fully rendered (not a placeholder) ---
is (1, 2, 4 ... 64).gist, '(1 2 4 8 16 32 64)', 'finite sequence renders fully';

# --- lazy pipeline can still be sliced (laziness preserved) ---
is-deeply (1 .. *).map(* + 1).head(3).List, (2, 3, 4), 'lazy map head still works';
is-deeply (1 .. *).grep(* %% 2).head(3).List, (2, 4, 6), 'lazy grep head still works';

# --- a forced gather behaves as a normal Seq afterward ---
is-deeply (gather { take 5; take 6 }).reverse.List, (6, 5), 'gather.reverse';
