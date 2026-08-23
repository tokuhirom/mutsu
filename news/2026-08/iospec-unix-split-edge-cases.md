# Unix path splitting handles empty and root edge cases

`IO::Spec::Unix.split` and `.splitpath` now match Rakudo for all-slash paths,
the empty path, and a bare dot.
