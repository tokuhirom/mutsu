# An empty array in a regex never matches

Interpolating an empty array into a regex (`/@a/`, `/@a+/`, `<@a>`) now behaves like Rakudo: it is
an alternation with no branches, so it never matches. Before, the bare `@a` form spliced in an
empty `[]` group and died with `Null regex not allowed`. The `<@a>` form dropped the atom from the
pattern altogether, so `/<@a>/` matched the empty string and `<?@a>` always held. Both forms now
lower an empty array to the `<!>` always-fail atom (#13152).
