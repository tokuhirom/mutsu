# `<?subrule>` / `<!subrule>` zero-width lookahead assertions

Raku's `<?name>` / `<!name>` assertion is a zero-width lookahead: it asserts that
the named subrule matches (or, negated, fails) at the current position without
consuming any input. mutsu recognised only the *special* forms of this —
`<?before …>`, `<?[…]>`, `<?alpha>` and the other builtin character classes,
`<?wb>`, `<?same>`, `<?:Prop>`, `<?@var>` — but a general `<?userToken>` fell
through to a literal-string match of `?userToken`, so the assertion never fired
and the surrounding rule simply failed.

Now any `<?name>` / `<!name>` whose `name` is an identifier-led subrule (with an
optional leading `.` for a non-capturing call, and optional arguments) lowers to
a `Lookaround` atom wrapping a `Named(subrule)` call — reusing the existing,
well-tested lookaround matcher. Both the positive and negative forms are covered,
and known character-class assertions (`<?alpha>` etc.) still resolve to the class
as before.

This was a blocker for the **YAMLish** battery's block collections: its
`list-entry` token is `'-' <?break> …`, asserting a break (space/tab/newline)
follows the dash before the element consumes it. Without the assertion the
sequence never parsed.

Pin: `t/regex-subrule-lookahead-assertion.t`.
