# An attribute array's `unshift` prepends while other threads run

Once a program has started a second thread, array mutations go through a
shared-array lane. Its fallback for a non-lexical array (an attribute
`@!p`) handled `push` and `unshift` with the same append, so `unshift`
added at the end. Terminal::LineEditor computes its width prefix sums with
`@!char-widths-prefix-sum = [\+] @!char-widths; ...unshift(0)`, and its
`CLIInput` starts a decoder thread when constructed, so the sums came out
`[1, 2, 3, 0]` and every rendered input field was padded wrong (Rakuish
t/58-feel-field.t). That lane now takes `push` only; `unshift` goes through
the ordinary path, as `prepend` already did.
