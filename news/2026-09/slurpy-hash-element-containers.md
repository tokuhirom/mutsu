`Hash` element containerization now tracks each stored value. Values captured
raw by a slurpy `*%h` binding keep Pair-style Boolean shorthand in `.raku`,
while values assigned into that hash acquire the same per-entry container
semantics as an ordinary `Hash`. Map coercion and hash views preserve the
distinction.
