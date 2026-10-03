# A block returning `rx/.../` matches the topic for any pattern

`.grep({ rx/ ^ \w+ ':' ... / })` now keeps only the elements the regex
matches. The returned Regex answers `.Bool` against the defining block's
`$_`, but that topic was captured only for patterns the static regex tree
can represent (`\d`, literals). An `rx//` holding `\w`, `.`, `\s` or a
character class stayed a plain literal without the capture, so its `.Bool`
was True for every element. File::Utils' `uid2username` read
`/etc/passwd` this way and found every line; its test suite now passes.
