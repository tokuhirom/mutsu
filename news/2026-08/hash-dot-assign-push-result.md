# Hash push ignores named Pair arguments

Fixed `Hash.push` and `Hash.append` so bareword fat-arrow and colon-pair arguments remain named arguments instead of being mistaken for positional Pair data.

This makes a fresh declaration such as:

```raku
my %h .= push(e => 6);
say %h.raku;
```

leave `%h` empty, matching Raku. Quoted-key and parenthesized Pairs remain positional data and continue to be inserted into the hash. The fix applies to both typed and untyped hash mutation paths while preserving existing Hash push, append, binding, and Pair-itemization behavior.
