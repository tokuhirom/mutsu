# `X without $v` no longer pins `$v` to the slow declaration store

`with` and `without` bind `$_` to their topic's own container, so a body can
write the source through it. The compiler tags the source variable or element
for that. The tag also kept the variable's own `my` declaration off the lean
declaration store (#12151): the variable had to keep its name-keyed
bookkeeping in case the alias needed it. MCP::Server::Tool::Web's memo helper
pays for that on every call:

```raku
my $known = %memo{$needle};
if $known.defined && $known[0] <= $from {
    return Int without $known[1];
```

The parser desugars `with`/`without` to a block that tests `$_.defined` and
then runs the statement. A body that never names `$_` cannot observe whether
`$_` aliases the source. Such a body is one that only returns, dies, or
`next`s/`last`s with literals, type names and plain variable and element reads.
For it, the topic is now read as a plain value with no tag. A body that reads
or writes `$_`, calls a routine, or matches a regex keeps the alias exactly as
before.
