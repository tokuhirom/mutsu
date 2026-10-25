# The parser decides an assignment's `:item` flag

`Assignment.new(:item)` follows rakudo's `$*LEFTSIGIL`. It used to be derived by a post-pass
(`rakuast/assignment_item.rs`) that cloned and rewrote the whole converted tree. The parser now
reads the sigil off the assignment target when it builds the node (`ast::left_sigil`,
`parsed::Assign::item`), and carries what earlier operands left across the loose operators
(`andthen`/`or` chains and the comma list), as the grammar does inside one `EXPR`. The post-pass
is deleted.
