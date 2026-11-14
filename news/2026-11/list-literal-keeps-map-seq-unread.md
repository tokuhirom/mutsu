# A List literal no longer runs the `.map` Seqs it holds

`((1..2).map({ say $_ }), 3);` prints nothing on rakudo. The List holds
the Seq as an unread element, and sinking the List does not touch it.
mutsu's List-literal builder (`exec_make_array_op`) reified every
not-yet-read Seq element on construction, exempting only a `gather`. So a
comma list of `.map`s ran each callback, both as a bare statement and as
the tail of a `--> Nil` routine. Assigning such a List to a scalar ran it
too.

A not-yet-run `.map`/`.grep` element now stays unread like a gather:
reading, flattening or iterating it runs it, exactly once. Found on
MCP::Server::Tool::Web t/12 (#17749). Test:
`t/collections/lazy-seq/list-literal-keeps-map-seq-unread.t`.
