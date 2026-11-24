# `.?` answers `Nil` again on a List, a `.VAR` view or a container's value

#19113 made `.?method` re-throw a missing-method error raised inside a method
it did find. To tell that inner miss from the receiver's own miss, it compared
the error's type name with the receiver's. Dispatch does not always spell the
receiver's type the same way: a List literal is dispatched as an Array, a
`$x.VAR` descriptor as the value it holds, and an array element as the value
in its container. For those receivers `.?` threw `X::Method::NotFound` where it
should have returned `Nil`. This broke Red (`.?is-hidden-from-sql-commenting`),
AttrX::Mooish (`$attr.VAR.?is-building`) and Text::CodeProcessing (`.?__hide`).

The check now counts the miss as the receiver's own when the receiver, or the
value its container or `.VAR` view holds, is of the type the error names. An
inner miss on a different receiver still propagates.
