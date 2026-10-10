# Same-named attributes with different sigils no longer share a slot

`has %!d; has $.d` (in one class, or split across a parent and a child) used
to lose the public `$.d` value: the accessor and `new`'s named-arg binding read
the bare-name key while the colliding declaration had been moved to a
sigil-qualified key. The public declaration now keeps the bare key and only the
other declarations are stored as `<sigil>name`; private reads try the sigil key
first, and `Attribute.get_value`/`set_value` resolve the same key (#12528).
