# `^add_method` of an attribute accessor under another name

`Foo.^add_method('VAL', Foo.^method_table<val>)` registered a forwarder that called
`&val`, a code variable the class never declares, so calling the alias died with
`Could not find symbol '&val' in 'Foo'`. AttrX::Mooish re-adds an attribute's
accessor under each `:alias` this way. The method table entry of a public
attribute now installs a real method that answers through the class's own
accessor (`self.val`), so the alias reads the attribute and an `is rw` accessor
stays assignable. AttrX::Mooish `t/120-set-options` passes in full (#14266).
