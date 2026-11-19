# A method's own `my` binds no longer make its exit look up the attribute cell

On method exit, `reconcile_attrs` adopts an attribute `:=` binding the method made
(`$!x := $outer`) by collecting the frame's `ContainerRef` locals and matching their names
against the instance's attribute keys. A `my $x := ...` or a raw parameter also leaves a
`ContainerRef` in its slot, so every method with such a bind looked up the attribute cell,
spelled each name under the seven attribute twigils and only then threw every reading away:
a name the frame declares itself is never an attribute binding.

The exit now applies that same rule to the collected names first and stops when none can be
an attribute reading, before the cell lookup. Only names with an attribute twigil (`!x`,
`@!x`, ...) or bare slots the frame did not declare go on to the scan, exactly the readings
the scan kept before. The ownership test also stopped interning the name it asks about.

AttrX::Mooish methods bind their own locals on almost every line
(`my $attr-var := self.get_value($instance)`), and its `t/090-concurrent*.rakutest` runs those
methods millions of times.
