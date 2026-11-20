# A bare block's line-ending `}` no longer chains a `.method` from the next line

A bare block statement whose closing `}` ends its line now ends the
statement there, as in rakudo, so a `.method` on the following line is a new
statement calling the method on `$_`:

```raku
{
    ...
}
.return with $no;   # returns $no, not a method call on the block above
```

mutsu's bare-block statement parser looked past the newline for a `.` and
turned the block into the invocant, so `.return with $no` returned the Block
itself. Red 0.2.14's `MetamodelX::Red::Model.create` ends exactly this way,
which made `Model.^create(...)` with relationship arguments return a `Block`
and broke Red's `t/035-create` and `t/058-nm-relationship` and RedFactory's
`t/02-example` (#18385). A `.method` or `()` on the same line as the `}` still
calls the block.
