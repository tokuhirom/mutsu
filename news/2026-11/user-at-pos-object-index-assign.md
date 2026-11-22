# A user `AT-POS` answers `$obj.AT-POS($key) = $v` with any index

Assigning through a method call to a class's own `AT-POS` —
`$obj.AT-POS($column) = $value`, or `SELF.AT-POS($index) = $value` inside an
`ASSIGN-POS` — went through `Any`'s built-in `AT-POS` row first, which coerced
the index with `.Int` and died with "Cannot resolve caller Int(Red::Column:D: )"
for an object index. A class (or a composed or mixed-in role) that declares the
method itself now answers the call. This unblocks Red's JSON column updates
(`t/087-json-operations.rakutest`) (#18782).
