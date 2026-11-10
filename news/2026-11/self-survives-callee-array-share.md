# A callee sharing an aggregate no longer overwrites a method caller's `self`

`promote_array_share_source` propagated a callee's `my $value = $data` share (a Hash/Array
promoted to a shared cell) into the saved caller frames by indexing their slot region with the
CALLEE's `code.locals` layout. A routine whose parameter was named like a variable of a method
caller (`data`) therefore wrote the shared cell over the caller's slot of the same index --
slot 0, `self`. LIVR's `when` rule left `$v.errors` unset because `self` of
`LIVR::Validator.validate` became a Hash. The region is now indexed with the caller's own code
layout (`VmCallFrame::caller_code`).
