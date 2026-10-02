# Infix calls enter a proto body before multi dispatch

An infix operator declared with a proto sub now executes the proto's body even when a multi candidate matches its operands. The proto may return its own result or use `{*}` to dispatch to that candidate. This applies to binary and list-associative infix calls.
