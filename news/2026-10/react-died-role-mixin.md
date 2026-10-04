# A dying `react` rethrows the original exception `but X::React::Died`

A `react` block whose `whenever` died used to throw a brand-new
`X::React::Died` instance whose `.message`/`.Str` was the whole
"A react block: ... Died because of the exception: ..." report and which was
no longer an `X::AdHoc` (or the user's own exception class). Code that
catches around a `react` and matches on the text -- MoarVM::Remote retries
`when .Str.contains("connection refused")` -- never matched.

mutsu now does what rakudo does: `X::React::Died` is a role, mixed into the
original exception (`X::AdHoc+{X::React::Died}`), so type checks,
`.message` and `.Str` see the original and only `.gist` renders the report,
with the react block's own location under the header. The gist rendering is
shared with `X::Promise::Broken`, the other rakudo role that works this way
(`src/runtime/wrapper_role_gist.rs`).
