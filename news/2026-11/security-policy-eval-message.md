# X::SecurityPolicy::Eval for prohibited regex interpolation

Interpolating a string containing a code block into a regex (`/<$s>/` with
`$s = q[{1}]`) without `MONKEY-SEE-NO-EVAL` now throws
`X::SecurityPolicy::Eval` with payload `Prohibited regex interpolation`, as
rakudo does, instead of a bare `X::SecurityPolicy`. The class's `.message` is
no longer a stub ("Stub code executed"): it appends rakudo's "use the
MONKEY-SEE-NO-EVAL pragma to override this error ..." advice to the payload,
word-wrapped at 72 columns. (#18937)
