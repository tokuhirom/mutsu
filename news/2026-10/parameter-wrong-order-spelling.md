# X::Parameter::WrongOrder spells the parameter as written

`sub g(:$a, @p) { }` reported "Cannot put required parameter `$@p` after named
parameters": the signature-order check prefixed `$` to every parameter name,
but only a scalar parameter's stored name lacks its sigil. The message and the
exception's `parameter` attribute now spell the parameter as Rakudo does —
`@p`, `%h`, `&f`, a sigilless `x` bare, and an anonymous parameter as its bare
sigil (`$`, `@`, `%`) instead of an internal placeholder name (#11373).
