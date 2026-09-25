# `temp %h<key> //= value` parses: compound assignment to a temporized element

`Net::HTTP`'s `Net::HTTP::POST` (a dependency of `raku-mailgun` and
`WebService::Slack::Webhook`) fills in default headers with

```raku
temp %header<Connection> //= <keep-alive>;
temp %header<User-Agent> //= <raku-net-http>;
```

and mutsu could not load it. The error surfaced as this #7988 cluster's generic
`Confused. ... expected expression statement or ')'`, reported two lines up at
the enclosing `multi method CALL-ME(Str:D $abs-url, :%header is copy, ...)`
header, which made the signature look guilty.

The `let` / `temp` element parser (`let_subscript_stmt`, introduced when the
brace-subscript form was added) accepted only `= value` after the subscript. A
compound assignment (`//=`, `+=`, `~=`, `.=`, ...) now saves the element and
then runs the whole assignment as an ordinary expression, the same lowering
`let_compound_assign_stmt` already uses for a plain variable
(`temp $x //= 5`).

Pinned by four new cases in
`t/collections/subscript/temp-let-brace-subscript.t`. Every `raku-mailgun` and
`WebService::Slack::Webhook` test file now matches rakudo.
