# A multi sub declared in a method keeps the method's self when returned

`&name` of a `multi sub` declared inside a method built its dispatcher and
candidate code objects without the creating frame's `self`/`?CLASS`/`?ROLE`
record (ADR-14843), so calling the value after the method returned died with
"'self' used where no object is available". CSS::Properties' Proxy `STORE`
subs (`t/threads.t`, `$css.margin = 5pt`) hit it. The dispatcher and its
candidates now capture the record, as a single routine's value already did.
