# Named placeholder signature introspection

Implicit `$:name` and `&:name` placeholders now expose their argument keys without the placeholder twigil through `Parameter.named_names`. Their signatures render as required named parameters, so code that selects arguments from signature metadata can pass those arguments into blocks and subs.
