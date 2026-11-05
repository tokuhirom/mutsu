# Private-attribute code variables are carried through finalization

`&!code`, a private attribute holding code, is kept by `finalize_unit` as the parser built it
instead of being converted to a lexical `&` variable node and lowered back to the same name
(ADR-15760 slice 2). `t/rakuast/rakuast-private-code-var-finalize.t` pins the semantics.
