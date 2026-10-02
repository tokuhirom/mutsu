# `:sigspace`'s `<.ws>` is a compiled regex op

The `<.ws>` token that `:sigspace` inserts for significant whitespace
(`RegexAtom::WsRule`) used to run inside the compiled regex engine (ADR-0135)
as a `CapAtom`, i.e. a call back into the tree walk's single-atom matcher, so
every `:s` pattern still leaned on the walk (`regex-walk: … leaf=(ws-rule=…)`
under `MUTSU_VM_STATS`).

It now compiles to its own op, `RxOp::Ws`, which matches `<!ww> \s*`
committed to its longest run. The built-in semantics live in one function,
`regex_helpers::ws_rule_end`, shared by the walk and the op; a `ws` method
wrapped with `.wrap` is still dispatched through its wrapper chain. The
differential corpus (`tests/regex_vm_differential.rs`) gained a sigspace case
and an engagement check, and `t/regex/regex-sigspace-ws-compiled.t` pins the
semantics against Rakudo (#10403).
