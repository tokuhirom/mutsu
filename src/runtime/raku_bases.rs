//! `nqp::gethllsym('Raku', 'Grammar')` / `('Raku', 'Actions')`: the base
//! grammar and actions classes of the Raku language, which modules such as
//! `RakuAST::Deparse::Highlight` subclass (`my class Actions is RakuActions`).
//!
//! They are declared on first request, as ordinary Raku classes named
//! `Raku::Grammar` and `Raku::Actions` (the names rakudo's own report), and
//! bound as the HLL symbols.

use super::*;

/// The classes. Productions of the core language are the Rust parser's; a
/// subclass overrides the action methods it wants to observe.
const RAKU_BASES: &str = r#"
grammar GLOBAL::Raku::Grammar {
    # The compile-time error a production raises: the exception class named
    # by `$type` (its `X::Comp` form, as rakudo throws it) with `%args`.
    method typed-panic(Str:D $type, *%args) {
        ::($type).^mixin(X::Comp).new(|%args).throw
    }
    # HLL::Grammar's `panic`: a compile-time error carrying the message.
    method panic(*@args) { self.typed-panic('X::AdHoc', payload => @args.join) }
}
class GLOBAL::Raku::Actions { }
nqp::bindhllsym('Raku', 'Grammar', GLOBAL::Raku::Grammar);
nqp::bindhllsym('Raku', 'Actions', GLOBAL::Raku::Actions);
"#;

impl Interpreter {
    /// Declare and bind the base classes unless an earlier request did.
    // Cost: the parse and run of the class declarations, once per interpreter.
    pub(super) fn ensure_raku_bases(&mut self) -> Result<(), RuntimeError> {
        if !self.get_hll_sym("Raku", "Grammar").is_nil() {
            return Ok(());
        }
        self.eval_code_string(&format!("use nqp;\n{RAKU_BASES}"))?;
        Ok(())
    }
}
