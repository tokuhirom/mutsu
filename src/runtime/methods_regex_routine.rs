//! `Regex`-as-`Routine` introspection: `.signature`, `.arity`, `.count`.
//!
//! A Raku `Regex` is a `Method`, so it answers the whole `Routine`
//! introspection surface — and the answer is never just the parameters the
//! declarator wrote. Every regex signature carries the cursor invocant
//! (`Mu $::`) and the implicit `*%_` that every method signature gets, so a
//! bare `rx/a/`, which declares nothing at all, still has a two-element
//! signature:
//!
//! ```text
//! $ raku -e 'say rx/a/.signature.raku'
//! :(Mu $:: *%_)
//! $ raku -e 'my $t = token ($x) { \d+ }; say $t.signature.raku'
//! :(Mu $:: $x, *%_)
//! ```
//!
//! mutsu stores a regex as a pattern string, with (since #8293) an anonymous
//! declarator's parameters riding alongside it on the value itself
//! ([`Value::regex_signature`]); a grammar `token`/`rule`/`regex`'s live on
//! its `Registry::token_defs` `FunctionDef`. So the *declared* half was
//! already reachable from both shapes and only the synthesized half, plus a
//! dispatch entry point, was missing — mutsu answered `No such method` for
//! all three names. See #8318.

use crate::ast::ParamDef;
use crate::runtime::Interpreter;
use crate::value::Value;
use crate::value::signature::SigInfo;

impl Interpreter {
    /// The full parameter list a `Regex`'s signature reports: the cursor
    /// invocant, the declarator's own parameters, and the implicit `*%_`.
    ///
    /// `invocant_type` is `Mu` for a free-standing regex value — Rakudo
    /// constrains the invocant to the *cursor* type, which for a regex that
    /// belongs to no grammar is left wide open — and the declaring grammar
    /// for a `token`/`rule`/`regex` reached through it (`G.^lookup('foo')`
    /// answers `:(G $:: $x, *%_)`).
    pub(super) fn regex_routine_param_defs(
        invocant_type: &str,
        declared: &[ParamDef],
    ) -> Vec<ParamDef> {
        let mut defs = Vec::with_capacity(declared.len() + 2);
        defs.push(Self::make_invocant_param(invocant_type));
        // `effective_method_param_defs` is the same helper that decides what
        // a method *body* sees, so a declarator that writes its own `*%foo`
        // does not get a second named slurpy here either.
        defs.extend(crate::method_signature_shared::effective_method_param_defs(
            declared, false,
        ));
        defs
    }

    /// [`Self::regex_routine_param_defs`] as a `SigInfo`. A regex declares no
    /// return type (`rx/a/.signature.returns` is `Mu`), so there is none to
    /// thread.
    pub(super) fn regex_routine_sig_info(invocant_type: &str, declared: &[ParamDef]) -> SigInfo {
        crate::value::signature::param_defs_to_sig_info(
            &Self::regex_routine_param_defs(invocant_type, declared),
            None,
        )
    }

    /// `.signature`, `.arity` or `.count` on a `Regex` *value* (a `/.../` or
    /// `rx/.../` literal, or an anonymous `token`/`rule`/`regex` term).
    ///
    /// `method` must be one of those three and `target` must be a regex; the
    /// caller's match arm guarantees both.
    pub(super) fn regex_value_routine_introspection(&self, target: &Value, method: &str) -> Value {
        let declared = target.regex_signature();
        let declared = declared.as_deref().map(Vec::as_slice).unwrap_or(&[]);
        let info = Self::regex_routine_sig_info("Mu", declared);
        match method {
            "signature" => crate::value::signature::make_signature_value(info, Some(self)),
            "arity" => Value::int(Self::signature_required_positional_count(&info)),
            _ => Self::signature_count_value(&info),
        }
    }
}
