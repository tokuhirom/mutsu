//! `constant` type aliases in type constraints, resolved at compile time
//! (#13728).
//!
//! `constant time = int64; sub f(time \year)` declares `f` with the
//! signature `:(int64 \year)` in rakudo: the alias is settled when the
//! signature compiles. Left to run time, mutsu looked the alias up by name
//! when the binder or a declaration's type check ran, through the frames that
//! called the routine, so a caller's same-named sigilless `my \time`
//! answered. The compiler's alias table ([`Compiler::type_aliases`]) is the
//! unit's `constant X = Type` declarations.

use super::Compiler;
use crate::ast::Stmt;
use crate::value::Value;

impl Compiler {
    /// `stmt` (a `SubDecl`) with each parameter type that names a `constant`
    /// type alias (`constant T = int64; sub f(T \x)`) replaced by the alias's
    /// target, or `None` when no parameter names one. Rakudo resolves the
    /// alias when it compiles the signature (`&f.signature` is `:(int64 \x)`);
    /// left to the binder, the name was looked up by name at call time, where
    /// a caller's same-named sigilless `my \T` answered (#13728).
    // Cost: O(p), p = parameters of the declaration's signatures.
    pub(super) fn with_param_type_aliases_resolved(&self, stmt: &Stmt) -> Option<Stmt> {
        if self.type_aliases.is_empty() && self.outer_type_aliases.is_empty() {
            return None;
        }
        let Stmt::SubDecl {
            param_defs,
            signature_alternates,
            ..
        } = stmt
        else {
            return None;
        };
        let main = self.param_defs_with_type_aliases_resolved(param_defs);
        let alternates: Vec<_> = signature_alternates
            .iter()
            .map(|(_, defs)| self.param_defs_with_type_aliases_resolved(defs))
            .collect();
        if main.is_none() && alternates.iter().all(Option::is_none) {
            return None;
        }
        let mut resolved = stmt.clone();
        if let Stmt::SubDecl {
            param_defs,
            signature_alternates,
            ..
        } = &mut resolved
        {
            if let Some(defs) = main {
                *param_defs = defs;
            }
            for ((_, defs), alt) in signature_alternates.iter_mut().zip(alternates) {
                if let Some(alt) = alt {
                    *defs = alt;
                }
            }
        }
        Some(resolved)
    }

    /// `defs` with each parameter type that names a `constant` type alias
    /// replaced by the alias's target ([`Self::resolve_type_alias_constraint`]),
    /// or `None` when no parameter names one (see
    /// [`Self::with_param_type_aliases_resolved`]).
    // Cost: O(p * a), p = `defs`, a = the alias chain's length (at most 16).
    pub(super) fn param_defs_with_type_aliases_resolved(
        &self,
        defs: &[crate::ast::ParamDef],
    ) -> Option<Vec<crate::ast::ParamDef>> {
        if self.type_aliases.is_empty() && self.outer_type_aliases.is_empty() {
            return None;
        }
        let resolved_at = |pd: &crate::ast::ParamDef| {
            let tc = pd.type_constraint.as_deref()?;
            let resolved = self.resolve_type_alias_constraint(tc);
            (resolved != tc).then_some(resolved)
        };
        if !defs.iter().any(|pd| resolved_at(pd).is_some()) {
            return None;
        }
        Some(
            defs.iter()
                .map(|pd| match resolved_at(pd) {
                    Some(resolved) => crate::ast::ParamDef {
                        type_constraint: Some(resolved),
                        ..pd.clone()
                    },
                    None => pd.clone(),
                })
                .collect(),
        )
    }

    /// The constant index of the type constraint text at `tc_idx` with a
    /// `constant` type alias resolved ([`Self::resolve_type_alias_constraint`]):
    /// `tc_idx` itself when the text names no alias.
    // Cost: O(a), a = the alias chain's length (at most 16); O(1) in a unit
    // that declares no alias.
    pub(super) fn type_alias_resolved_constant(&mut self, tc_idx: u32) -> u32 {
        if self.type_aliases.is_empty() && self.outer_type_aliases.is_empty() {
            return tc_idx;
        }
        let Some(tc) = self
            .code
            .constants
            .get(tc_idx as usize)
            .and_then(|c| c.as_str().map(str::to_owned))
        else {
            return tc_idx;
        };
        let resolved = self.resolve_type_alias_constraint(&tc);
        if resolved == tc {
            return tc_idx;
        }
        self.code.add_constant(Value::str(resolved))
    }
}
