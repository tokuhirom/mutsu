//! Whether a role method call can observe its composition bindings.
//!
//! A role method's composition bindings (`T => Int` for `role R[::T]`, and the
//! per-composition rename entries of nested generic classes) are installed into
//! the method's env overlay before its parameters bind
//! (`vm_method_role_bindings`). They exist for the names the method resolves
//! through them: a `T` in its body (`GetTypeCapture`, `GetBareWord`, a type
//! check against `Array[T]`), a `T`-typed parameter, a `--> T` return check.
//! Most role methods name none of them (`method is-some { self ~~ Some }`), yet
//! every call paid for inserting them into a fresh overlay and keeping them
//! out of the exit merge -- ~3.4k instructions of a ~15k role method call
//! (#17331).
//!
//! [`Interpreter::role_bindings_unobservable`] answers, per call, whether the
//! install can be skipped: every binding name is absent from the method body's
//! name tokens (its string and package constants, split into identifier runs)
//! and from its signature's type names, and nothing can reach the bindings by
//! a name the compiler did not see. Both token sets are pure functions of
//! immutable data (the chunk, the declared signature), so they are computed
//! once and memoized on the chunk / the def; the per-call cost is one hash
//! probe per binding.
//!
//! What keeps the install (the answer is "observable"):
//!
//! - a body that creates closures (a closure captures the frame's env, the
//!   bindings included), keeps an AST `stmt_pool`, or reads names reflectively
//!   (`EVAL`, symbolic `::($n)`, pseudo-packages: `needs_reflective_capture`);
//! - any chunk of the program that reads another frame's lexicals by name
//!   (`CALLER::`, `OUTER::`, ...: the process-wide `non_eval_reflective_possible`
//!   and `caller_frame_read_possible` latches), since a callee could then read
//!   `T` out of this frame;
//! - a signature with a default, `where`, sub-signature, code signature, type
//!   capture, shape or trait argument (each an expression or a declaration the
//!   token scan does not see into);
//! - a def carrying its own captured env (`.^add_method` closures, classes
//!   declared in a routine), whose install path re-applies the bindings.

use super::*;
use crate::symbol::Symbol;
use rustc_hash::FxHashSet;

/// Add every identifier run of `s` to `out`: maximal runs of name characters
/// (letters, digits, `_`, `-`, `'`, `:` and `?`), plus each `::`-separated
/// segment of a run, so `Array[T]` yields `Array` and `T`, and `G::A` yields
/// `G::A`, `G` and `A`. Over-collecting only keeps an install.
// Cost: O(n), n = the length of `s`.
fn add_name_tokens(s: &str, out: &mut FxHashSet<Symbol>) {
    let is_name_char = |c: char| c.is_alphanumeric() || matches!(c, '_' | '-' | '\'' | ':' | '?');
    for run in s.split(|c: char| !is_name_char(c)) {
        if run.is_empty() {
            continue;
        }
        out.insert(Symbol::intern(run));
        if run.contains("::") {
            for seg in run.split("::").filter(|seg| !seg.is_empty()) {
                out.insert(Symbol::intern(seg));
            }
        }
        // A twigil-led or `?`-led run (`?T`, `::?ROLE`) also names its bare part.
        let bare = run.trim_start_matches([':', '?']);
        if !bare.is_empty() && bare.len() != run.len() {
            out.insert(Symbol::intern(bare));
        }
    }
}

/// The name tokens of `code`'s constants and of every closure chunk nested in
/// it, or `None` when the chunk can reach a name the scan cannot see.
// Cost: O(c), c = the constants' total length (once per chunk).
fn compute_body_name_tokens(code: &CompiledCode) -> Option<Box<FxHashSet<Symbol>>> {
    if !code.closure_compiled_codes.is_empty()
        || !code.stmt_pool.is_empty()
        || code.needs_reflective_capture
        || code.static_name_lookup
    {
        return None;
    }
    let mut out = FxHashSet::default();
    for constant in &code.constants {
        match constant.view() {
            ValueView::Str(s) => add_name_tokens(s.as_str(), &mut out),
            ValueView::Package(p) => add_name_tokens(p.as_str(), &mut out),
            // A constant that names something by another shape (a Pair, a
            // list of names, a parameter spec) is not tokenized: treat the
            // whole chunk as unanalyzable.
            ValueView::Int(_)
            | ValueView::BigInt(_)
            | ValueView::Num(_)
            | ValueView::Bool(_)
            | ValueView::Nil
            | ValueView::Rat(..) => {}
            _ => return None,
        }
    }
    Some(Box::new(out))
}

/// Add the type names `pd` mentions; `false` when it carries anything the
/// token scan does not see into.
// Cost: O(t), t = the length of its type names.
fn add_param_tokens(pd: &crate::ast::ParamDef, out: &mut FxHashSet<Symbol>) -> bool {
    if pd.default.is_some()
        || pd.where_constraint.is_some()
        || pd.sub_signature.is_some()
        || pd.outer_sub_signature.is_some()
        || pd.code_signature.is_some()
        || pd.type_capture.is_some()
        || pd.shape_constraints.is_some()
        || !pd.trait_args.is_empty()
    {
        return false;
    }
    if let Some(ty) = &pd.type_constraint {
        add_name_tokens(ty, out);
    }
    true
}

impl CompiledCode {
    /// The name tokens of this chunk's constants (see the module docs), or
    /// `None` when it can reach a name the scan cannot see.
    // Cost: O(1) after the first call.
    pub(crate) fn body_name_tokens(&self) -> Option<&FxHashSet<Symbol>> {
        self.body_name_tokens
            .get_or_init(|| compute_body_name_tokens(self))
            .as_deref()
    }
}

impl crate::runtime::MethodDef {
    /// The type names this method's signature mentions (parameter types and
    /// the return type), or `None` when a parameter carries an expression or
    /// declaration the scan does not see into.
    // Cost: O(1) after the first call.
    pub(crate) fn signature_name_tokens(&self) -> Option<&FxHashSet<Symbol>> {
        self.syms
            .signature_name_tokens
            .get_or_init(|| {
                let mut out = FxHashSet::default();
                for pd in self.param_defs.iter() {
                    if !add_param_tokens(pd, &mut out) {
                        return None;
                    }
                }
                if let Some(ret) = &self.return_type {
                    add_name_tokens(ret, &mut out);
                }
                Some(Box::new(out))
            })
            .as_deref()
    }
}

impl Interpreter {
    /// Whether a call of `method_def` (body `cc`) can observe none of the
    /// composition bindings named by `syms`, so installing them may be skipped
    /// (see the module docs).
    // Cost: O(b), b = the bindings (one hash probe each per token set).
    pub(super) fn role_bindings_unobservable<'a>(
        method_def: &crate::runtime::MethodDef,
        cc: &CompiledCode,
        syms: impl IntoIterator<Item = &'a Symbol>,
    ) -> bool {
        if method_def.captured_env.is_some()
            || crate::opcode::non_eval_reflective_possible()
            || crate::opcode::caller_frame_read_possible()
        {
            return false;
        }
        let (Some(body), Some(signature)) =
            (cc.body_name_tokens(), method_def.signature_name_tokens())
        else {
            return false;
        };
        syms.into_iter()
            .all(|sym| !body.contains(sym) && !signature.contains(sym))
    }
}
