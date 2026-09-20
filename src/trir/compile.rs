//! AST -> [`TrChunk`]: the resolution pass of ADR-0110 §3.
//!
//! Conservative and all-or-nothing (ADR-0110 §4): every construct this
//! compiler does not *prove* it can execute with static operand kinds makes
//! the whole routine decline, and a declined routine takes today's untyped
//! path unchanged. There is therefore no fallback arm anywhere below — the
//! only two outcomes are a complete chunk and `None`.

use std::collections::HashMap;

use super::{TrChunk, TrKind, TrOp, TrOuter, TrParam};
use crate::ast::{ParamDef, Stmt};
use crate::symbol::Symbol;
use crate::value::Value;

/// Where a name resolves inside the routine being compiled.
#[derive(Debug, Clone, Copy)]
struct Binding {
    slot: u16,
    kind: TrKind,
}

pub(crate) struct TrirCompiler {
    ops: Vec<TrOp>,
    constants: Vec<Value>,
    locals: HashMap<String, Binding>,
    outers: Vec<TrOuter>,
    outer_index: HashMap<String, u16>,
    n_native: u16,
    n_obj: u16,
    params: Vec<TrParam>,
    /// Boxed slots this body assigns to after their declaration. A slot that
    /// is written invalidates the per-frame character memo, so `OrdAtLocal`
    /// may only target a slot that is never re-assigned (Stage 1 keeps the
    /// memo immutable rather than tracking invalidation).
    obj_written: Vec<bool>,
}

/// Whether a parameter's recorded name is a plain `$`-scalar lexical.
///
/// `ParamDef::name` carries the spelling with the `$` stripped but every
/// other marker intact, so this is the gate that keeps out an ATTRIBUTIVE
/// parameter (`$!t` arrives as `"!t"`, and binding it must reach `self`'s
/// attribute cell, which only the general binder does — `sub s($!t) {}` with
/// an empty body is otherwise a perfectly provable TRIR routine that silently
/// discards its argument), a `@`/`%`/`&` container, a `*`-slurpy, a dynamic
/// (`*foo`), a compiler variable (`?FILE`), and the anonymous `_`.
fn plain_scalar_param_name(name: &str) -> bool {
    let mut chars = name.chars();
    let Some(first) = chars.next() else {
        return false;
    };
    if !(first.is_alphabetic() || first == '_') {
        return false;
    }
    // `$self` is the reserved invocant lexical (ADR-0061), not an ordinary
    // parameter name.
    if name == "_" || name == "self" || name.starts_with("__") {
        return false;
    }
    name.chars()
        .all(|c| c.is_alphanumeric() || c == '_' || c == '-' || c == '\'')
}

/// The native scalar spellings TRIR gives a native slot. Everything else —
/// including the boxed nominal types `Int`/`Str`/`Num` — is a boxed slot,
/// because a boxed parameter legitimately accepts a bare type object and a
/// native one must reject it (`FastParamType`'s own note).
fn native_kind_of(tc: Option<&str>) -> Option<TrKind> {
    match tc {
        Some("int") => Some(TrKind::Int),
        Some("num") => Some(TrKind::Num),
        _ => None,
    }
}

impl TrirCompiler {
    /// Compile `body` under `param_defs` to a [`TrChunk`], or decline.
    ///
    /// `name` is the routine's name, used only in error messages.
    pub(crate) fn compile(
        name: Symbol,
        param_defs: &[ParamDef],
        params: &[String],
        return_type: Option<&str>,
        body: &[Stmt],
    ) -> Option<TrChunk> {
        if !TrChunk::enabled() {
            return None;
        }
        // A legacy bare-name parameter list carries no types at all, so
        // nothing here could be proved; and a signature mixing the two never
        // occurs (`CompiledFunction`'s own invariant).
        if !params.is_empty() && param_defs.is_empty() {
            return None;
        }
        // ADR-0110 §7 Stage 1 supports only `Nil` and an unconstrained
        // return; a nominal return type needs the general path's coercion.
        if !matches!(return_type, None | Some("Nil")) {
            return None;
        }
        let mut c = TrirCompiler {
            ops: Vec::new(),
            constants: Vec::new(),
            locals: HashMap::new(),
            outers: Vec::new(),
            outer_index: HashMap::new(),
            n_native: 0,
            n_obj: 0,
            params: Vec::new(),
            obj_written: Vec::new(),
        };
        c.declare_params(param_defs)?;
        let returns_nil = return_type == Some("Nil");
        let last = c.compile_body(body, returns_nil)?;
        // The routine's value: `--> Nil` discards it, otherwise the last
        // statement's value is the result.
        match (returns_nil, last) {
            (true, _) | (false, None) => c.ops.push(TrOp::ReturnNil),
            (false, Some(TrKind::Int)) => c.ops.push(TrOp::ReturnI),
            (false, Some(TrKind::Num)) => c.ops.push(TrOp::ReturnN),
            (false, Some(TrKind::Obj)) => c.ops.push(TrOp::ReturnObj),
        }
        Some(TrChunk {
            ops: c.ops,
            constants: c.constants,
            n_native: c.n_native,
            n_obj: c.n_obj,
            params: c.params,
            outers: c.outers,
            name,
        })
    }

    fn declare_params(&mut self, param_defs: &[ParamDef]) -> Option<()> {
        for pd in param_defs {
            // ADR-0110 §4.4: positional scalars only, no slurpy/named/where/
            // sub-signature/default/capture/attributive forms.
            if pd.named
                || pd.slurpy
                || pd.double_slurpy
                || pd.onearg
                || pd.sigilless
                || pd.is_invocant
                || pd.optional_marker
                || pd.default.is_some()
                || pd.sub_signature.is_some()
                || pd.outer_sub_signature.is_some()
                || pd.where_constraint.is_some()
                || pd.code_signature.is_some()
                || pd.type_capture.is_some()
                || pd.literal_value.is_some()
                || pd.shape_constraints.is_some()
                || !pd.trait_args.is_empty()
                || !plain_scalar_param_name(&pd.name)
            {
                return None;
            }
            let is_rw = pd.traits.iter().any(|t| t == "rw");
            // `is rw` is the only trait Stage 1 understands; `is copy`,
            // `is raw` and every custom trait decline.
            if pd.traits.iter().any(|t| t != "rw") {
                return None;
            }
            let tc = pd.type_constraint.as_deref();
            let kind = match native_kind_of(tc) {
                Some(k) => k,
                // A boxed parameter is admitted only when it is untyped or a
                // native `str`: any other constraint needs the general
                // binder's type check, which TRIR does not reproduce.
                None if tc.is_none() || tc == Some("str") => TrKind::Obj,
                None => return None,
            };
            // Only a native parameter may be `is rw` here: a boxed one would
            // need a real container, which is exactly what the untyped path
            // already does well (ADR-0109).
            if is_rw && !kind.is_native() {
                return None;
            }
            let slot = self.alloc(&pd.name, kind);
            let type_name = match tc {
                Some("int") => "int",
                Some("num") => "num",
                Some("str") => "str",
                _ => "",
            };
            self.params.push(TrParam {
                slot,
                kind,
                is_rw,
                type_name,
            });
        }
        Some(())
    }

    fn alloc(&mut self, name: &str, kind: TrKind) -> u16 {
        let slot = if kind.is_native() {
            let s = self.n_native;
            self.n_native += 1;
            s
        } else {
            let s = self.n_obj;
            self.n_obj += 1;
            self.obj_written.push(false);
            s
        };
        self.locals.insert(name.to_string(), Binding { slot, kind });
        slot
    }

    fn outer(&mut self, name: &str) -> u16 {
        if let Some(i) = self.outer_index.get(name) {
            return *i;
        }
        let i = self.outers.len() as u16;
        self.outers.push(TrOuter {
            name: Symbol::intern(name),
        });
        self.outer_index.insert(name.to_string(), i);
        i
    }

    fn add_const(&mut self, v: Value) -> u32 {
        let i = self.constants.len() as u32;
        self.constants.push(v);
        i
    }

    /// Compile a statement list, answering the kind the LAST statement left
    /// on a bank (`None` when it left nothing).
    fn compile_body(&mut self, body: &[Stmt], sink_all: bool) -> Option<Option<TrKind>> {
        let mut last: Option<TrKind> = None;
        // Index of the final value-bearing statement, so every earlier one can
        // be sunk.
        let value_stmts: Vec<usize> = body
            .iter()
            .enumerate()
            .filter(|(_, s)| !matches!(s, Stmt::SetLine(_)))
            .map(|(i, _)| i)
            .collect();
        let final_idx = value_stmts.last().copied();
        for (i, stmt) in body.iter().enumerate() {
            match stmt {
                Stmt::SetLine(_) => continue,
                Stmt::Expr(e) => {
                    if !sink_all && Some(i) == final_idx {
                        last = Some(self.compile_expr(e)?);
                    } else {
                        self.compile_expr_sink(e)?;
                    }
                }
                Stmt::VarDecl {
                    name,
                    expr,
                    type_constraint,
                    is_state,
                    is_our,
                    is_dynamic,
                    is_export,
                    custom_traits,
                    where_constraint,
                    ..
                } => {
                    if *is_state
                        || *is_our
                        || *is_dynamic
                        || *is_export
                        || where_constraint.is_some()
                        || name.starts_with(['@', '%', '&'])
                    {
                        return None;
                    }
                    // `__has_initializer` is the parser's own marker for
                    // `my T $x = ...`; anything else (`__scalar_bind`, a
                    // user trait) declines.
                    if custom_traits.iter().any(|(t, _)| t != "__has_initializer") {
                        return None;
                    }
                    let tc = type_constraint.as_deref();
                    let kind = match native_kind_of(tc) {
                        Some(k) => k,
                        None if tc.is_none() || tc == Some("str") => TrKind::Obj,
                        None => return None,
                    };
                    let init = self.compile_expr(expr)?;
                    self.coerce(init, kind)?;
                    let slot = self.alloc(name, kind);
                    self.store(slot, kind);
                    if !sink_all && Some(i) == final_idx {
                        // A declaration in value position yields the bound
                        // value; re-read it rather than duplicating a bank.
                        self.load(slot, kind);
                        last = Some(kind);
                    }
                }
                _ => return None,
            }
        }
        Some(last)
    }

    fn drop_top(&mut self, kind: TrKind) {
        match kind {
            TrKind::Int | TrKind::Num => self.ops.push(TrOp::PopI),
            TrKind::Obj => self.ops.push(TrOp::PopObj),
        }
    }

    fn load(&mut self, slot: u16, kind: TrKind) {
        match kind {
            TrKind::Int | TrKind::Num => self.ops.push(TrOp::LoadI(slot)),
            TrKind::Obj => self.ops.push(TrOp::LoadObj(slot)),
        }
    }

    fn store(&mut self, slot: u16, kind: TrKind) {
        match kind {
            TrKind::Int | TrKind::Num => self.ops.push(TrOp::StoreI(slot)),
            TrKind::Obj => {
                self.obj_written[slot as usize] = true;
                self.ops.push(TrOp::StoreObj(slot));
            }
        }
    }

    /// Emit the conversion from `have` to `want`, or decline when there is
    /// none TRIR performs without the general binder's coercion rules.
    fn coerce(&mut self, have: TrKind, want: TrKind) -> Option<()> {
        match (have, want) {
            (a, b) if a == b => Some(()),
            (TrKind::Int, TrKind::Num) => {
                self.ops.push(TrOp::IntToNum);
                Some(())
            }
            (TrKind::Num, TrKind::Int) => {
                self.ops.push(TrOp::NumToInt);
                Some(())
            }
            (TrKind::Int, TrKind::Obj) => {
                self.ops.push(TrOp::BoxI);
                Some(())
            }
            (TrKind::Num, TrKind::Obj) => {
                self.ops.push(TrOp::BoxN);
                Some(())
            }
            // Unboxing is a checked boundary op, but "is this boxed value an
            // int" is a run-time question, so an implicit narrowing from a
            // boxed expression into a native slot declines instead: Stage 1
            // never silently coerces where the general binder would raise.
            (TrKind::Obj, _) => None,
            // Unreachable: the equality guard above covers both, but the
            // exhaustiveness check does not count guarded arms.
            (TrKind::Int, TrKind::Int) | (TrKind::Num, TrKind::Num) => Some(()),
        }
    }

    fn binding_of(&self, name: &str) -> Option<Binding> {
        self.locals.get(name).copied()
    }

    /// Whether boxed slot `slot` is ever assigned after its declaration. The
    /// operand-direct string reads memoize the slot's characters for the
    /// frame, which is only sound while nothing rewrites the slot.
    fn obj_slot_written(&self, slot: u16) -> bool {
        self.obj_written.get(slot as usize).copied().unwrap_or(true)
    }
}

mod expr;
