//! AST -> [`TrChunk`]: the resolution pass of ADR-0110 §3.
//!
//! Conservative and all-or-nothing (ADR-0110 §4): every construct this
//! compiler does not *prove* it can execute with static operand kinds makes
//! the whole routine decline, and a declined routine takes today's untyped
//! path unchanged. There is therefore no fallback arm anywhere below — the
//! only two outcomes are a complete chunk and `None`.

use std::collections::HashMap;

use super::{TrChunk, TrKind, TrOp, TrOuter, TrParam};
use crate::ast::{Expr, ParamDef, Stmt};
use crate::symbol::Symbol;
use crate::value::Value;

/// Where a name resolves inside the routine being compiled.
#[derive(Debug, Clone, Copy)]
struct Binding {
    slot: u16,
    kind: TrKind,
}

pub(crate) struct TrirCompiler<'a> {
    ops: Vec<TrOp>,
    constants: Vec<Value>,
    locals: HashMap<String, Binding>,
    outers: Vec<TrOuter>,
    outer_index: HashMap<String, u16>,
    n_native: u16,
    n_obj: u16,
    params: Vec<TrParam>,
    /// Boxed slots this body assigns to after their declaration.
    obj_written: Vec<bool>,
    /// The calls this body makes, indexed by `CallTr`/`CallGen`.
    pub(super) calls: Vec<crate::trir::TrInnerCall>,
    /// Whether any call has been emitted — see [`TrChunk::has_calls`].
    pub(super) has_calls: bool,
    /// Routines this compile has already registered with a chunk, so a call
    /// to one can be linked statically (ADR-0110 §3.3).
    pub(super) routines: Option<&'a TrirRoutineMap>,
    /// The compile's function table, for reading a resolved callee's
    /// signature.
    pub(super) fns: Option<&'a crate::opcode::CompiledFns>,
    /// The first construct that made this routine decline, recorded only when
    /// `MUTSU_TRIR_DUMP` asked. Widening the admitted set is otherwise a
    /// guessing game: "declined" alone does not say which of a hundred nodes
    /// in a JSON::Fast scanner was the one.
    pub(super) why: Option<String>,
    /// Whether the value the last `compile_expr` left on a bank came from an
    /// `nqp::` op. Only such a value may be narrowed from boxed to native
    /// ([`Self::coerce`]): `nqp`'s own `iarg` coercion IS those ops'
    /// semantics, where the general binder would reject the same narrowing
    /// on a `my int $x = <arbitrary boxed>`.
    pub(super) nqp_sourced: bool,
}

/// A statement's variant name, for a decline report.
fn discriminant_of(s: &Stmt) -> String {
    let rendered = format!("{s:?}");
    rendered
        .split(|c: char| c == ' ' || c == '(' || c == '{')
        .next()
        .unwrap_or("?")
        .to_string()
}

/// `(routine name, positional arity)` -> its `CompiledFns` key and body
/// fingerprint.
pub(crate) type TrirRoutineMap = HashMap<(String, usize), (Symbol, u64)>;

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

impl<'a> TrirCompiler<'a> {
    /// Compile `body` under `param_defs` to a [`TrChunk`], or decline.
    ///
    /// `name` is the routine's name, used only in error messages.
    pub(crate) fn compile(
        name: Symbol,
        param_defs: &[ParamDef],
        params: &[String],
        return_type: Option<&str>,
        body: &[Stmt],
        routines: Option<&'a TrirRoutineMap>,
        fns: Option<&'a crate::opcode::CompiledFns>,
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
            calls: Vec::new(),
            has_calls: false,
            routines,
            fns,
            why: None,
            nqp_sourced: false,
        };
        if c.declare_params(param_defs).is_none() {
            return c.declined(name);
        }
        let returns_nil = return_type == Some("Nil");
        let Some(last) = c.compile_body(body, returns_nil) else {
            return c.declined(name);
        };
        // The routine's value: `--> Nil` discards it, otherwise the last
        // statement's value is the result.
        match (returns_nil, last) {
            (true, _) | (false, None) => c.ops.push(TrOp::ReturnNil),
            (false, Some(TrKind::Int)) => c.ops.push(TrOp::ReturnI),
            (false, Some(TrKind::Num)) => c.ops.push(TrOp::ReturnN),
            (false, Some(TrKind::Obj)) => c.ops.push(TrOp::ReturnObj),
        }
        Some(TrChunk {
            id: super::next_chunk_id(),
            ops: c.ops,
            constants: c.constants,
            n_native: c.n_native,
            n_obj: c.n_obj,
            params: c.params,
            outers: c.outers,
            name,
            calls: c.calls,
            has_calls: c.has_calls,
        })
    }

    /// Report the recorded decline reason (when asked) and answer `None`.
    fn declined(&self, name: Symbol) -> Option<TrChunk> {
        if std::env::var("MUTSU_TRIR_WHY").is_ok() {
            let why = self.why.as_deref().unwrap_or("an unrecorded construct");
            eprintln!("trir: {} declined on {why}", name.as_str());
        }
        None
    }

    /// Record the first construct that could not be proved.
    pub(super) fn note_decline(&mut self, what: impl FnOnce() -> String) {
        if self.why.is_none() {
            self.why = Some(what());
        }
    }

    fn declare_params(&mut self, param_defs: &[ParamDef]) -> Option<()> {
        for pd in param_defs {
            // ADR-0110 §4.4: positional scalars only, no slurpy/named/where/
            // sub-signature/default/capture/attributive forms.
            let shape_reason = if pd.named {
                "named"
            } else if pd.slurpy || pd.double_slurpy || pd.onearg {
                "slurpy"
            } else if pd.optional_marker || pd.default.is_some() {
                "optional or defaulted"
            } else if pd.sub_signature.is_some() || pd.outer_sub_signature.is_some() {
                "destructuring"
            } else if pd.where_constraint.is_some() {
                "where-constrained"
            } else if pd.code_signature.is_some() {
                "code-signature"
            } else if pd.type_capture.is_some() {
                "type-capturing"
            } else if pd.literal_value.is_some() {
                "literal"
            } else if pd.shape_constraints.is_some() {
                "shaped"
            } else if pd.is_invocant {
                "invocant"
            } else if pd.sigilless {
                "sigilless"
            } else if !pd.trait_args.is_empty() {
                "trait-argument"
            } else {
                "not a plain $ lexical"
            };
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
                let n = pd.name.clone();
                self.note_decline(|| format!("parameter ${n} is {shape_reason}"));
                return None;
            }
            let is_rw = pd.traits.iter().any(|t| t == "rw");
            // `is rw` is the only trait Stage 1 understands; `is copy`,
            // `is raw` and every custom trait decline.
            if pd.traits.iter().any(|t| t != "rw") {
                let t = pd.traits.join(" ");
                self.note_decline(|| format!("parameter trait is {t}"));
                return None;
            }
            let tc = pd.type_constraint.as_deref();
            let kind = match native_kind_of(tc) {
                Some(k) => k,
                // A boxed parameter is admitted only when it is untyped or a
                // native `str`: any other constraint needs the general
                // binder's type check, which TRIR does not reproduce.
                None if tc.is_none() || tc == Some("str") => TrKind::Obj,
                None => {
                    let t = tc.unwrap_or("").to_string();
                    self.note_decline(|| format!("parameter type {t}"));
                    return None;
                }
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

    pub(super) fn outer(&mut self, name: &str) -> u16 {
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

    pub(super) fn add_const(&mut self, v: Value) -> u32 {
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
                Stmt::VarDecl { .. } => {
                    let keep = !sink_all && Some(i) == final_idx;
                    last = self.compile_var_decl(stmt, keep)?;
                }
                Stmt::Assign { name, expr, op } => {
                    if !matches!(op, crate::ast::AssignOp::Assign) {
                        self.note_decline(|| format!("compound assignment {op:?}"));
                        return None;
                    }
                    let Binding { slot, kind } = self.binding_of(name).or_else(|| {
                        self.note_decline(|| format!("assignment to non-local {name}"));
                        None
                    })?;
                    let got = self.compile_expr(expr)?;
                    self.coerce(got, kind)?;
                    self.store(slot, kind);
                    if !sink_all && Some(i) == final_idx {
                        self.load(slot, kind);
                        last = Some(kind);
                    }
                }
                Stmt::If {
                    cond,
                    then_branch,
                    else_branch,
                    binding_var: None,
                    ..
                } => {
                    self.compile_if(cond, then_branch, else_branch)?;
                }
                // `die EXPR` is a call, and compiling it as one is what lets a
                // routine whose hot loop is typed keep its cold error path
                // instead of declining for it.
                Stmt::Die(e) => {
                    self.compile_routine_call("die", std::slice::from_ref(e))?;
                    self.ops.push(TrOp::PopObj);
                }
                Stmt::Return(e) => {
                    let kind = self.compile_expr(e)?;
                    self.ops.push(match kind {
                        TrKind::Int => TrOp::ReturnI,
                        TrKind::Num => TrOp::ReturnN,
                        TrKind::Obj => TrOp::ReturnObj,
                    });
                    // A `return` ends this statement list's value flow; a
                    // following statement is dead but still has to compile,
                    // and `last` stays whatever the tail yields.
                }
                other => {
                    self.note_decline(|| format!("statement {}", discriminant_of(other)));
                    return None;
                }
            }
        }
        Some(last)
    }

    /// `if COND { ... } else { ... }` as a statement. Both branches are
    /// compiled in sink position: an `if` statement's own value is not one
    /// TRIR tracks, and every JSON::Fast use of it is for effect.
    fn compile_if(
        &mut self,
        cond: &crate::ast::Expr,
        then_branch: &[Stmt],
        else_branch: &[Stmt],
    ) -> Option<()> {
        let ck = self.compile_expr(cond)?;
        self.truthy(ck)?;
        let branch_at = self.ops.len();
        self.ops.push(TrOp::JumpIfFalseI(0));
        self.compile_body(then_branch, true)?;
        let jump_end_at = self.ops.len();
        self.ops.push(TrOp::Jump(0));
        let else_at = self.ops.len() as u32;
        self.compile_body(else_branch, true)?;
        let end = self.ops.len() as u32;
        match &mut self.ops[branch_at] {
            TrOp::JumpIfFalseI(x) => *x = else_at,
            _ => return None,
        }
        match &mut self.ops[jump_end_at] {
            TrOp::Jump(x) => *x = end,
            _ => return None,
        }
        Some(())
    }

    /// Compile a `my` declaration. Answers the kind it left on a bank when
    /// `keep` asked for its value, `None` otherwise.
    ///
    /// Shared with the expression form: `(my int $end = EXPR)` is a
    /// `DoStmt(VarDecl)` in value position, which is how JSON::Fast's
    /// scanners are written almost throughout.
    pub(super) fn compile_var_decl(&mut self, stmt: &Stmt, keep: bool) -> Option<Option<TrKind>> {
        let Stmt::VarDecl {
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
        } = stmt
        else {
            return None;
        };
        if *is_state
            || *is_our
            || *is_dynamic
            || *is_export
            || where_constraint.is_some()
            || name.starts_with('&')
        {
            let n = name.clone();
            self.note_decline(|| format!("declaration shape {n}"));
            return None;
        }
        // `my %result;` / `my @result;` — a FRESH container per invocation,
        // held in a boxed slot under its sigiled name. Only the empty form:
        // an initializer would be a list/hash construction this does not
        // compile.
        if name.starts_with(['@', '%']) {
            let empty = match expr {
                Expr::Hash(entries) if entries.is_empty() => TrOp::NewHash,
                Expr::Literal(v) => match v.view() {
                    crate::value::ValueView::Array(items, _) if items.is_empty() => TrOp::NewArray,
                    _ => {
                        let n = name.clone();
                        self.note_decline(|| format!("initialized container declaration {n}"));
                        return None;
                    }
                },
                _ => {
                    let n = name.clone();
                    self.note_decline(|| format!("initialized container declaration {n}"));
                    return None;
                }
            };
            self.ops.push(empty);
            let slot = self.alloc(name, TrKind::Obj);
            self.store(slot, TrKind::Obj);
            if keep {
                self.load(slot, TrKind::Obj);
                return Some(Some(TrKind::Obj));
            }
            return Some(None);
        }
        // `__has_initializer` is the parser's own marker for `my T $x = ...`;
        // `__scalar_bind` marks `my $x := ...`, which binds rather than
        // assigns — for a fresh `my` in a TRIR frame the two are the same
        // thing, because nothing else can name the slot. Any other trait
        // declines.
        if custom_traits
            .iter()
            .any(|(t, _)| t != "__has_initializer" && t != "__scalar_bind")
        {
            return None;
        }
        let tc = type_constraint.as_deref();
        let kind = match native_kind_of(tc) {
            Some(k) => k,
            None if tc.is_none() || tc == Some("str") => TrKind::Obj,
            None => {
                let t = tc.unwrap_or("").to_string();
                self.note_decline(|| format!("declared type {t}"));
                return None;
            }
        };
        let init = self.compile_expr(expr)?;
        self.coerce(init, kind)?;
        let slot = self.alloc(name, kind);
        self.store(slot, kind);
        if keep {
            // A declaration in value position yields the bound value; re-read
            // it rather than duplicating a bank.
            self.load(slot, kind);
            return Some(Some(kind));
        }
        Some(None)
    }

    /// Insert `op` at `at`, keeping every jump target pointing at the same
    /// instruction.
    ///
    /// Needed by the two branch forms: a `then` arm whose kind differs from
    /// the `else` arm's has to be boxed BEFORE its jump to the join, and by
    /// then the arm is already emitted. Targets are absolute indices, so
    /// everything at or past the insertion point moves by one.
    pub(super) fn insert_op(&mut self, at: usize, op: TrOp) {
        self.ops.insert(at, op);
        let at = at as u32;
        for o in &mut self.ops {
            match o {
                TrOp::Jump(t)
                | TrOp::JumpIfFalseI(t)
                | TrOp::JumpIfTrueI(t)
                | TrOp::JumpIfFalseKeepI(t)
                | TrOp::JumpIfTrueKeepI(t) => {
                    if *t >= at {
                        *t += 1;
                    }
                }
                _ => {}
            }
        }
    }

    /// Reconcile two branch arms that left different kinds, answering the
    /// common kind. `then_end` is where the `then` arm's jump to the join
    /// sits, so a box for that arm goes immediately before it.
    pub(super) fn unify_arms(
        &mut self,
        then_kind: TrKind,
        else_kind: TrKind,
        then_end: usize,
    ) -> Option<TrKind> {
        if then_kind == else_kind {
            return Some(then_kind);
        }
        // Box whichever arm is native. An `int`/`num` pair is not unified:
        // widening one side would change which arithmetic the consumer does.
        match (then_kind, else_kind) {
            (TrKind::Int, TrKind::Obj) => {
                self.insert_op(then_end, TrOp::BoxI);
                Some(TrKind::Obj)
            }
            (TrKind::Num, TrKind::Obj) => {
                self.insert_op(then_end, TrOp::BoxN);
                Some(TrKind::Obj)
            }
            (TrKind::Obj, TrKind::Int) => {
                self.ops.push(TrOp::BoxI);
                Some(TrKind::Obj)
            }
            (TrKind::Obj, TrKind::Num) => {
                self.ops.push(TrOp::BoxN);
                Some(TrKind::Obj)
            }
            _ => {
                self.note_decline(|| {
                    format!("branch arms of different native kinds ({then_kind:?}/{else_kind:?})")
                });
                None
            }
        }
    }

    pub(super) fn drop_top(&mut self, kind: TrKind) {
        match kind {
            TrKind::Int | TrKind::Num => self.ops.push(TrOp::PopI),
            TrKind::Obj => self.ops.push(TrOp::PopObj),
        }
    }

    /// Whether a native slot holds a REFERENCE rather than a value — true
    /// exactly for this routine's own `is rw` native parameters, which are
    /// bound to the caller's slot (ADR-0110 §3.3).
    pub(super) fn slot_is_ref(&self, slot: u16, kind: TrKind) -> bool {
        kind.is_native()
            && self
                .params
                .iter()
                .any(|p| p.is_rw && p.kind.is_native() && p.slot == slot)
    }

    pub(super) fn load(&mut self, slot: u16, kind: TrKind) {
        match kind {
            TrKind::Int | TrKind::Num if self.slot_is_ref(slot, kind) => {
                self.ops.push(TrOp::GetRefI(slot))
            }
            TrKind::Int | TrKind::Num => self.ops.push(TrOp::LoadI(slot)),
            TrKind::Obj => self.ops.push(TrOp::LoadObj(slot)),
        }
    }

    pub(super) fn store(&mut self, slot: u16, kind: TrKind) {
        match kind {
            TrKind::Int | TrKind::Num if self.slot_is_ref(slot, kind) => {
                self.ops.push(TrOp::SetRefI(slot))
            }
            TrKind::Int | TrKind::Num => self.ops.push(TrOp::StoreI(slot)),
            TrKind::Obj => {
                self.obj_written[slot as usize] = true;
                self.ops.push(TrOp::StoreObj(slot));
            }
        }
    }

    /// Emit the conversion from `have` to `want`, or decline when there is
    /// none TRIR performs without the general binder's coercion rules.
    pub(super) fn coerce(&mut self, have: TrKind, want: TrKind) -> Option<()> {
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
            // boxed expression into a native slot declines instead: TRIR
            // never silently coerces where the general binder would raise.
            (TrKind::Obj, TrKind::Int) if self.nqp_sourced => {
                self.ops.push(TrOp::UnboxI);
                Some(())
            }
            (TrKind::Obj, want) => {
                self.note_decline(|| format!("narrowing a boxed value to {want:?}"));
                None
            }
            // Unreachable: the equality guard above covers both, but the
            // exhaustiveness check does not count guarded arms.
            (TrKind::Int, TrKind::Int) | (TrKind::Num, TrKind::Num) => Some(()),
        }
    }

    pub(super) fn binding_of(&self, name: &str) -> Option<Binding> {
        self.locals.get(name).copied()
    }

    /// Whether boxed slot `slot` is ever assigned after its declaration. The
    /// operand-direct string reads memoize the slot's characters for the
    /// frame, which is only sound while nothing rewrites the slot.
    pub(super) fn obj_slot_written(&self, slot: u16) -> bool {
        self.obj_written.get(slot as usize).copied().unwrap_or(true)
    }
}

mod call;
mod expr;
