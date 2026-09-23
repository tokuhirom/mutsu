//! AST -> [`TrChunk`]: the resolution pass of ADR-0110 §3.
//!
//! Conservative and all-or-nothing (ADR-0110 §4): every construct this
//! compiler does not *prove* it can execute with static operand kinds makes
//! the whole routine decline, and a declined routine takes today's untyped
//! path unchanged. There is therefore no fallback arm anywhere below — the
//! only two outcomes are a complete chunk and `None`.

use std::collections::{HashMap, HashSet};

use super::{TrChunk, TrKind, TrOp, TrOuter, TrParam, TrParamCheck};
use crate::ast::{ParamDef, Stmt};
use crate::symbol::Symbol;
use crate::value::Value;

/// Where a name resolves inside the routine being compiled.
#[derive(Debug, Clone, Copy)]
pub(super) struct Binding {
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
    /// The routine declares `--> Nil`, so every `return` discards its value.
    pub(super) returns_nil: bool,
    /// The routine declares a definite return value (`--> True`): the body
    /// runs for its effects and the routine answers this constant.
    pub(super) definite_return: Option<Value>,
    /// Boxed slots bound (`:=`) to an `nqp::` op's result and never written
    /// again. A read of one is `nqp`-sourced, so `$end + 1` on
    /// `my $end := nqp::index(...)` narrows exactly as `nqp::index(...) + 1`
    /// does.
    pub(super) nqp_bound: HashSet<u16>,
    /// Native slots declared with a sized type (`uint32`, `int8`, ...):
    /// `(bits, signed)`. Every store wraps to the width.
    pub(super) sized: HashMap<u16, (u8, bool, &'static str)>,
    /// The call-only inner `my sub`s of this body (ADR-0113's frame
    /// lexicals), inlined at their call sites. `None` until the declaration
    /// statement has been compiled; see `inline.rs`.
    pub(super) inline_subs: HashMap<String, Option<inline::InlineSub>>,
    /// The inner subs being inlined right now, innermost last.
    pub(super) inline_stack: Vec<String>,
    /// The method calls this body makes, indexed by `MethodGen`.
    pub(super) methods: Vec<crate::trir::TrMethodCall>,
    /// Addresses of the routine body's own top-level statements: only a
    /// declaration there is a frame lexical.
    top_level: HashSet<usize>,
}

/// A statement's variant name, for a decline report.
fn discriminant_of(s: &Stmt) -> String {
    let rendered = format!("{s:?}");
    rendered
        .split([' ', '(', '{'])
        .next()
        .unwrap_or("?")
        .to_string()
}

/// `(routine name, positional arity)` -> its `CompiledFns` key and body
/// fingerprint.
pub(crate) type TrirRoutineMap = HashMap<(String, usize), (Symbol, u64)>;

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
        frame_lexicals: &[Symbol],
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
        // A definite VALUE (`--> True`) needs none: the body's value is
        // discarded and the constant answered, which is what #9074 does on
        // the light call path.
        let definite_return = match return_type {
            None | Some("Nil") => None,
            Some("True") => Some(Value::TRUE),
            Some("False") => Some(Value::FALSE),
            Some(_) => return None,
        };
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
            routines,
            fns,
            why: None,
            nqp_sourced: false,
            returns_nil: return_type == Some("Nil") || definite_return.is_some(),
            definite_return,
            nqp_bound: HashSet::new(),
            sized: HashMap::new(),
            inline_subs: HashMap::new(),
            inline_stack: Vec::new(),
            methods: Vec::new(),
            top_level: body.iter().map(|s| s as *const Stmt as usize).collect(),
        };
        c.collect_inline_subs(body, frame_lexicals);
        if c.declare_params(param_defs).is_none() {
            return c.declined(name);
        }
        let returns_nil = c.returns_nil;
        let Some(last) = c.compile_body(body, returns_nil) else {
            return c.declined(name);
        };
        // The routine's value: `--> Nil` discards it, otherwise the last
        // statement's value is the result.
        match (returns_nil, last) {
            (true, _) if c.definite_return.is_some() => c.push_definite_return(),
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
            methods: c.methods,
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
                    if self.slot_is_readonly_param(slot, kind) {
                        self.note_decline(|| {
                            format!("assignment to the read-only parameter {name}")
                        });
                        return None;
                    }
                    self.check_not_bound(slot, kind, name)?;
                    let got = self.compile_expr(expr)?;
                    let tn = self.native_type_name(slot);
                    self.coerce_store(got, kind, tn)?;
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
                    // Only in sink position. A trailing `if`/`elsif`/`else`
                    // IS the routine's value in Raku, and compiling its
                    // branches for effect and then returning `Nil` is a wrong
                    // answer, not a missing optimization — `sub sel($a, $b) {
                    // if $a && $b { "both" } elsif ... }` answered `Nil` for
                    // every call after the first (the first runs untyped,
                    // before the call site is linked to TRIR).
                    if !sink_all && Some(i) == final_idx {
                        self.note_decline(|| {
                            "a trailing `if` statement, whose value is the routine's".to_string()
                        });
                        return None;
                    }
                    self.compile_if(cond, then_branch, else_branch)?;
                }
                // `die EXPR` is a call, and compiling it as one is what lets a
                // routine whose hot loop is typed keep its cold error path
                // instead of declining for it.
                Stmt::Die(e) => {
                    self.compile_routine_call("die", std::slice::from_ref(e))?;
                    self.ops.push(TrOp::PopObj);
                }
                // A call-only inner `my sub` (ADR-0113): inlined at its call
                // sites, so its declaration emits nothing.
                Stmt::SubDecl { name, .. }
                    if self.top_level.contains(&(stmt as *const Stmt as usize))
                        && self.inline_subs.contains_key(name.as_str()) =>
                {
                    self.declare_inline_sub(stmt)?;
                }
                Stmt::Return(e) => {
                    self.compile_return(std::slice::from_ref(e))?;
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
                | TrOp::JumpIfTrueKeepI(t)
                    if *t >= at =>
                {
                    *t += 1;
                }
                _ => {}
            }
        }
    }

    /// Reconcile two branch arms that left different kinds, answering the
    /// common kind and how many ops were inserted AT OR BEFORE `then_end` —
    /// which every index the caller recorded from `then_end` onwards has to
    /// be shifted by. `then_end` is where the `then` arm's jump to the join
    /// sits, so a box for that arm goes immediately before it; a box for the
    /// `else` arm goes on the end and shifts nothing, and reading the shift
    /// off `then_kind != else_kind` got that second case wrong — the caller
    /// then found something other than its own `Jump` under the shifted index
    /// and declined the whole routine. `JSON::Fast`'s scanners are exactly
    /// that shape (`nqp::if(cond, die-helper(...), $pos)`: a boxed `then`
    /// against a native `else`), so the slip cost the routine that motivated
    /// the stage.
    pub(super) fn unify_arms(
        &mut self,
        then_kind: TrKind,
        else_kind: TrKind,
        then_end: usize,
    ) -> Option<(TrKind, usize)> {
        if then_kind == else_kind {
            return Some((then_kind, 0));
        }
        // Box whichever arm is native. An `int`/`num` pair is not unified:
        // widening one side would change which arithmetic the consumer does.
        match (then_kind, else_kind) {
            (TrKind::Int, TrKind::Obj) => {
                self.insert_op(then_end, TrOp::BoxI);
                Some((TrKind::Obj, 1))
            }
            (TrKind::Num, TrKind::Obj) => {
                self.insert_op(then_end, TrOp::BoxN);
                Some((TrKind::Obj, 1))
            }
            (TrKind::Obj, TrKind::Int) => {
                self.ops.push(TrOp::BoxI);
                Some((TrKind::Obj, 0))
            }
            (TrKind::Obj, TrKind::Num) => {
                self.ops.push(TrOp::BoxN);
                Some((TrKind::Obj, 0))
            }
            _ => {
                self.note_decline(|| {
                    format!("branch arms of different native kinds ({then_kind:?}/{else_kind:?})")
                });
                None
            }
        }
    }
}

mod binary;
mod call;
mod expr;
mod inline;
mod method;
mod nqp;
mod params;
mod ret;
mod slots;

/// Compile a routine to TRIR at declaration time, or answer `None`.
///
/// One call site (`compiler/helpers_sub_body.rs`), so the eligibility gate
/// and the chunk can never disagree about which routines have one.
pub(crate) fn compile_routine(
    name: Symbol,
    param_defs: &[crate::ast::ParamDef],
    params: &[String],
    return_type: Option<&str>,
    body: &[crate::ast::Stmt],
    routines: Option<&TrirRoutineMap>,
    fns: Option<&crate::opcode::CompiledFns>,
    frame_lexicals: &[Symbol],
) -> Option<std::sync::Arc<TrChunk>> {
    let chunk = TrirCompiler::compile(
        name,
        param_defs,
        params,
        return_type,
        body,
        routines,
        fns,
        frame_lexicals,
    );
    if dump_enabled() {
        match &chunk {
            Some(c) => eprintln!(
                "trir: {} accepted ({} ops, {} native slots, {} obj slots, {} outers, {} calls)",
                name.as_str(),
                c.ops.len(),
                c.n_native,
                c.n_obj,
                c.outers.len(),
                c.calls.len(),
            ),
            None => eprintln!("trir: {} declined", name.as_str()),
        }
    }
    chunk.map(std::sync::Arc::new)
}

/// Whether `MUTSU_TRIR_DUMP` asked for the eligibility decisions to be
/// reported. Read once: this runs per routine declaration.
fn dump_enabled() -> bool {
    use std::sync::OnceLock;
    static ON: OnceLock<bool> = OnceLock::new();
    *ON.get_or_init(|| std::env::var("MUTSU_TRIR_DUMP").is_ok())
}
