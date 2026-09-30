//! The routines a lifted BEGIN calls (ADR-0134, slice 2).
//!
//! A BEGIN is lifted to run in the unit prologue, before the scope it is
//! written in has been entered. A routine declared in that scope ahead of the
//! BEGIN does not exist yet. The BEGIN may call it, though, and the routine may
//! close over variables of the scope.
//!
//! So the lifted body runs in blocks that declare the routine again, one block
//! per scope the body reads from, nested as those scopes are. The routine is
//! declared in the block of its own scope, after the copies of the variables it
//! closes over. Those are the same static cells the variables get for the body
//! itself. The routine's own declaration stays where it is, so every frame of
//! its scope still gets its own.
//!
//! The routines to copy are found by scanning the compiled body, then every
//! routine that scan selects, for the names they call and the variables they
//! read. A body that can reach a name dynamically cannot be scanned, so it is
//! not lifted from a scope that declares routines.

use super::{BindingKind, Walker};
use crate::ast::Stmt;
use crate::opcode::CompiledCode;
use crate::value::{Value, ValueView};
use std::collections::{BTreeMap, BTreeSet, HashSet};

/// A routine declared in an inner scope, as a lifted BEGIN re-declares it.
pub(super) struct Routine {
    /// The name a call spells.
    pub(super) name: String,
    /// The declaration, as it reads once the nested BEGINs inside it are lifted.
    pub(super) decl: Stmt,
    /// How many bindings the scope held when the routine was declared. The
    /// routine closes over those and no later ones.
    pub(super) bindings: usize,
}

/// How the lifted body reaches one inner name.
pub(super) enum Access {
    CopyIn(Box<Stmt>),
    Cell,
}

/// What a lifted body needs from the scopes around it: the inner bindings it
/// reads and the routines it calls, each keyed by its frame and its position
/// in that frame.
#[derive(Default)]
pub(super) struct Dependencies {
    pub(super) bindings: BTreeMap<(usize, usize), Access>,
    routines: BTreeSet<(usize, usize)>,
}

/// What the block for one frame holds, in the order it holds it.
#[derive(Default)]
pub(super) struct FrameBlock {
    pub(super) copy_in: Vec<Stmt>,
    routines: Vec<Stmt>,
    pub(super) copy_out: Vec<Stmt>,
}

impl FrameBlock {
    /// Wrap `inner` in one block per frame, the innermost frame closest to it.
    /// A routine then closes over the same declarations it does in place, and
    /// an inner name shadows an outer one as it does there.
    pub(super) fn nest(blocks: BTreeMap<usize, FrameBlock>, mut inner: Vec<Stmt>) -> Vec<Stmt> {
        for (_, frame_block) in blocks.into_iter().rev() {
            let mut block = frame_block.copy_in;
            block.extend(frame_block.routines);
            block.extend(inner);
            block.extend(frame_block.copy_out);
            inner = vec![Stmt::Block(block)];
        }
        inner
    }
}

/// The part of the enclosing scopes a declared routine closes over: the frame
/// it was declared in, and that frame's first `bindings` bindings.
#[derive(Clone, Copy)]
pub(super) struct Scope {
    frame: usize,
    bindings: usize,
}

/// The names a piece of code reads, as the compiler resolves them.
pub(super) struct Scan {
    /// The free variables, read or written.
    free: Vec<crate::symbol::Symbol>,
    /// The routines called by bare name, or read as `&name`.
    callees: HashSet<crate::symbol::Symbol>,
    /// The code can reach a name dynamically (`EVAL`, `CALLER::`, symbolic
    /// lookup), which neither of the sets above can bound.
    reflective: bool,
}

impl Scan {
    pub(super) fn of(stmts: &[Stmt]) -> Scan {
        let (code, fns) = crate::compiler::Compiler::new().compile(stmts);
        let mut scan = Scan {
            free: code.free_var_syms.clone(),
            callees: HashSet::new(),
            reflective: false,
        };
        scan.absorb_code(&code);
        scan.absorb(&fns);
        scan
    }

    /// The free variables the scanned code reads or writes.
    pub(super) fn free(&self) -> &[crate::symbol::Symbol] {
        &self.free
    }

    /// Fold in the routines declared inside the scanned code. Their bodies are
    /// compiled on their own, so the code's own ops do not hold their calls.
    fn absorb(&mut self, fns: &crate::opcode::CompiledFns) {
        for function in fns.values() {
            self.absorb_code(&function.code);
            if let Some(nested) = &function.compiled_fns {
                self.absorb(nested);
            }
        }
    }

    /// The routine names one chunk refers to: a bare call (`f(...)`) and a
    /// code-variable read (`&f`, `&f(...)`, `&f.()`). The compiler lists the
    /// latter among the free variables only when an enclosing scope declares
    /// `&f` as a variable, which a routine is not.
    fn absorb_code(&mut self, code: &CompiledCode) {
        self.callees.extend(code.bare_callee_names());
        self.reflective |= code.needs_reflective_capture;
        self.absorb_ops(code);
    }

    fn absorb_ops(&mut self, code: &CompiledCode) {
        let const_str = |idx: u32| match code.constants.get(idx as usize).map(Value::view) {
            Some(ValueView::Str(s)) => Some(s),
            _ => None,
        };
        for op in &code.ops {
            if let Some(name) = CompiledCode::op_code_var_read_const_idx(op).and_then(const_str) {
                self.callees.insert(crate::symbol::Symbol::intern(&name));
            }
            // `::($name)` finds a routine as readily as a type, and a call
            // qualified by a pseudo-package (`MY::helper()`) names one in a
            // scope the bare-name scan does not look at. Neither is a name the
            // scan can list.
            let indirect = matches!(
                op,
                crate::opcode::OpCode::IndirectTypeLookup
                    | crate::opcode::OpCode::IndirectTypeLookupStore
            );
            let pseudo_call = CompiledCode::op_callee_name_const_idx(op)
                .and_then(const_str)
                .is_some_and(|name| is_pseudo_qualified(crate::symbol::Symbol::intern(&name)));
            self.reflective |= indirect || pseudo_call;
        }
        for nested in &code.closure_compiled_codes {
            self.absorb_ops(nested);
        }
    }
}

/// Whether a called name is qualified by a pseudo-package (`MY::helper`,
/// `OUTER::helper`), which names a routine in a scope the bare-name scan does
/// not look at.
fn is_pseudo_qualified(name: crate::symbol::Symbol) -> bool {
    crate::qualified::package_parent(name)
        .and_then(|pkg| crate::qualified::package_ancestors(pkg).last())
        .is_some_and(|head| crate::runtime::Interpreter::is_pseudo_package_name(head.as_str()))
}

/// The name a call spells for a routine a lifted BEGIN can declare again, or
/// `None` for any other. A routine that installs into a package (`our sub`), a
/// multi, an operator or other category routine (the parser has already
/// registered its syntax, which a scan of the called names cannot see), an
/// exported one and a redeclaring one keep their scope blocked.
fn copyable_routine_name(decl: &Stmt) -> Option<String> {
    let Stmt::SubDecl {
        name,
        name_expr: None,
        multi: false,
        is_export: false,
        supersede: false,
        custom_traits,
        ..
    } = decl
    else {
        return None;
    };
    let name = name.resolve();
    let plain = !name.contains(':') && !custom_traits.iter().any(|(t, _)| t.starts_with("__"));
    plain.then_some(name)
}

impl Walker<'_> {
    /// Note a routine declared in the current scope. A plain one can be
    /// declared again in a lifted BEGIN's block; any other blocks the scope.
    pub(super) fn declare_routine(&mut self, decl: &Stmt) {
        let Some(frame) = self.frames.last_mut() else {
            return;
        };
        match copyable_routine_name(decl) {
            Some(name) => frame.routines.push(Routine {
                name,
                decl: decl.clone(),
                bindings: frame.bindings.len(),
            }),
            None => frame.blocked = true,
        }
    }

    /// The declarations of the routines `deps` selects, grouped by frame.
    pub(super) fn add_routines(
        &self,
        deps: &Dependencies,
        blocks: &mut BTreeMap<usize, FrameBlock>,
    ) {
        for &(frame, routine) in &deps.routines {
            let decl = self.frames[frame].routines[routine].decl.clone();
            blocks.entry(frame).or_default().routines.push(decl);
        }
    }

    /// What the lifted `body` needs from the scopes around it: how it reaches
    /// each inner name it reads, and which declared routines it calls, together
    /// with whatever those routines read and call in turn. `None` when one of
    /// them cannot be supplied in the prologue.
    pub(super) fn resolve_dependencies(&self, body: &[Stmt]) -> Option<Dependencies> {
        let mut deps = Dependencies::default();
        // A name reached by `EVAL` or a symbolic lookup could be any routine in
        // scope, so a body that does that cannot say which ones it needs.
        let routines_in_scope = self.frames.iter().any(|f| !f.routines.is_empty());
        // The body sees every scope; a routine sees the ones it was declared in,
        // up to its own position.
        let mut pending = vec![(Scan::of(body), None)];
        while let Some((scan, scope)) = pending.pop() {
            if scan.reflective && routines_in_scope {
                return None;
            }
            for sym in &scan.free {
                self.resolve_free_name(*sym, scope, &mut deps, &mut pending)?;
            }
            for callee in &scan.callees {
                self.resolve_callee(&callee.resolve(), scope, &mut deps, &mut pending);
            }
        }
        Some(deps)
    }

    fn resolve_free_name(
        &self,
        sym: crate::symbol::Symbol,
        scope: Option<Scope>,
        deps: &mut Dependencies,
        pending: &mut Vec<(Scan, Option<Scope>)>,
    ) -> Option<()> {
        if crate::qualified::is_qualified(sym) {
            return Some(());
        }
        let name = sym.resolve();
        if CompiledCode::is_non_lexical_name(&name) {
            return Some(());
        }
        // `&helper` reads the routine `helper` declared ahead, if one is.
        if let Some(routine) = name.strip_prefix('&')
            && self.resolve_callee(routine, scope, deps, pending)
        {
            return Some(());
        }
        match self.find_binding(&name, scope) {
            Some((frame, binding)) => {
                let access = match &self.frames[frame].bindings[binding].kind {
                    BindingKind::Param => Access::CopyIn(Box::new(super::unbound_decl(&name))),
                    BindingKind::Our(decl) => Access::CopyIn(decl.clone()),
                    BindingKind::Local { .. } => Access::Cell,
                    BindingKind::Opaque => return None,
                };
                deps.bindings.entry((frame, binding)).or_insert(access);
            }
            None => {
                if !self.unit_names.contains(&name) && crate::env::is_plain_user_lexical(&name) {
                    return None;
                }
            }
        }
        Some(())
    }

    /// If `name` is a routine declared ahead, in the scopes `scope` sees,
    /// select it and queue what it reads. Returns whether it was one.
    fn resolve_callee(
        &self,
        name: &str,
        scope: Option<Scope>,
        deps: &mut Dependencies,
        pending: &mut Vec<(Scan, Option<Scope>)>,
    ) -> bool {
        let Some((frame, index)) = self.find_routine(name, scope) else {
            return false;
        };
        if deps.routines.insert((frame, index)) {
            let routine = &self.frames[frame].routines[index];
            let scope = Scope {
                frame,
                bindings: routine.bindings,
            };
            pending.push((Scan::of(std::slice::from_ref(&routine.decl)), Some(scope)));
        }
        true
    }

    /// The innermost binding of `name`. Given a `scope`, only what a routine
    /// declared there closes over counts.
    pub(super) fn find_binding(&self, name: &str, scope: Option<Scope>) -> Option<(usize, usize)> {
        let frames = scope.map_or(self.frames.len(), |s| s.frame + 1);
        (0..frames).rev().find_map(|f| {
            let bindings = &self.frames[f].bindings;
            let visible = match scope {
                Some(s) if s.frame == f => &bindings[..s.bindings],
                _ => &bindings[..],
            };
            visible.iter().rposition(|b| b.name == name).map(|b| (f, b))
        })
    }

    /// The innermost routine called `name`, among the scopes `scope` sees.
    fn find_routine(&self, name: &str, scope: Option<Scope>) -> Option<(usize, usize)> {
        let frames = scope.map_or(self.frames.len(), |s| s.frame + 1);
        (0..frames).rev().find_map(|f| {
            self.frames[f]
                .routines
                .iter()
                .rposition(|r| r.name == name)
                .map(|r| (f, r))
        })
    }
}
