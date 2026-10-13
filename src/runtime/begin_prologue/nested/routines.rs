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
//! read. A call to a multi brings every candidate and proto it can reach. An
//! operator or other category routine is used through its syntax, which the
//! scan need not see, so every one of the scopes is copied. A body that can
//! reach a name dynamically (`EVAL`, a symbolic lookup) gets every variable and
//! routine of the scopes it sees. One that calls a routine it does not know is
//! not lifted from a scope that declares routines, since that routine may
//! evaluate a string where it was called from.

use super::routine_copy::{copyable_routine, copyable_token};
use super::scan::{Scan, is_core_routine, is_pseudo_qualified};
use super::token_reads::TokenReads;
use super::{BindingKind, Walker};
use crate::ast::Stmt;
use crate::opcode::CompiledCode;
use std::collections::{BTreeMap, BTreeSet};

/// A routine declared in an inner scope, as a lifted BEGIN re-declares it.
pub(super) struct Routine {
    /// The name a call spells.
    pub(super) name: String,
    /// A candidate of a multi, or its proto: a call reaches every one of them
    /// in scope, not just the innermost.
    pub(super) multi: bool,
    /// The declaration, as it reads once the nested BEGINs inside it are lifted.
    pub(super) decl: Stmt,
    /// How many bindings the scope held when the routine was declared. The
    /// routine closes over those and no later ones.
    pub(super) bindings: usize,
    /// Copied whether or not the body seems to use it: an operator or other
    /// category routine, or a token, which code reaches through syntax.
    pub(super) always: bool,
    /// For a token, what its pattern reads, which the scan of its
    /// declaration does not see.
    pub(super) reads: Option<TokenReads>,
}

impl Routine {
    /// What the routine reads and calls, as declared in its scope.
    // Cost: O(n), n = the size of the declaration.
    fn scan(&self) -> Scan {
        match &self.reads {
            Some(reads) => reads.scan(),
            None => Scan::of(std::slice::from_ref(&self.decl)),
        }
    }
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
    pub(super) routines: BTreeSet<(usize, usize)>,
}

/// What the block for one frame holds, in the order it holds it.
#[derive(Default)]
pub(super) struct FrameBlock {
    /// The scope's imports, repeated ([`super::decls`]).
    pub(super) imports: Vec<Stmt>,
    /// The scope's types the body names, repeated ([`super::decls`]).
    pub(super) types: Vec<Stmt>,
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
            let mut block = frame_block.imports;
            block.extend(frame_block.types);
            block.extend(frame_block.copy_in);
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

impl Walker<'_> {
    /// Note a routine declared in the current scope. A plain one can be
    /// declared again in a lifted BEGIN's block; any other blocks the scope.
    pub(super) fn declare_routine(&mut self, decl: &Stmt) {
        let Some(frame) = self.frames.last_mut() else {
            return;
        };
        match copyable_routine(decl) {
            Some((name, multi, decl)) => frame.routines.push(Routine {
                always: name.contains(':'),
                name,
                multi,
                decl,
                bindings: frame.bindings.len(),
                reads: None,
            }),
            None => frame.blocked = true,
        }
    }

    /// Note a `token`, `rule` or `regex` declared in the current scope. A
    /// plain one is declared again in every lifted BEGIN's block; any other
    /// blocks the scope ([`copyable_token`]).
    pub(super) fn declare_token(&mut self, decl: &Stmt) {
        let Some(frame) = self.frames.last_mut() else {
            return;
        };
        match copyable_token(decl) {
            Some((name, decl, reads)) => frame.routines.push(Routine {
                name,
                multi: false,
                decl,
                bindings: frame.bindings.len(),
                always: true,
                reads: Some(reads),
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
        // A name reached by `EVAL` or a symbolic lookup could be any name in
        // scope, so a body that does that gets every one it can see. A body
        // that calls a routine it does not know cannot say which ones it
        // needs: that routine may evaluate a string it is given where it was
        // called from.
        // A type an inner scope declares is only supplied when the body names it
        // ([`super::decls`]), so it is subject to the same rule.
        let routines_in_scope = self
            .frames
            .iter()
            .any(|f| !f.routines.is_empty() || !f.types.is_empty());
        // The body sees every scope; a routine sees the ones it was declared in,
        // up to its own position.
        let mut pending = vec![(Scan::of(body), None)];
        self.add_operator_code_vars(&mut deps)?;
        self.add_category_routines(&mut deps, &mut pending);
        while let Some((scan, scope)) = pending.pop() {
            if scan.reflective {
                self.add_whole_scope(scope, &mut deps, &mut pending)?;
            }
            for sym in &scan.free {
                self.resolve_free_name(*sym, scope, &mut deps, &mut pending)?;
            }
            // A multi read as `&name` is its proto object, which the code may
            // change (`&p.add_dispatchee(...)`); a change to the copy would be
            // lost, as for a type ([`super::decls`]).
            if scan.code_reads.iter().any(|name| self.reads_multi(&name.resolve(), scope)) {
                return None;
            }
            for callee in &scan.callees {
                let name = callee.resolve();
                // `g()` calls a code variable `my &g` when that is the innermost
                // `&g`.
                if self.code_var_shadows_routine(&name, scope) {
                    let var = crate::symbol::Symbol::intern(&format!("&{name}"));
                    self.resolve_free_name(var, scope, &mut deps, &mut pending)?;
                    continue;
                }
                let selected = self.resolve_callee(&name, scope, &mut deps, &mut pending)
                    || self.calls_into_inner_type(&name);
                // `EVAL` reaches the scope by name, which the whole scope
                // supplied above covers.
                let evaluates = scan.reflective && matches!(name.as_str(), "EVAL" | "EVALFILE");
                if !selected
                    && routines_in_scope
                    && !is_core_routine(*callee)
                    && !evaluates
                    && !is_pseudo_qualified(*callee)
                {
                    return None;
                }
            }
        }
        Some(deps)
    }

    /// Supply every name `scope` sees (the whole of the enclosing inner scopes,
    /// for the body itself): each variable and each routine, the innermost of
    /// a name only, as a lookup by that name finds it. A body that reaches
    /// names dynamically (`EVAL`, `::($name)`, `&::($name)`) may reach any of
    /// them. `None` when one cannot be supplied in the prologue.
    // Cost: O(b + r), b = the bindings and r = the routines of the enclosing
    // inner scopes.
    fn add_whole_scope(
        &self,
        scope: Option<Scope>,
        deps: &mut Dependencies,
        pending: &mut Vec<(Scan, Option<Scope>)>,
    ) -> Option<()> {
        let frames = scope.map_or(self.frames.len(), |s| s.frame + 1);
        let mut variables = BTreeSet::new();
        let mut routines = BTreeSet::new();
        for (f, frame) in self.frames[..frames].iter().enumerate() {
            let visible = match scope {
                Some(s) if s.frame == f => &frame.bindings[..s.bindings],
                _ => &frame.bindings[..],
            };
            variables.extend(visible.iter().map(|b| b.name.as_str()));
            routines.extend(frame.routines.iter().map(|r| r.name.as_str()));
        }
        for name in variables {
            let sym = crate::symbol::Symbol::intern(name);
            self.resolve_free_name(sym, scope, deps, pending)?;
        }
        for name in routines {
            if !self.code_var_shadows_routine(name, scope) {
                self.resolve_callee(name, scope, deps, pending);
            }
        }
        Some(())
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
            && !self.code_var_shadows_routine(routine, scope)
            && self.resolve_callee(routine, scope, deps, pending)
        {
            return Some(());
        }
        match self.find_binding(&name, scope) {
            Some((frame, binding)) => {
                // A package body's `my` variable is reached through the
                // package, which a lifted phaser re-enters.
                if matches!(
                    self.frames[frame].bindings[binding].kind,
                    BindingKind::PackageLexical
                ) {
                    return Some(());
                }
                let access = self.access_of(frame, binding)?;
                deps.bindings.entry((frame, binding)).or_insert(access);
            }
            None => {
                // A name nothing in the unit declares is refused: in an EVAL it
                // may be a lexical of the EVAL's caller, which the prologue
                // cannot supply. Outside an EVAL, with `strict` off where the
                // BEGIN sits, it can only be an auto-declared package variable,
                // which the lifted block (repeating `no strict`) declares the
                // same way: a package variable, which outlives that block
                // (#10622).
                if !self.unit_names.contains(&name)
                    && crate::env::is_plain_user_lexical(&name)
                    && !(scope.is_none() && !self.unit.is_eval && self.strict_is_off())
                {
                    return None;
                }
            }
        }
        Some(())
    }

    /// Whether `strict` is off where the current statement sits: the last
    /// `use strict` / `no strict` ahead of it in the scopes around it, and
    /// then in the unit's top level, is `no strict`.
    // Cost: O(i), i = the pragmas and imports in the enclosing scopes.
    fn strict_is_off(&self) -> bool {
        for stmt in self
            .frames
            .iter()
            .rev()
            .flat_map(|f| f.imports.iter().rev())
        {
            if let Some(off) = super::super::strict_pragma(stmt) {
                return off;
            }
        }
        self.unit.strict_off
    }

    /// How the lifted body reaches an inner binding, or `None` when it cannot.
    fn access_of(&self, frame: usize, binding: usize) -> Option<Access> {
        let binding = &self.frames[frame].bindings[binding];
        Some(match &binding.kind {
            BindingKind::Param => {
                Access::CopyIn(Box::new(super::cell_ast::unbound_decl(&binding.name)))
            }
            BindingKind::Our(decl) => Access::CopyIn(decl.clone()),
            BindingKind::Local { .. } => Access::Cell,
            BindingKind::Opaque | BindingKind::PackageLexical => return None,
        })
    }

    /// Supply every operator code variable (`my &infix:<x>`) of the scopes
    /// around the body. The code that uses one does so through its operator's
    /// syntax (`1 x 2`), which the parser registered for the rest of the scope,
    /// or by symbolic lookup (`&::("infix:<x>")`), and neither has to name the
    /// variable where a scan of the compiled code would see it. Each one is
    /// supplied in its own scope's block, so a copied routine finds the one it
    /// closes over.
    // Cost: O(b), b = the bindings of the enclosing inner scopes.
    fn add_operator_code_vars(&self, deps: &mut Dependencies) -> Option<()> {
        for (f, frame) in self.frames.iter().enumerate() {
            for (b, binding) in frame.bindings.iter().enumerate() {
                if binding.name.starts_with('&') && binding.name.contains(':') {
                    let access = self.access_of(f, b)?;
                    deps.bindings.entry((f, b)).or_insert(access);
                }
            }
        }
        Some(())
    }

    /// Supply every operator or other category routine (`sub infix:<plus>`)
    /// of the scopes around the body, for the reason
    /// [`Walker::add_operator_code_vars`] gives for an operator code variable.
    // Cost: O(r), r = the routines of the enclosing inner scopes.
    fn add_category_routines(
        &self,
        deps: &mut Dependencies,
        pending: &mut Vec<(Scan, Option<Scope>)>,
    ) {
        for (frame, f) in self.frames.iter().enumerate() {
            for (index, routine) in f.routines.iter().enumerate() {
                if routine.always && deps.routines.insert((frame, index)) {
                    let scope = Scope {
                        frame,
                        bindings: routine.bindings,
                    };
                    pending.push((routine.scan(), Some(scope)));
                }
            }
        }
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
        let selected = self.find_dispatchees(name, scope);
        for &(frame, index) in &selected {
            if deps.routines.insert((frame, index)) {
                let routine = &self.frames[frame].routines[index];
                let scope = Scope {
                    frame,
                    bindings: routine.bindings,
                };
                pending.push((routine.scan(), Some(scope)));
            }
        }
        !selected.is_empty()
    }

    /// Whether `&name`, among the scopes `scope` sees, is a multi of them.
    // Cost: O(r), r = the routines declared in the enclosing inner scopes.
    fn reads_multi(&self, name: &str, scope: Option<Scope>) -> bool {
        self.find_dispatchees(name, scope)
            .iter()
            .any(|&(f, r)| self.frames[f].routines[r].multi)
    }

    /// The routines a call to `name` can reach, among the scopes `scope` sees:
    /// the innermost one, or, when that is a multi candidate or proto, every
    /// candidate and proto of the name out to the first scope whose own
    /// declaration of it is not a multi (that one shadows everything further
    /// out). Scopes outside every inner one are the unit's, which the prologue
    /// declares on its own.
    // Cost: O(r), r = the routines declared in the enclosing inner scopes.
    fn find_dispatchees(&self, name: &str, scope: Option<Scope>) -> Vec<(usize, usize)> {
        let frames = scope.map_or(self.frames.len(), |s| s.frame + 1);
        let mut found = Vec::new();
        for f in (0..frames).rev() {
            let routines = &self.frames[f].routines;
            let named = || routines.iter().enumerate().filter(|(_, r)| r.name == name);
            if let Some((r, _)) = named().rfind(|(_, r)| !r.multi) {
                // A plain routine shadows everything further out; under inner
                // candidates, it is shadowed in turn.
                if found.is_empty() {
                    found.push((f, r));
                }
                return found;
            }
            found.extend(named().map(|(r, _)| (f, r)));
        }
        found
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

    /// Whether the innermost `&name` that `scope` sees is an inner code
    /// variable (`my &name = ...`) rather than a routine declared ahead. A
    /// routine and a code variable of one name in the same scope would be a
    /// redeclaration.
    fn code_var_shadows_routine(&self, name: &str, scope: Option<Scope>) -> bool {
        let Some((var_frame, _)) = self.find_binding(&format!("&{name}"), scope) else {
            return false;
        };
        self.find_routine(name, scope)
            .is_none_or(|(routine_frame, _)| var_frame >= routine_frame)
    }

    /// The declarations of the routines `deps` selects.
    pub(super) fn routine_decls<'a>(
        &'a self,
        deps: &'a Dependencies,
    ) -> impl Iterator<Item = &'a Stmt> + 'a {
        deps.routines
            .iter()
            .map(|&(frame, routine)| &self.frames[frame].routines[routine].decl)
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
