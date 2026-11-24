//! The AST walk of the undeclared-routine check: which names the unit
//! declares, and which bareword calls it makes.
//!
//! Declarations are collected scope-blind, except a routine (`sub`, `proto`)
//! declared inside a statement list other than the unit's own (a `sub`/
//! `method`/`proto` body, a bare or control-flow block, a package body):
//! rakudo installs it in that list's lexical scope (an `our` one additionally
//! as a package symbol, which only a qualified call reaches), so a bare call
//! outside the list is `Undeclared routine` (#13111, #15861). Such a routine
//! is recorded in the list's [`Frame`], and a call is explained by it only
//! when the call sits in that scope — before or after the declaration, since
//! the check runs at CHECK time. An `is export` routine stays unit-wide: an
//! import makes it reachable from outside its scope.
//! Types and variables stay scope-blind: a `class K` in a routine body is
//! `our`-scoped and reachable from outside.
//!
//! A *term* — a sigilless variable (`my \x`), a `constant`, an enum key — is
//! visible from its declaration on, in the statement list that declares it and
//! the lists nested in it (ADR-15760 Amendment 1, slice 1b). Rakudo hoists
//! routines only, so `say x; my \x = 1` is `Undeclared routine` there. A scan
//! made in *term order* (`Scan::term_order`, over a unit still in source
//! order) excuses a call by a term only if the term's list is still open and
//! the term was declared before the call. The ordinary scan runs on the
//! phaser-reordered tree, where declarations have been hoisted and the order
//! is gone, so it takes every term as declared everywhere.

use crate::ast::{CallArg, Expr, ParamDef, Stmt};
use crate::ast_visit::{
    NameKind, Visit, walk_call_arg, walk_expr, walk_param, walk_regex_node, walk_stmt, walk_stmts,
};
use crate::regex_tree::RegexNode;
use std::collections::HashSet;

use super::conditional;
use super::package_frames::PackageFrames;

mod visit;

/// The lexical scope of one statement list below the unit's top level.
struct Frame {
    /// The enclosing scope, if any.
    parent: Option<usize>,
    /// The routines declared in this scope (outside any nested scope).
    routines: HashSet<String>,
    /// A `use` in this scope imports names the walk cannot see: a call in it
    /// (or in a scope nested in it) is not judged. The import is lexical, so
    /// a call outside the scope still is (#13441).
    opaque_imports: bool,
}

#[derive(Default)]
pub(super) struct Scan {
    pub(super) declared: HashSet<String>,
    /// The subset of `declared` that is a *routine* declaration (`sub`, `multi
    /// sub`). `declared` deliberately also absorbs variables and types, because
    /// suppressing a call on any of them is the safe direction — but a
    /// suggestion drawn from it would propose a variable as the routine the
    /// user meant, which rakudo never does. Suggestions read this instead.
    pub(super) declared_routines: HashSet<String>,
    /// The routines declared inside some routine body, which `declared` does
    /// not hold (they are in their [`Frame`]).
    pub(super) body_routines: HashSet<String>,
    /// The calls in source order. Until [`Scan::resolve_body_scoped_calls`]
    /// has run, a call explained by an enclosing body's routine is still here.
    pub(super) calls: Vec<(String, i64)>,
    /// The innermost routine body of each entry of `calls`.
    call_frames: Vec<Option<usize>>,
    frames: Vec<Frame>,
    /// The routine body the walk is in.
    current: Option<usize>,
    /// The name of the `proto` whose declaration is being walked: the visitor
    /// reports it as a plain `Decl`, but it declares a routine.
    proto_name: Option<String>,
    /// Judge terms by their position (see the module docs).
    pub(super) term_order: bool,
    /// The terms in scope at the walk's position: one set per open statement
    /// list, outermost first (never empty).
    terms: Vec<HashSet<String>>,
    /// Every term the unit declares, wherever: `declared`'s share that is
    /// order-aware for the call check but scope-blind for the `EVAL`
    /// undeclared-name check.
    pub(super) all_terms: HashSet<String>,
    /// The keys of the enum being walked, which the visitor reports as plain
    /// `Decl`s after [`Scan::declare_term`] has taken them.
    enum_keys: Vec<String>,
    line: i64,
    /// The unit imports names the walker cannot see (use/require/...): skip
    /// the whole check rather than risk a false positive.
    pub(super) bail: bool,
    /// The `EVAL` flavour of the check (`ScanMode::Eval`).
    pub(super) eval: bool,
    /// The unit's top-level conditional `use`s (see [`conditional`]).
    pub(super) conditional_uses: Vec<conditional::ConditionalUse>,
    /// The `&name` references, with their lines. One is judged as a call only
    /// if no routine of that name is declared anywhere in the unit
    /// ([`Scan::adopt_code_vars`]): a `&name` of a package's or a routine
    /// body's routine is reachable in more ways (an `import`, an exported
    /// symbol, a closure handed out) than a call's scope rules model.
    code_vars: Vec<(String, i64)>,
    /// The frames of the package bodies walked so far.
    package_frames: PackageFrames,
}

/// A declared name with its sigil and twigil removed.
fn bare_name(name: &str) -> &str {
    let bare = name.strip_prefix('\\').unwrap_or(name);
    bare.strip_prefix(['$', '@', '%', '&'])
        .unwrap_or(bare)
        .trim_start_matches(['!', '.', '*', '^', ':'])
}

impl Scan {
    pub(super) fn new(eval: bool, term_order: bool) -> Self {
        Scan {
            line: 1,
            term_order,
            terms: vec![HashSet::new()],
            eval,
            ..Default::default()
        }
    }

    // Cost: O(n), n = size of the walked AST.
    pub(super) fn walk_unit(&mut self, stmts: &[Stmt]) {
        walk_stmts(self, stmts);
    }

    // Cost: O(n), n = size of the statement's AST.
    pub(super) fn walk_top_level(&mut self, stmt: &Stmt) {
        self.visit_stmt(stmt);
    }

    /// Judge each `&name` reference whose routine the unit declares nowhere,
    /// at any scope, as a call (`say &nosuch` is "Undeclared routine").
    // Cost: O(v + f * r), v = `&name` references, f = frames, r = routines each.
    pub(super) fn adopt_code_vars(&mut self) {
        let code_vars = std::mem::take(&mut self.code_vars);
        // A `&name` carries no scope here: with a `use` below the top level,
        // one inside its scope may name what it imported (#13441).
        if self.frames.iter().any(|f| f.opaque_imports) {
            return;
        }
        let saved_line = self.line;
        for (name, line) in code_vars {
            let declared_somewhere = self.declared.contains(&name)
                || self.body_routines.contains(&name)
                || self.frames.iter().any(|f| f.routines.contains(&name));
            if !declared_somewhere {
                self.line = line;
                self.record_call(&name);
            }
        }
        self.line = saved_line;
    }

    /// Drop every call that a routine declared in an enclosing routine body
    /// explains.
    // Cost: O(c * d), c = calls, d = routine-body nesting depth.
    pub(super) fn resolve_body_scoped_calls(&mut self) {
        let frames = &self.frames;
        let mut call_frames = std::mem::take(&mut self.call_frames).into_iter();
        self.calls.retain(|(name, _)| {
            let mut frame = call_frames.next().flatten();
            while let Some(i) = frame {
                if frames[i].routines.contains(name) || frames[i].opaque_imports {
                    return false;
                }
                frame = frames[i].parent;
            }
            true
        });
    }

    /// Walk a routine declaration whose name the caller has declared: its
    /// signature and body are the new routine body's scope.
    fn walk_routine_body(&mut self, stmt: &Stmt) {
        let parent = self.current;
        self.frames.push(Frame {
            parent,
            routines: HashSet::new(),
            opaque_imports: false,
        });
        self.current = Some(self.frames.len() - 1);
        walk_stmt(self, stmt);
        self.current = parent;
    }

    /// Record a routine declaration: both a suppressor (like any other name)
    /// and a suggestion candidate — or, inside a routine body, a suppressor
    /// for the calls in that body only.
    fn declare_routine(&mut self, name: &str) {
        self.declare_routine_in(name, self.current);
    }

    /// Record a routine that an import makes reachable outside its scope.
    fn declare_unit_wide_routine(&mut self, name: &str) {
        self.declare_routine_in(name, None);
    }

    fn declare_routine_in(&mut self, name: &str, scope: Option<usize>) {
        let bare = bare_name(name);
        if bare.is_empty() {
            return;
        }
        // The callable itself is a routine, but its bare `term:<x>` spelling
        // becomes a term only from this declaration onward. A later term
        // routine must not excuse an earlier bare `x` during the ordered scan.
        let term = term_name(bare);
        if let Some(inner) = term {
            self.all_terms.insert(inner.to_string());
            if self.term_order && let Some(open) = self.terms.last_mut() {
                open.insert(inner.to_string());
            }
        }
        if let Some(i) = scope {
            let routines = &mut self.frames[i].routines;
            routines.insert(bare.to_string());
            // The unordered scan still resolves the term through its lexical
            // frame; the ordered scan uses the term stack above instead.
            if !self.term_order && let Some(inner) = term {
                routines.insert(inner.to_string());
            }
            self.body_routines.extend(routines.iter().cloned());
            return;
        }
        if self.term_order && term.is_some() {
            self.declared.insert(bare.to_string());
        } else {
            self.declare(name);
        }
        self.declared_routines.insert(bare.to_string());
    }

    /// Record a term declaration: visible to the calls after it in the
    /// current statement list and the lists nested in it.
    fn declare_term(&mut self, name: &str) {
        let bare = bare_name(name);
        if bare.is_empty() {
            return;
        }
        self.all_terms.insert(bare.to_string());
        if !self.term_order {
            self.declare(name);
            return;
        }
        if let Some(open) = self.terms.last_mut() {
            open.insert(bare.to_string());
        }
    }

    fn term_in_scope(&self, name: &str) -> bool {
        self.terms.iter().any(|open| open.contains(name))
    }

    fn declare(&mut self, name: &str) {
        let bare = bare_name(name);
        if bare.is_empty() {
            return;
        }
        self.declared.insert(bare.to_string());
        // `constant term:<$bar> = 42` declares `$bar`, which the parser reads
        // as a bare term, sigil and all.
        if bare.len() != name.len() {
            self.declared.insert(name.to_string());
        }
        // `sub term:<x> {...}` declares the bare term `x`.
        if let Some(inner) = term_name(bare) {
            self.declared.insert(inner.to_string());
        }
    }

    fn record_call(&mut self, name: &str) {
        if self.bail {
            return;
        }
        // `require`/`import` in call form pull in names at runtime that the
        // walker cannot see — give up on the whole unit.
        if matches!(name, "require" | "import" | "need" | "use" | "EVALFILE") {
            self.bail = true;
            return;
        }
        let Some(first) = name.chars().next() else {
            return;
        };
        // Only plain lowercase bareword calls are checked: uppercase names are
        // type coercions (a different error class with many more legitimate
        // sources), qualified/adverbed names carry `:`/`::`, and `__`-names
        // are compiler-synthesized.
        if !(first.is_ascii_lowercase() || self.eval && first.is_ascii_uppercase())
            || name.contains(':')
            || name.starts_with("__")
        {
            return;
        }
        if self.term_in_scope(name) {
            return;
        }
        self.calls.push((name.to_string(), self.line));
        self.call_frames.push(self.current);
    }
}

/// The bare term a `term:<x>` routine declares.
fn term_name(bare: &str) -> Option<&str> {
    bare.strip_prefix("term:<")
        .and_then(|s| s.strip_suffix('>'))
}

/// Whether a declared variable name is a sigilless term (`my \x`, `constant x`).
fn is_term_decl(name: &str) -> bool {
    let bare = name.strip_prefix('\\').unwrap_or(name);
    !bare.is_empty() && !bare.starts_with(['$', '@', '%', '&'])
}
