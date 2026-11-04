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

/// The lexical scope of one statement list below the unit's top level.
struct Frame {
    /// The enclosing scope, if any.
    parent: Option<usize>,
    /// The routines declared in this scope (outside any nested scope).
    routines: HashSet<String>,
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

    /// Drop every call that a routine declared in an enclosing routine body
    /// explains.
    // Cost: O(c * d), c = calls, d = routine-body nesting depth.
    pub(super) fn resolve_body_scoped_calls(&mut self) {
        let frames = &self.frames;
        let mut call_frames = std::mem::take(&mut self.call_frames).into_iter();
        self.calls.retain(|(name, _)| {
            let mut frame = call_frames.next().flatten();
            while let Some(i) = frame {
                if frames[i].routines.contains(name) {
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
        if let Some(i) = scope {
            let routines = &mut self.frames[i].routines;
            routines.insert(bare.to_string());
            // `sub term:<x> {...}` declares the bare term `x`.
            if let Some(inner) = term_name(bare) {
                routines.insert(inner.to_string());
            }
            self.body_routines.extend(routines.iter().cloned());
            return;
        }
        self.declare(name);
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

impl<'ast> Visit<'ast> for Scan {
    /// Every statement list below the unit's top level is a lexical scope of
    /// its own.
    fn visit_stmts(&mut self, body: &'ast [Stmt]) {
        let parent = self.current;
        self.frames.push(Frame {
            parent,
            routines: HashSet::new(),
        });
        self.current = Some(self.frames.len() - 1);
        self.terms.push(HashSet::new());
        walk_stmts(self, body);
        self.terms.pop();
        self.current = parent;
    }

    fn visit_stmt(&mut self, stmt: &'ast Stmt) {
        if let Stmt::Package { name, .. } = stmt {
            for parent in crate::qualified::package_ancestors(*name).skip(1) {
                self.declared.insert(parent.as_str().to_owned());
            }
        }
        // The `EVAL` scan keeps walking after a bail: its declarations also
        // feed the undeclared-name check (`scope_blind_declared_names`).
        if self.bail && !self.eval {
            return;
        }
        match stmt {
            Stmt::SetLine(n, _) => self.line = *n,
            Stmt::Use { module, .. } if !self.eval => {
                if !conditional::imports_no_routines(module) {
                    self.bail = true;
                }
            }
            Stmt::No { .. } | Stmt::Need { .. } | Stmt::Import { .. } if !self.eval => {
                self.bail = true;
            }
            // Dynamically-named sub: the declared name is unknowable.
            Stmt::SubDecl {
                name_expr: Some(_), ..
            } => self.bail = true,
            // The routine's own name belongs to the enclosing scope; the
            // walk reports it again inside the body, where it is harmless
            // (a routine can call itself).
            Stmt::SubDecl {
                name, is_export, ..
            } => {
                if *is_export {
                    self.declare_unit_wide_routine(name.as_str());
                } else {
                    self.declare_routine(name.as_str());
                }
                self.walk_routine_body(stmt);
            }
            Stmt::ProtoDecl { name, .. } => {
                // A top-level `proto` stays a plain declaration (not a
                // suggestion candidate), as it always was.
                if self.current.is_some() {
                    self.declare_routine(name.as_str());
                } else {
                    self.declare(name.as_str());
                }
                self.proto_name = Some(name.to_string());
                self.walk_routine_body(stmt);
            }
            Stmt::MethodDecl { .. } => self.walk_routine_body(stmt),
            // A parser desugaring (`sub a {...} given 3`) is no lexical scope.
            Stmt::SyntheticBlock(body) => walk_stmts(self, body),
            // The keys of an enum are terms from the declaration on.
            Stmt::EnumDecl { variants, .. } => {
                // A computed body (`(:30SLA, slip <SLB>)`) holds its keys in the
                // `__DYNAMIC__` entry: the shared helper reads them out.
                for key in crate::parser::enum_value_names(variants) {
                    self.declare_term(&key);
                    self.enum_keys.push(key);
                }
                walk_stmt(self, stmt);
                self.enum_keys.clear();
            }
            // A bare lowercase identifier standing alone as a whole statement
            // (`dead;`) is, syntactically, the exact same "no-args routine
            // reference" `record_call` already recognizes for `Stmt::Call` --
            // rakudo's parser resolves an unrecognized bare lowercase term this
            // way and reports the same "Undeclared routine" error for it
            // (verified against `raku`: a `unit class Foo; dead` body dies with
            // "Undeclared routine:\n    dead used at line N"), where mutsu
            // previously fell through the runtime's bareword resolution to a
            // plain Str. `self` is the one common legitimate bare statement
            // this parses to (a method's own bare `self` statement) and is
            // excluded explicitly.
            Stmt::Expr(Expr::BareWord(name)) if name != "self" => self.record_call(name),
            // An explicit-invocant call (`foo($obj: ...)`) is a method call.
            Stmt::Call { args, .. } if args.iter().any(|a| matches!(a, CallArg::Invocant(_))) => {
                for a in args {
                    walk_call_arg(self, a);
                }
            }
            _ => walk_stmt(self, stmt),
        }
    }

    fn visit_expr(&mut self, expr: &'ast Expr) {
        // A lowercase bareword in any expression position (`say bar`,
        // `my $x = bar`) is the same "no-args routine reference" as the
        // statement form above; rakudo rejects it at CHECK time too.
        if let Expr::BareWord(name) = expr
            && name != "self"
            && !name.starts_with(|c: char| c.is_ascii_uppercase())
        {
            self.record_call(name);
        }
        walk_expr(self, expr);
    }

    fn visit_regex_node(&mut self, node: &'ast RegexNode) {
        // `<value:sym<number>>` names a proto candidate: the parser keeps the
        // `sym<number>` tail as a subscript on the bareword `sym`, which is
        // the adverb's name, not a call to a routine called `sym`.
        if let RegexNode::Subrule {
            name,
            args: Some(args),
            ..
        }
        | RegexNode::SubruleAlias {
            name,
            args: Some(args),
            ..
        } = node
            && args.source.as_deref().is_some_and(|s| s.starts_with("sym<"))
        {
            self.visit_name(name, NameKind::Regex);
            return;
        }
        walk_regex_node(self, node);
    }

    fn visit_param(&mut self, param: &'ast ParamDef) {
        // A type capture (`::T`, `role R[::T]`, `method m(::T:)`) declares
        // the type name `T` for the rest of its scope.
        if let Some(capture) = &param.type_capture {
            self.declare(capture);
        }
        walk_param(self, param);
    }

    fn visit_name(&mut self, name: &str, kind: NameKind) {
        match kind {
            NameKind::Call | NameKind::UserRoutineCall => self.record_call(name),
            NameKind::SubDecl => self.declare_routine(name),
            // A `proto`'s own name, already declared in the enclosing scope.
            NameKind::Decl if self.proto_name.as_deref() == Some(name) => {
                self.proto_name = None;
                self.declare_routine(name);
            }
            NameKind::Decl if self.enum_keys.iter().any(|k| k == name) => {}
            NameKind::VarDecl if is_term_decl(name) => self.declare_term(name),
            // Declarations are collected scope-blind. Assignment targets are
            // not declarations, but suppressing calls to an assigned name is
            // the safe (false-negative) direction. A callable parameter
            // (`&f`, also a role's `role R[&f]`) is in scope as the routine
            // `f`.
            NameKind::Decl
            | NameKind::VarDecl
            | NameKind::AssignTarget
            | NameKind::Param
            | NameKind::BlockParam
            // A label (`L: for ...`) is a term in its scope (`last L`,
            // `:label(L)`).
            | NameKind::Label => self.declare(name),
            _ => {}
        }
    }
}

