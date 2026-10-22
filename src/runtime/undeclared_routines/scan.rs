//! The AST walk of the undeclared-routine check: which names the unit
//! declares, and which bareword calls it makes.
//!
//! Declarations are collected scope-blind, except a routine (`sub`, `proto`)
//! declared inside a `sub`/`method`/`proto` body: rakudo installs it in that
//! body's lexical scope (an `our` one additionally as a package symbol, which
//! only a qualified call reaches), so a bare call outside the body is
//! `Undeclared routine` (#13111). Such a routine is recorded in the body's
//! [`Frame`], and a call is explained by it only when the call sits in that
//! body — before or after the declaration, since the check runs at CHECK time.
//! Types and variables stay scope-blind: a `class K` in a routine body is
//! `our`-scoped and reachable from outside.

use crate::ast::{CallArg, Expr, ParamDef, Stmt};
use crate::ast_visit::{
    NameKind, Visit, walk_call_arg, walk_expr, walk_param, walk_regex_node, walk_stmt, walk_stmts,
};
use crate::regex_tree::RegexNode;
use std::collections::HashSet;

use super::conditional;

/// The lexical scope of one routine body.
struct Frame {
    /// The enclosing routine body, if any.
    parent: Option<usize>,
    /// The routines declared in this body (outside any nested routine body).
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
    pub(super) fn new(eval: bool) -> Self {
        Scan {
            line: 1,
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
        let bare = bare_name(name);
        if bare.is_empty() {
            return;
        }
        if let Some(i) = self.current {
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
        self.calls.push((name.to_string(), self.line));
        self.call_frames.push(self.current);
    }
}

/// The bare term a `term:<x>` routine declares.
fn term_name(bare: &str) -> Option<&str> {
    bare.strip_prefix("term:<")
        .and_then(|s| s.strip_suffix('>'))
}

impl<'ast> Visit<'ast> for Scan {
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
            Stmt::SubDecl { name, .. } => {
                self.declare_routine(name.as_str());
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

