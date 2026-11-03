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
use std::collections::{HashMap, HashSet};

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
    /// `declared` without the names that only a variable, parameter, label or
    /// other term declares: those excuse a call by their position
    /// ([`Scan::term_decls`]), not unit-wide (ADR-15760 Amendment 1, slice 1b).
    pub(super) call_declared: HashSet<String>,
    /// For each name a term declares: the pre-order position and the lexical
    /// block of every declaration.
    term_decls: HashMap<String, Vec<(usize, usize)>>,
    /// The enclosing block of each lexical block; block 0 is the unit.
    blocks: Vec<Option<usize>>,
    /// The lexical block the walk is in.
    block: usize,
    /// Pre-order position of the next call or term declaration.
    position: usize,
    /// The position and block of each entry of `calls`.
    call_positions: Vec<(usize, usize)>,
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
            blocks: vec![None],
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
    /// explains, and every call that a variable, parameter, label or other
    /// term explains by being declared before the call in a block that
    /// encloses it (rakudo resolves those lexically, in source order).
    // Cost: O(c * (d + t)), c = calls, d = routine-body nesting depth, t =
    // declarations of the called name.
    pub(super) fn resolve_body_scoped_calls(&mut self) {
        let frames = &self.frames;
        let blocks = &self.blocks;
        let term_decls = &self.term_decls;
        let mut call_frames = std::mem::take(&mut self.call_frames).into_iter();
        let mut positions = std::mem::take(&mut self.call_positions).into_iter();
        self.calls.retain(|(name, _)| {
            let mut frame = call_frames.next().flatten();
            let (position, block) = positions.next().unwrap_or((0, 0));
            while let Some(i) = frame {
                if frames[i].routines.contains(name) {
                    return false;
                }
                frame = frames[i].parent;
            }
            // A term declared earlier in a block that encloses the call.
            if let Some(decls) = term_decls.get(name) {
                let mut at = Some(block);
                while let Some(b) = at {
                    if decls
                        .iter()
                        .any(|&(declared_at, declared_in)| declared_in == b && declared_at < position)
                    {
                        return false;
                    }
                    at = blocks[b];
                }
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
        let outer_block = self.enter_block();
        walk_stmt(self, stmt);
        self.block = outer_block;
        self.current = parent;
    }

    /// Enter a new lexical block; returns the one to go back to.
    fn enter_block(&mut self) -> usize {
        let outer = self.block;
        self.blocks.push(Some(outer));
        self.block = self.blocks.len() - 1;
        outer
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
        self.declare_with(name, false);
    }

    /// Declare the name a variable, parameter, label or other term introduces:
    /// it explains a call by its position (see [`Scan::resolve_body_scoped_calls`]).
    fn declare_term(&mut self, name: &str) {
        self.declare_with(name, true);
    }

    fn declare_with(&mut self, name: &str, ordered: bool) {
        let bare = bare_name(name);
        if bare.is_empty() {
            return;
        }
        let mut names = vec![bare.to_string()];
        // `constant term:<$bar> = 42` declares `$bar`, which the parser reads
        // as a bare term, sigil and all.
        if bare.len() != name.len() {
            names.push(name.to_string());
        }
        // `sub term:<x> {...}` declares the bare term `x`.
        if let Some(inner) = term_name(bare) {
            names.push(inner.to_string());
        }
        for declared in names {
            self.declared.insert(declared.clone());
            if ordered {
                let at = self.position;
                self.position += 1;
                self.term_decls
                    .entry(declared)
                    .or_default()
                    .push((at, self.block));
            } else {
                self.call_declared.insert(declared);
            }
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
        self.call_positions.push((self.position, self.block));
        self.position += 1;
    }
}

/// The bare term a `term:<x>` routine declares.
fn term_name(bare: &str) -> Option<&str> {
    bare.strip_prefix("term:<")
        .and_then(|s| s.strip_suffix('>'))
}

impl<'ast> Visit<'ast> for Scan {
    fn visit_stmts(&mut self, body: &'ast [Stmt]) {
        let outer = self.enter_block();
        walk_stmts(self, body);
        self.block = outer;
    }

    fn visit_stmt(&mut self, stmt: &'ast Stmt) {
        if let Stmt::Package { name, .. } = stmt {
            for parent in crate::qualified::package_ancestors(*name).skip(1) {
                self.declared.insert(parent.as_str().to_owned());
                self.call_declared.insert(parent.as_str().to_owned());
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
            // A `my method` is a lexical routine of the scope it is written in
            // (`my method m {...}; ... m(self)`): its name belongs to that
            // scope, not to its own body.
            Stmt::MethodDecl {
                name, is_my: true, ..
            } => {
                self.declare_routine(name.as_str());
                self.walk_routine_body(stmt);
            }
            Stmt::MethodDecl { .. } => self.walk_routine_body(stmt),
            // The wrapper the parser puts around one source declaration
            // (`my \x := $y` becomes `{ my \x := $y }`): it is no lexical
            // block of the source, so what it declares belongs to the block
            // around it.
            Stmt::SyntheticBlock(body) => walk_stmts(self, body),
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
            | NameKind::Label => self.declare_term(name),
            _ => {}
        }
    }
}

