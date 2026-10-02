//! Placeholder (`$^a`, `$:b`, `@_`, `%_`) collection over the typed AST
//! visitor (ADR-0137).
//!
//! Two scopes are asked about:
//!
//! - a block's **own placeholder scope** ([`walk_stmt_placeholder_scope`] /
//!   [`walk_expr_placeholder_scope`]): what belongs to the block itself, which
//!   stops wherever the ADR-0048 oracle ([`super::placeholder_kind`]) says a
//!   nested construct takes a signature of its own (or may not take one). The
//!   signature collector, the unattached-placeholder check and the ordering
//!   checks in `crate::placeholder_order` all walk it, so they agree on where a
//!   block's placeholder scope ends;
//! - the **deep** scope ([`walk_stmt_deep_scope`]): every nested block and
//!   closure too, stopping only at routine and package declarations. A
//!   `where` block's binder uses it.

use super::placeholder_kind::{
    PlaceholderBodyKind, placeholder_body_kind, placeholder_body_kind_expr,
};
use super::regex_placeholders::{
    placeholder_display_name, regex_literal_source, regex_source_placeholders,
};
use super::{Expr, Stmt};
use crate::ast_visit::{NameKind, Visit, walk_expr, walk_stmt};
use crate::compiler::scope_scan::{is_scope_declaration, opens_own_scope, walk_control_header};
use crate::regex_tree::RegexNode;

/// A statement whose body is a `{ ... }` block (or a statement modifier's
/// lowering of one) that the ADR-0048 oracle classifies.
// Cost: O(1).
fn has_block_body(stmt: &Stmt) -> bool {
    matches!(
        stmt,
        Stmt::If { .. }
            | Stmt::While { .. }
            | Stmt::For { .. }
            | Stmt::Loop { .. }
            | Stmt::Given { .. }
            | Stmt::When { .. }
            | Stmt::Whenever { .. }
            | Stmt::Block(_)
            | Stmt::SyntheticBlock(_)
            | Stmt::React { .. }
            | Stmt::Default(_)
            | Stmt::Catch(_)
            | Stmt::Control(_)
            | Stmt::Phaser { .. }
    )
}

/// Compile-time declarations, and the thunks they carry, which never run as
/// part of the enclosing block: an `enum`'s values, an attribute default, a
/// `use`/`no`/`need`/`import` argument, a `does` role argument, a nested
/// method capture's closure and a `DOC` phaser.
// Cost: O(1).
fn is_compile_time_declaration(stmt: &Stmt) -> bool {
    matches!(
        stmt,
        Stmt::EnumDecl { .. }
            | Stmt::HasDecl { .. }
            | Stmt::Use { .. }
            | Stmt::No { .. }
            | Stmt::Need { .. }
            | Stmt::Import { .. }
            | Stmt::DoesDecl { .. }
            | Stmt::NestedMethodCapture { .. }
            | Stmt::DocPhaser(_)
    )
}

/// Walks the parts of `stmt` that belong to the enclosing block's own
/// placeholder scope.
///
/// A control statement's header (`if` condition, loop iterable, condition and
/// C-style init/cond/step, `given` topic, `when` matcher, `whenever` supply)
/// always runs in this scope; its body joins it only when the oracle says
/// [`PlaceholderBodyKind::Transparent`] (statement modifiers, the parser's
/// synthetic blocks). Routine and package declarations (a `role` body is a
/// signature-capable boundary, ADR-0048 D7) and compile-time declarations are
/// not entered. A variable's `where` thunk is: rakudo makes `$^a` in
/// `{ my $x where $^a > 0 = 1 }` the block's parameter.
// Cost: O(n), n = size of the part of `stmt` in the enclosing placeholder scope.
pub(crate) fn walk_stmt_placeholder_scope<'ast, V: Visit<'ast> + ?Sized>(
    v: &mut V,
    stmt: &'ast Stmt,
) {
    match stmt {
        s if is_scope_declaration(s) || is_compile_time_declaration(s) => {}
        // A variable's type constraint and traits are compile-time; its
        // initializer and `where` thunk run here.
        Stmt::VarDecl {
            name,
            expr,
            where_constraint,
            ..
        } => {
            v.visit_name(name, NameKind::VarDecl);
            v.visit_expr(expr);
            if let Some(w) = where_constraint {
                v.visit_expr(w);
            }
        }
        s if has_block_body(s) => {
            if placeholder_body_kind(s) == PlaceholderBodyKind::Transparent {
                walk_stmt(v, s);
            } else {
                walk_control_header(v, s);
            }
        }
        _ => walk_stmt(v, stmt),
    }
}

/// Walks `expr` within the enclosing block's own placeholder scope: a nested
/// body (closure, `gather`, `try`, `once`, phaser) is entered only when the
/// oracle says [`PlaceholderBodyKind::Transparent`] — a WhateverCode, a bare
/// `{}` value, a `do {}` block.
// Cost: O(n), n = size of the part of `expr` in the enclosing placeholder scope.
pub(crate) fn walk_expr_placeholder_scope<'ast, V: Visit<'ast> + ?Sized>(
    v: &mut V,
    expr: &'ast Expr,
) {
    if opens_own_scope(expr) && placeholder_body_kind_expr(expr) != PlaceholderBodyKind::Transparent
    {
        return;
    }
    walk_expr(v, expr);
}

/// Walks `stmt` and every nested block and closure, stopping only at routine
/// and non-role package declarations (compiled by their own pass) and at
/// compile-time declarations. A `role` body is entered: it is a
/// signature-capable nested block (ADR-0048 D7).
// Cost: O(n), n = size of the part of `stmt` outside nested routines/packages.
pub(crate) fn walk_stmt_deep_scope<'ast, V: Visit<'ast> + ?Sized>(v: &mut V, stmt: &'ast Stmt) {
    match stmt {
        Stmt::RoleDecl { .. } => walk_stmt(v, stmt),
        s if is_scope_declaration(s) || is_compile_time_declaration(s) => {}
        _ => walk_stmt(v, stmt),
    }
}

/// `^name` for a placeholder assignment target, whatever its sigil.
fn push_if_placeholder(name: &str, out: &mut Vec<String>) {
    let bare = name.trim_start_matches(|c: char| "$@%&".contains(c));
    if let Some(rest) = bare.strip_prefix('^') {
        let key = format!("^{rest}");
        if !out.contains(&key) {
            out.push(key);
        }
    }
}

fn push_unique(name: String, out: &mut Vec<String>) {
    if !out.contains(&name) {
        out.push(name);
    }
}

/// The collector's name for a placeholder variable mentioned at a position of
/// kind `kind`: `^a` for `$^a`, `@^a`, `%^a`, `&^a` (and `:` for named ones).
fn placeholder_key(name: &str, kind: NameKind) -> Option<String> {
    if !(name.starts_with('^') || name.starts_with(':')) {
        return None;
    }
    match kind {
        NameKind::Var => Some(name.to_string()),
        NameKind::CodeVar => Some(format!("&{name}")),
        NameKind::ArrayVar => Some(format!("@{name}")),
        NameKind::HashVar => Some(format!("%{name}")),
        _ => None,
    }
}

/// Scan source text that is parsed again later (an `s///` pattern or
/// replacement, an interpolating heredoc body) for placeholder variables
/// (`$^a`, `@^a`, `%^a`, `&^a`), in the format [`placeholder_key`] produces.
fn collect_placeholders_in_str(src: &str, out: &mut Vec<String>) {
    let bytes = src.as_bytes();
    let mut i = 0;
    while i + 2 < bytes.len() {
        let sigil = bytes[i];
        if matches!(sigil, b'$' | b'@' | b'%' | b'&')
            && bytes[i + 1] == b'^'
            && bytes[i + 2].is_ascii_alphabetic()
        {
            let start = i + 2;
            let mut j = start;
            while j < bytes.len() && (bytes[j].is_ascii_alphanumeric() || bytes[j] == b'_') {
                j += 1;
            }
            let name = &src[start..j];
            let entry = match sigil {
                b'$' => format!("^{name}"),
                b'@' => format!("@^{name}"),
                b'%' => format!("%^{name}"),
                _ => format!("&^{name}"),
            };
            push_unique(entry, out);
            i = j;
        } else {
            i += 1;
        }
    }
}

fn placeholder_sort_key(name: &str) -> &str {
    let without_sigil = name.strip_prefix(['$', '@', '%', '&']).unwrap_or(name);
    without_sigil
        .strip_prefix(['^', ':'])
        .unwrap_or(without_sigil)
}

/// Sort by the name component, so `$^a`, `@^a`, `%^a`, `&^a` all sort as `a`.
fn sorted(mut names: Vec<String>) -> Vec<String> {
    names.sort_by(|a, b| placeholder_sort_key(a).cmp(placeholder_sort_key(b)));
    names.dedup();
    names
}

/// Which scope a [`PlaceholderCollector`] walks.
#[derive(Clone, Copy, PartialEq, Eq)]
enum Scope {
    Own,
    Deep,
}

/// Collects placeholder names: the ones read (variable mentions and source
/// text parsed later) and/or the ones assigned to.
struct PlaceholderCollector {
    scope: Scope,
    reads: bool,
    targets: bool,
    out: Vec<String>,
}

impl PlaceholderCollector {
    fn run(scope: Scope, reads: bool, targets: bool, stmts: &[Stmt]) -> Vec<String> {
        let mut c = PlaceholderCollector {
            scope,
            reads,
            targets,
            out: Vec::new(),
        };
        for stmt in stmts {
            c.visit_stmt(stmt);
        }
        c.out
    }
}

impl<'ast> Visit<'ast> for PlaceholderCollector {
    fn visit_stmt(&mut self, stmt: &'ast Stmt) {
        match self.scope {
            Scope::Own => walk_stmt_placeholder_scope(self, stmt),
            Scope::Deep => walk_stmt_deep_scope(self, stmt),
        }
    }

    fn visit_expr(&mut self, expr: &'ast Expr) {
        // A placeholder interpolated into a regex (`/$^a/`) belongs to the
        // enclosing block; the literal keeps only its source (mutsu#10542).
        if self.reads
            && let Some(src) = regex_literal_source(expr)
        {
            for key in regex_source_placeholders(&src).interpolated {
                push_unique(key, &mut self.out);
            }
        }
        match self.scope {
            Scope::Own => walk_expr_placeholder_scope(self, expr),
            Scope::Deep => walk_expr(self, expr),
        }
    }

    // A regex code block (`/<?{ $^a }>/`) is a block of its own that takes no
    // signature (the compiler rejects a placeholder there), and the regex's
    // interpolations are read from its source in `visit_expr`, so its tree
    // adds nothing.
    fn visit_regex_node(&mut self, _node: &'ast RegexNode) {}

    fn visit_name(&mut self, name: &str, kind: NameKind) {
        match kind {
            NameKind::Var | NameKind::CodeVar | NameKind::ArrayVar | NameKind::HashVar
                if self.reads =>
            {
                if let Some(key) = placeholder_key(name, kind) {
                    push_unique(key, &mut self.out);
                }
            }
            // `s/.../$^a/` and an interpolating heredoc are parsed again when
            // they run, in this block's scope.
            NameKind::Source if self.reads => collect_placeholders_in_str(name, &mut self.out),
            // `$^a = 1`: an assignment target still declares the parameter.
            NameKind::AssignTarget | NameKind::VarDecl if self.targets => {
                push_if_placeholder(name, &mut self.out)
            }
            _ => {}
        }
    }
}

/// Every placeholder in `stmts`, nested blocks and closures included (stops at
/// routine and package declarations), sorted by name and deduplicated.
/// Assignment targets are not included — see
/// [`collect_where_assign_placeholders`].
// Cost: O(n log n), n = size of `stmts`' subtree.
pub(crate) fn collect_placeholders(stmts: &[Stmt]) -> Vec<String> {
    sorted(PlaceholderCollector::run(Scope::Deep, true, false, stmts))
}

/// The placeholders that make up the block's own signature: its own
/// placeholder scope only (nested closures and signature-taking blocks own
/// theirs), assignment targets included, sorted by name and deduplicated.
// Cost: O(n log n), n = size of the block's own placeholder scope.
pub(crate) fn collect_placeholders_shallow(stmts: &[Stmt]) -> Vec<String> {
    sorted(PlaceholderCollector::run(Scope::Own, true, true, stmts))
}

/// Placeholder names (`^name`, sigil stripped) that are the *target* of an
/// assignment anywhere in a `where`-block body — e.g. the `^epic` in
/// `where { $^epic = "fail" }`. A `where`-block parameter is read-only, so the
/// caller binds these and marks them read-only to make the assignment die.
// Cost: O(n), n = size of `stmts`' subtree.
pub(crate) fn collect_where_assign_placeholders(stmts: &[Stmt]) -> Vec<String> {
    PlaceholderCollector::run(Scope::Deep, false, true, stmts)
}

/// Collects placeholders used where no signature can capture them; see
/// [`collect_unattached_placeholders`].
struct UnattachedCollector {
    out: Vec<String>,
}

impl<'ast> Visit<'ast> for UnattachedCollector {
    fn visit_stmt(&mut self, stmt: &'ast Stmt) {
        walk_stmt_placeholder_scope(self, stmt);
    }

    fn visit_expr(&mut self, expr: &'ast Expr) {
        // Unlike the signature collector, stop at a `do {}` block and a bare
        // `{}` value too: each rejects its own stray placeholder with the
        // "surrounding block does not take a signature" error, as rakudo
        // does (`do { $^a }`, `"{$^a}"`), and must not be reported as this
        // scope's. A WhateverCode is no boundary (`say * + $^a` at the
        // mainline is rejected as the mainline's).
        let whatever_code = matches!(
            expr,
            Expr::Lambda {
                is_whatever_code: true,
                ..
            } | Expr::AnonSubParams {
                is_whatever_code: true,
                ..
            }
        );
        if opens_own_scope(expr) && !whatever_code {
            return;
        }
        if let Some(src) = regex_literal_source(expr) {
            for key in regex_source_placeholders(&src).interpolated {
                push_unique(placeholder_display_name(key), &mut self.out);
            }
        }
        walk_expr(self, expr);
    }

    // See `PlaceholderCollector::visit_regex_node`.
    fn visit_regex_node(&mut self, _node: &'ast RegexNode) {}

    fn visit_name(&mut self, name: &str, kind: NameKind) {
        let sigil = match kind {
            NameKind::Var => '$',
            NameKind::CodeVar => '&',
            NameKind::ArrayVar => '@',
            NameKind::HashVar => '%',
            _ => return,
        };
        let implicit_slurpy = name == "_" && matches!(sigil, '@' | '%');
        if name.starts_with('^') || name.starts_with(':') || implicit_slurpy {
            push_unique(format!("{sigil}{name}"), &mut self.out);
        }
    }
}

/// Collect placeholder variables that belong directly to the *current*
/// (non-signature) scope: the mainline, a `do {}` block, or a class/role/module
/// body. Walks this scope's own placeholder scope (statement headers,
/// statement-modifier bodies, expressions) but stops at any nested `{}` block
/// that could capture them.
///
/// Also recognizes the implicit slurpy placeholders `@_` / `%_`. Returns
/// display names like `$^x`, `@^a`, `@_`; a positive means a placeholder is
/// genuinely used where no signature can capture it.
// Cost: O(n), n = size of the current scope's own part of `stmts`.
pub(crate) fn collect_unattached_placeholders(stmts: &[Stmt]) -> Vec<String> {
    let mut c = UnattachedCollector { out: Vec::new() };
    for stmt in stmts {
        c.visit_stmt(stmt);
    }
    c.out
}

#[cfg(test)]
#[path = "placeholders_tests.rs"]
mod tests;
