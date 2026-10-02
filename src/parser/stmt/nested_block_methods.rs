//! `method`s declared in a nested block of a package body, hoisted into the
//! package.
//!
//! A `method` declarator is has-scoped: wherever it lexically sits inside a
//! package body -- a bare block, a `do { }`, an `if` branch -- it installs the
//! method in that package, while the body still closes over the enclosing
//! block's lexicals. Data::Record uses exactly this to keep a helper `sub`
//! private to one method:
//!
//! ```raku
//! do { # hide this sub
//!     proto sub unrecord(Mu) is raw {*}
//!     multi sub unrecord(Mu \value) { value }
//!     method unrecord(::?CLASS:D: --> List:D) { @!record.map(&unrecord).List }
//! }
//! ```
//!
//! Package registration only installs methods that are direct statements of
//! the body, so [`hoist`] rewrites the body once it has parsed:
//!
//! - the declaration is appended to the package body as an ordinary method
//!   statement, carrying the [`NESTED_BLOCK_METHOD_TRAIT`] marker with a
//!   per-body index. Every registration pass then sees an ordinary method:
//!   the method table, `.^methods`, role requirements, return type,
//!   private/submethod/multi status and traits all come from the one path.
//!   Like rakudo, it is installed whether or not the block ever runs;
//! - in the block, the declaration is replaced by a
//!   [`Stmt::NestedMethodCapture`] with the same index. Its closure is an
//!   anonymous method with the declaration's signature and body, so the
//!   ordinary closure machinery computes what the body closes over; the
//!   package-body walk gives that capture to the hoisted method.
//!
//! `my method` / `our method` in a block are not has-scoped and are left
//! alone, as is anything inside a nested routine or package declaration
//! (those own their own scope).

use crate::ast::{DoBlockOrigin, Expr, RoutineDeclarator, Stmt};
use crate::symbol::Symbol;
use crate::value::Value;

/// The parser marker trait on a hoisted nested-block method; its argument is
/// the per-package-body index shared with the in-block
/// [`Stmt::NestedMethodCapture`]. Read back into
/// `CompiledMethodDecl::nested_capture_index`.
pub(crate) const NESTED_BLOCK_METHOD_TRAIT: &str = "__nested_block_method";

/// Hoist every has-scoped `method` declared in a nested block of the package
/// body `body` (see the module doc).
pub(crate) fn hoist(body: &mut Vec<Stmt>) {
    let mut hoisted = Vec::new();
    for stmt in body.iter_mut() {
        // A top-level statement is already a package-body statement; only
        // the blocks nested in it are scanned.
        for nested in nested_blocks_mut(stmt) {
            hoist_in_block(nested, &[], &mut hoisted);
        }
        if let Some(routine_body) = routine_body_mut(stmt) {
            hoist_in_block(routine_body, &[], &mut hoisted);
        }
    }
    body.extend(hoisted);
}

/// `outer_routines`: the routines declared by the blocks enclosing `block`
/// (inside the package body), which a method declared here closes over.
fn hoist_in_block(block: &mut [Stmt], outer_routines: &[Symbol], hoisted: &mut Vec<Stmt>) {
    let mut routines = outer_routines.to_vec();
    collect_block_routines(block, &mut routines);
    for stmt in block.iter_mut() {
        hoist_proto_methods(stmt, hoisted);
        if is_has_scoped_method(stmt) {
            // Its own body may declare methods too (they belong to the same
            // package); rewrite it first so the capture closure and the
            // hoisted copy share the rewritten body.
            if let Some(routine_body) = routine_body_mut(stmt) {
                hoist_in_block(routine_body, &routines, hoisted);
            }
            let index = hoisted.len() as u32;
            let closure = capture_closure(stmt);
            let mut decl = std::mem::replace(
                stmt,
                Stmt::NestedMethodCapture {
                    index,
                    closure: Box::new(closure),
                    routines: routines.clone(),
                },
            );
            if let Stmt::MethodDecl { custom_traits, .. } = &mut decl {
                custom_traits.push((
                    NESTED_BLOCK_METHOD_TRAIT.to_string(),
                    Some(Expr::Literal(Value::int(i64::from(index)))),
                ));
            }
            hoisted.push(decl);
            continue;
        }
        if let Stmt::SyntheticBlock(inner) = stmt {
            hoist_in_block(inner, &routines, hoisted);
            continue;
        }
        for nested in nested_blocks_mut(stmt) {
            hoist_in_block(nested, &routines, hoisted);
        }
        if let Some(routine_body) = routine_body_mut(stmt) {
            hoist_in_block(routine_body, &routines, hoisted);
        }
    }
}

/// The body of a routine declared in the package body (at any depth): a
/// `method` (`multi method handler` inside `method ^find_method`, the
/// Object::Trampoline shape), a `sub` or a `proto`. A `method` declarator in
/// it is still has-scoped to the package -- rakudo installs `class C { sub
/// f { method m { } } }`'s `m` in `C` -- and closes over the routine's
/// lexicals as of its latest invocation, which the in-body
/// [`Stmt::NestedMethodCapture`] marker re-files on every call.
fn routine_body_mut(stmt: &mut Stmt) -> Option<&mut Vec<Stmt>> {
    match stmt {
        Stmt::SubDecl { body, .. }
        | Stmt::MethodDecl { body, .. }
        | Stmt::ProtoDecl { body, .. } => Some(body),
        _ => None,
    }
}

/// A `proto method` in a nested block or routine body is has-scoped too, and
/// so is one used as a term (`my constant &proto-handler = proto method
/// handler(|) {*}`, Object::Trampoline): a copy is added to the package body
/// so the package-body walk installs it as the dispatcher of the `multi
/// method`s declared beside it. The declaration itself stays in place (its
/// in-place registration is a no-op for a method proto) and, as a term,
/// evaluates to that proto (`Compiler::compile_expr_do_stmt`). A proto's
/// `{*}` body closes over nothing worth capturing, so the copy carries no
/// capture index; it carries the [`NESTED_BLOCK_METHOD_TRAIT`] marker (with
/// no argument) so [`unhoist`] can tell it from a proto written in the body.
fn hoist_proto_methods(stmt: &Stmt, hoisted: &mut Vec<Stmt>) {
    struct Protos<'a> {
        out: &'a mut Vec<Stmt>,
        entered: bool,
    }
    impl<'ast> crate::ast_visit::Visit<'ast> for Protos<'_> {
        fn visit_expr(&mut self, expr: &'ast Expr) {
            if let Expr::DoStmt(inner) = expr
                && let Stmt::ProtoDecl {
                    is_method: true, ..
                } = inner.as_ref()
            {
                self.out.push(marked_proto_copy(inner));
                return;
            }
            crate::ast_visit::walk_expr(self, expr);
        }
        // Only `stmt`'s own expressions: the statements nested in it are
        // scanned by the block walk itself, which knows the package and
        // routine boundaries.
        fn visit_stmt(&mut self, stmt: &'ast Stmt) {
            if !std::mem::replace(&mut self.entered, true) {
                crate::ast_visit::walk_stmt(self, stmt);
            }
        }
    }
    match stmt {
        Stmt::ProtoDecl {
            is_method: true, ..
        } => hoisted.push(marked_proto_copy(stmt)),
        Stmt::SubDecl { .. } | Stmt::MethodDecl { .. } | Stmt::ProtoDecl { .. } => {}
        _ => {
            let mut visitor = Protos {
                out: hoisted,
                entered: false,
            };
            crate::ast_visit::Visit::visit_stmt(&mut visitor, stmt);
        }
    }
}

/// The names of the `sub`s and `proto`s `block` itself declares (a
/// `SyntheticBlock` shares its scope), added to `out` once each.
fn collect_block_routines(block: &[Stmt], out: &mut Vec<Symbol>) {
    for stmt in crate::ast::scope_members(block) {
        match stmt {
            Stmt::SubDecl {
                name,
                name_expr: None,
                ..
            }
            | Stmt::ProtoDecl {
                name,
                is_method: false,
                ..
            } if !out.contains(name) => out.push(*name),
            _ => {}
        }
    }
}

/// A `submethod` always carries `is_my` from the parser; like the class-body
/// walk (`class_body_method_decl`'s `is_lexical_only`), only a non-submethod
/// `is_my` is a lexical `my method`.
fn is_has_scoped_method(stmt: &Stmt) -> bool {
    matches!(
        stmt,
        Stmt::MethodDecl {
            is_my,
            is_our: false,
            is_submethod,
            ..
        } if !*is_my || *is_submethod
    )
}

/// The nested blocks of a statement that share the package body's scope for
/// the purpose of `method` installation: bare and `do` blocks and the blocks
/// of control-flow statements. Routine, package and phaser bodies are not
/// among them.
fn nested_blocks_mut(stmt: &mut Stmt) -> Vec<&mut Vec<Stmt>> {
    match stmt {
        Stmt::Block(body)
        | Stmt::While { body, .. }
        | Stmt::Loop { body, .. }
        | Stmt::For { body, .. }
        | Stmt::Given { body, .. }
        | Stmt::When { body, .. }
        | Stmt::Default(body) => vec![body],
        Stmt::If {
            then_branch,
            else_branch,
            ..
        } => vec![then_branch, else_branch],
        Stmt::Expr(Expr::DoBlock {
            body,
            origin: DoBlockOrigin::SourceBlock,
            ..
        }) => vec![body],
        _ => Vec::new(),
    }
}

/// An anonymous method over `decl`'s signature and body: building it where
/// the declaration stood captures what the body closes over. It is never
/// called.
fn capture_closure(decl: &Stmt) -> Expr {
    let Stmt::MethodDecl {
        params,
        param_defs,
        body,
        is_submethod,
        ..
    } = decl
    else {
        unreachable!("capture_closure expects a MethodDecl");
    };
    let declarator = if *is_submethod {
        RoutineDeclarator::Submethod
    } else {
        RoutineDeclarator::Method
    };
    // A method literal's invocant is its leading synthetic `self`; an
    // explicitly declared invocant is not an extra positional.
    let mut all_params = vec!["self".to_string()];
    let mut all_param_defs = vec![crate::parser::primary::ident::anon_sub::invocant_param_def()];
    for (name, pd) in params.iter().zip(param_defs.iter()) {
        if pd.is_invocant {
            continue;
        }
        all_params.push(name.clone());
        all_param_defs.push(pd.clone());
    }
    Expr::AnonSubParams {
        params: all_params,
        param_defs: all_param_defs,
        return_type: None,
        body: body.clone(),
        is_rw: false,
        is_raw: false,
        custom_traits: Default::default(),
        is_whatever_code: false,
        declarator,
    }
}

/// The package-body copy of a hoisted `proto method`: the declaration with
/// the [`NESTED_BLOCK_METHOD_TRAIT`] marker added (a `__` marker, which trait
/// application skips).
fn marked_proto_copy(stmt: &Stmt) -> Stmt {
    let mut copy = stmt.clone();
    if let Stmt::ProtoDecl {
        custom_traits,
        trait_args,
        ..
    } = &mut copy
    {
        custom_traits.push(NESTED_BLOCK_METHOD_TRAIT.to_string());
        trait_args.push((NESTED_BLOCK_METHOD_TRAIT.to_string(), None));
    }
    copy
}

/// Whether `stmt` is a declaration [`hoist`] appended to the package body.
fn is_hoisted_copy(stmt: &Stmt) -> bool {
    match stmt {
        Stmt::MethodDecl { custom_traits, .. } => custom_traits
            .iter()
            .any(|(t, _)| t == NESTED_BLOCK_METHOD_TRAIT),
        Stmt::ProtoDecl { custom_traits, .. } => {
            custom_traits.iter().any(|t| t == NESTED_BLOCK_METHOD_TRAIT)
        }
        _ => false,
    }
}

/// The inverse of [`hoist`]: the package body as written. Each in-block
/// [`Stmt::NestedMethodCapture`] marker is replaced by the method declaration
/// it stood for, and the copies `hoist` appended are dropped. The RakuAST
/// reader renders a package from this form (rakudo's tree has the
/// declaration where it was written), and its lowering hoists again.
pub(crate) fn unhoist(body: &[Stmt]) -> Vec<Stmt> {
    let mut methods: Vec<Option<Stmt>> = Vec::new();
    let mut written = Vec::with_capacity(body.len());
    for stmt in body {
        if !is_hoisted_copy(stmt) {
            written.push(stmt.clone());
            continue;
        }
        let Stmt::MethodDecl { custom_traits, .. } = stmt else {
            continue;
        };
        let Some(index) = custom_traits.iter().find_map(|(t, arg)| match arg {
            Some(Expr::Literal(v)) if t == NESTED_BLOCK_METHOD_TRAIT => v.as_int(),
            _ => None,
        }) else {
            continue;
        };
        let mut decl = stmt.clone();
        if let Stmt::MethodDecl { custom_traits, .. } = &mut decl {
            custom_traits.retain(|(t, _)| t != NESTED_BLOCK_METHOD_TRAIT);
        }
        let index = index as usize;
        if methods.len() <= index {
            methods.resize(index + 1, None);
        }
        methods[index] = Some(decl);
    }
    struct Restore(Vec<Option<Stmt>>);
    impl crate::ast_visit::VisitMut for Restore {
        fn visit_stmt_mut(&mut self, stmt: &mut Stmt) {
            if let Stmt::NestedMethodCapture { index, .. } = stmt
                && let Some(decl) = self.0.get_mut(*index as usize).and_then(Option::take)
            {
                *stmt = decl;
            }
            // A nested package's markers index its own hoisted list.
            if matches!(
                stmt,
                Stmt::ClassDecl { .. } | Stmt::RoleDecl { .. } | Stmt::Package { .. }
            ) {
                return;
            }
            crate::ast_visit::walk_stmt_mut(self, stmt);
        }
    }
    let mut restore = Restore(methods);
    for stmt in &mut written {
        crate::ast_visit::VisitMut::visit_stmt_mut(&mut restore, stmt);
    }
    written
}
