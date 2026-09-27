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
    }
    body.extend(hoisted);
}

/// `outer_routines`: the routines declared by the blocks enclosing `block`
/// (inside the package body), which a method declared here closes over.
fn hoist_in_block(block: &mut [Stmt], outer_routines: &[Symbol], hoisted: &mut Vec<Stmt>) {
    let mut routines = outer_routines.to_vec();
    collect_block_routines(block, &mut routines);
    for stmt in block.iter_mut() {
        if is_has_scoped_method(stmt) {
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
    }
}

/// The names of the `sub`s and `proto`s `block` itself declares (a
/// `SyntheticBlock` shares its scope), added to `out` once each.
fn collect_block_routines(block: &[Stmt], out: &mut Vec<Symbol>) {
    for stmt in block {
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
            } => {
                if !out.contains(name) {
                    out.push(*name);
                }
            }
            Stmt::SyntheticBlock(inner) => collect_block_routines(inner, out),
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
