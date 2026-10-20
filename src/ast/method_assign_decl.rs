//! Preserve a declaration's `.=` initializer until the RakuAST boundary.

use super::{Expr, SourceForm, Stmt};
use crate::symbol::Symbol;

/// An untyped `.=` initializer reads its own container as the invocant;
/// the redeclaration check must not treat that as a premature self-read.
pub(crate) const METHOD_ASSIGN_DECL_TRAIT: &str = "__method_assign_decl";

/// The source spelling of `my [Type] $x .= method(...)`.
#[derive(Debug, Clone, Hash, serde::Serialize, serde::Deserialize)]
pub(crate) struct MethodAssignDecl {
    pub(crate) name: String,
    pub(crate) anonymous: bool,
    pub(crate) type_constraint: Option<String>,
    pub(crate) is_state: bool,
    pub(crate) is_our: bool,
    pub(crate) is_dynamic: bool,
    pub(crate) is_export: bool,
    pub(crate) export_tags: Vec<String>,
    pub(crate) custom_traits: Vec<(String, Option<Expr>)>,
    pub(crate) where_constraint: Option<Box<Expr>>,
    pub(crate) method: Symbol,
    /// A term as the method (`my $x .= &f`, `my $x .= $m`): the call's
    /// `name_expr`. `method` is then empty.
    pub(crate) callee: Option<Box<Expr>>,
    pub(crate) args: Vec<Expr>,
    /// A postfix chain after the call (`my $o .= $m.uc`), over the declared
    /// variable: it applies to the new value and is sunk, as in Rakudo.
    pub(crate) chain: Option<Box<Expr>>,
    /// A shaped array's dimensions (`my @c[2;2] .= new(...)`), kept for the
    /// source only: the call builds the container.
    pub(crate) shape: Option<Vec<Expr>>,
    pub(crate) is_v6c: bool,
}

/// The invocant a declaration's `.=` calls, shared by parsing and lowering.
pub(crate) fn invocant(name: &str, type_constraint: Option<&str>, is_v6c: bool) -> Expr {
    match type_constraint {
        Some(c) if name.starts_with('@') => {
            if crate::native_types::is_native_array_element_type(c) {
                Expr::BareWord(format!("array[{c}]"))
            } else {
                Expr::BareWord(format!("Array[{c}]"))
            }
        }
        Some(c) if name.starts_with('%') => Expr::BareWord(format!("Hash[{c}]")),
        Some(c) => {
            let target = if is_v6c {
                c
            } else {
                c.strip_suffix(":U")
                    .or_else(|| c.strip_suffix(":D"))
                    .or_else(|| c.strip_suffix(":_"))
                    .unwrap_or(c)
            };
            Expr::BareWord(target.to_string())
        }
        None => match name.chars().next() {
            Some('@') => Expr::ArrayVar(name[1..].to_string()),
            Some('%') => Expr::HashVar(name[1..].to_string()),
            Some('$') => Expr::Var(name[1..].to_string()),
            _ => Expr::Var(name.to_string()),
        },
    }
}

/// Whether an untyped `.=` declaration is lowered as "declare, then call on
/// the declared container". Its invocant is the fresh variable (an `Any`, an
/// empty Array/Hash, or an `is default(...)` value), never a same-named outer
/// binding, so the call has to run after the new lexical is bound.
///
/// `state` keeps the single-initializer form (it initializes once), and so do
/// `our`, a dynamic, a shaped and a typed declaration, whose invocant is not
/// a lexical a later statement could reach by name.
fn declares_before_call(form: &MethodAssignDecl) -> bool {
    form.type_constraint.is_none()
        && form.shape.is_none()
        && !form.is_state
        && !form.is_our
        && !form.is_dynamic
        && !form.name.starts_with('&')
        && !form.name.contains(['*', '?', '!', '.', '^', ':'])
}

/// The value a fresh untyped declaration of `name` starts with, as the parser
/// spells it for a declaration without an initializer.
fn fresh_container(name: &str) -> Expr {
    match name.chars().next() {
        Some('@') => Expr::Literal(crate::value::Value::real_array(Vec::new())),
        Some('%') => Expr::Hash(Vec::new(), super::HashSpelling::Composer),
        _ => Expr::Literal(crate::value::Value::NIL),
    }
}

/// Compile-oriented declaration, with the source form removed.
pub(crate) fn expanded_declaration(form: &MethodAssignDecl) -> Vec<Stmt> {
    let target = invocant(&form.name, form.type_constraint.as_deref(), form.is_v6c);
    let call = method_call(form, target.clone());
    if declares_before_call(form) {
        // `my $x .= m(...)` is `my $x; $x = $x.m(...)`: the name in the
        // call reads the slot the declaration just bound (#13273).
        return vec![
            Stmt::VarDecl {
                anonymous: form.anonymous,
                name: form.name.clone(),
                expr: fresh_container(&form.name),
                type_constraint: None,
                is_state: false,
                is_our: false,
                is_dynamic: false,
                is_export: form.is_export,
                export_tags: form.export_tags.clone(),
                custom_traits: form.custom_traits.clone(),
                where_constraint: form.where_constraint.clone(),
            },
            Stmt::Assign {
                name: form.name.clone(),
                expr: call,
                op: super::AssignOp::Assign,
                target_is_sigilless: false,
            },
        ];
    }
    let mut custom_traits = form.custom_traits.clone();
    if !matches!(target, Expr::BareWord(_)) {
        custom_traits.push((METHOD_ASSIGN_DECL_TRAIT.to_string(), None));
    } else if form.shape.is_none() && !custom_traits.iter().any(|(n, _)| n == "__has_initializer") {
        custom_traits.push(("__has_initializer".to_string(), None));
    }
    vec![Stmt::VarDecl {
        anonymous: form.anonymous,
        name: form.name.clone(),
        expr: call,
        type_constraint: form.type_constraint.clone(),
        is_state: form.is_state,
        is_our: form.is_our,
        is_dynamic: form.is_dynamic,
        is_export: form.is_export,
        export_tags: form.export_tags.clone(),
        custom_traits,
        where_constraint: form.where_constraint.clone(),
    }]
}

/// The `.=` call on `target`.
fn method_call(form: &MethodAssignDecl, target: Expr) -> Expr {
    match &form.callee {
        Some(callee) => Expr::DynamicMethodCall {
            target: Box::new(target),
            name_expr: callee.clone(),
            args: form.args.clone(),
            modifier: None,
            quoted: false,
        },
        None => Expr::MethodCall {
            target: Box::new(target),
            name: form.method,
            args: form.args.clone(),
            modifier: None,
            quoted: false,
            sugar: false,
        },
    }
}

/// Keep the source form ahead of its bytecode-ready expansion.
pub(crate) fn expand(form: MethodAssignDecl) -> Stmt {
    let declaration = expanded_declaration(&form);
    let chain = form.chain.clone();
    let mut stmts = vec![Stmt::SourceForm(Box::new(SourceForm::MethodAssignDecl(
        form,
    )))];
    stmts.extend(declaration);
    stmts.extend(chain.map(|chain| Stmt::Expr(*chain)));
    Stmt::SyntheticBlock(stmts)
}
