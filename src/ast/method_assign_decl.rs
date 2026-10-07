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
    pub(crate) type_constraint: Option<String>,
    pub(crate) is_state: bool,
    pub(crate) is_our: bool,
    pub(crate) is_dynamic: bool,
    pub(crate) is_export: bool,
    pub(crate) export_tags: Vec<String>,
    pub(crate) custom_traits: Vec<(String, Option<Expr>)>,
    pub(crate) where_constraint: Option<Box<Expr>>,
    pub(crate) method: Symbol,
    pub(crate) args: Vec<Expr>,
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

/// Compile-oriented declaration, with the source form removed.
pub(crate) fn expanded_declaration(form: &MethodAssignDecl) -> Stmt {
    let target = invocant(&form.name, form.type_constraint.as_deref(), form.is_v6c);
    let mut custom_traits = form.custom_traits.clone();
    if !matches!(target, Expr::BareWord(_)) {
        custom_traits.push((METHOD_ASSIGN_DECL_TRAIT.to_string(), None));
    } else if !custom_traits.iter().any(|(n, _)| n == "__has_initializer") {
        custom_traits.push(("__has_initializer".to_string(), None));
    }
    Stmt::VarDecl {
        name: form.name.clone(),
        expr: Expr::MethodCall {
            target: Box::new(target),
            name: form.method,
            args: form.args.clone(),
            modifier: None,
            quoted: false,
            sugar: false,
        },
        type_constraint: form.type_constraint.clone(),
        is_state: form.is_state,
        is_our: form.is_our,
        is_dynamic: form.is_dynamic,
        is_export: form.is_export,
        export_tags: form.export_tags.clone(),
        custom_traits,
        where_constraint: form.where_constraint.clone(),
    }
}

/// Keep the source form ahead of its bytecode-ready expansion.
pub(crate) fn expand(form: MethodAssignDecl) -> Stmt {
    let declaration = expanded_declaration(&form);
    Stmt::SyntheticBlock(vec![
        Stmt::SourceForm(Box::new(SourceForm::MethodAssignDecl(form))),
        declaration,
    ])
}
