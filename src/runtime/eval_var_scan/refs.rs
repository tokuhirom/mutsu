//! The variable references of an expression, and the parameter `where`
//! check that reads them.

use super::*;

/// A parameter's `where` constraint is checked left-to-right: it may name
/// the parameter it constrains and any *earlier* parameter, but referencing a
/// *later* parameter is `X::Undeclared` (that param is not in scope yet).
/// Matches `sub foo($x where { $x == $y }, $y) {}` → X::Undeclared.
/// (subtypes.t 77)
pub(super) fn later_param_in_where(param_defs: &[ParamDef]) -> Option<Undeclared> {
    let named: Vec<&ParamDef> = param_defs.iter().filter(|pd| !pd.name.is_empty()).collect();
    for (i, pd) in named.iter().enumerate() {
        let Some(where_expr) = &pd.where_constraint else {
            continue;
        };
        let mut refs = VarRefs::default();
        refs.visit_expr(where_expr);
        let earlier = |r: &str| named[..=i].iter().any(|p| bare_var_name(&p.name) == r);
        if let Some(r) = refs.names.iter().find(|r| {
            named[i + 1..]
                .iter()
                .any(|p| bare_var_name(&p.name) == r.as_str())
                && !earlier(r)
        }) {
            return Some(("$", r.clone(), Vec::new()));
        }
    }
    None
}

/// The names of the variables (`$`, `@`, `%`, `&`) an expression refers to.
#[derive(Default)]
pub(super) struct VarRefs {
    pub(super) names: Vec<String>,
}

impl<'ast> Visit<'ast> for VarRefs {
    fn visit_expr(&mut self, expr: &'ast Expr) {
        if let Expr::Var(n) | Expr::ArrayVar(n) | Expr::HashVar(n) | Expr::CodeVar(n) = expr {
            self.names.push(n.clone());
        }
        walk_expr(self, expr);
    }
}
