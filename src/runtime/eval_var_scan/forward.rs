//! The compunit mode of the undeclared-variable scan (#12800): a variable
//! used before the unit's top level declares it.

use super::*;

/// The first variable the unit uses before its top level declares it
/// (#12800): `sub f { $zz = 42 }; my $zz = 1;` is `X::Undeclared` at
/// compile time in rakudo, as is `say $x; my $x`. A narrow form of
/// [`first_undeclared_var`] over a whole compunit: a use is reported only
/// when nothing in scope there declares it and the unit's top level declares
/// that name later. Any other undeclared use is left to run time, as are
/// names a `use` of a module whose exported variables the parse-time scan
/// does not know may provide.
// Cost: O(n * d), n = size of the unit's AST, d = depth of the scope stack.
pub(in crate::runtime) fn first_var_used_before_unit_declares(
    interp: &Interpreter,
    stmts: &[Stmt],
) -> Option<Undeclared> {
    let mut top_names = HashSet::new();
    collect_top_level_decls(stmts, &mut top_names);
    if top_names.is_empty() {
        return None;
    }
    let mut refs = VarRefs::default();
    walk_stmts(&mut refs, stmts);
    let placeholder_names = refs
        .names
        .iter()
        .filter_map(|n| n.strip_prefix(['^', ':']).map(str::to_string))
        .collect();
    let mut scan = UndeclaredVars {
        interp,
        placeholder_names,
        package_exports: super::eval_package_exports::package_exported_vars(stmts),
        scopes: vec![HashSet::new()],
        strict: true,
        judged_stmt: true,
        forward: Some(ForwardUses {
            top_names,
            bail: false,
        }),
        found: None,
    };
    walk_stmts(&mut scan, stmts);
    scan.found
}

/// The variable names the unit's top level declares, in `declared_var_name`
/// form, including those of a desugared multi-declaration.
// Cost: O(t), t = top-level statements.
fn collect_top_level_decls(stmts: &[Stmt], out: &mut HashSet<String>) {
    // The top level and the synthetic blocks a desugaring left there; no
    // scope is entered.
    let mut pending = vec![stmts];
    while let Some(list) = pending.pop() {
        for stmt in list {
            if let Stmt::VarDecl { name, .. } = stmt {
                out.insert(declared_var_name(name).to_string());
            } else if let Stmt::SyntheticBlock(inner) = stmt {
                pending.push(inner);
            }
        }
    }
}

/// [`first_var_used_before_unit_declares`]'s mode of the scan.
pub(super) struct ForwardUses {
    /// What the unit's top level declares.
    pub(super) top_names: HashSet<String>,
    /// A `use` whose exported variables are unknown was seen: nothing is
    /// reported from here on.
    pub(super) bail: bool,
}


impl UndeclaredVars<'_> {
    /// In the compunit mode, a `use` declares the variables its module
    /// exports, when the parse-time scan knows them; otherwise nothing after
    /// it is reported.
    pub(super) fn forward_use(&mut self, module: &str, tags: &[String]) {
        let Some(forward) = &self.forward else {
            return;
        };
        if forward.bail {
            return;
        }
        match crate::parser::scanned_exported_vars(module) {
            Some(vars) => {
                for var in vars.iter().filter(|var| var.imported_by(tags)) {
                    self.declare(&var.name);
                }
            }
            None => {
                if let Some(forward) = &mut self.forward {
                    forward.bail = true;
                }
            }
        }
    }


    /// In the compunit mode, whether the undeclared use `found` is left to
    /// run time: the unit's top level does not declare it later, or the
    /// scan gave up.
    // Cost: O(|name|).
    pub(super) fn forward_ignores(&self, found: Option<&Undeclared>) -> bool {
        let (Some(forward), Some((sigil, name, _))) = (&self.forward, found) else {
            return false;
        };
        let key = if *sigil == "$" {
            name.clone()
        } else {
            format!("{sigil}{name}")
        };
        forward.bail || !forward.top_names.contains(&key)
    }
}
