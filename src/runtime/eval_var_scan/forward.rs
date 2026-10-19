//! The compunit mode of the undeclared-variable scan (#12800): a variable
//! the unit declares nowhere, or uses before its top level declares it.

use super::*;

/// The first variable the unit uses where rakudo rejects it at compile time
/// with `X::Undeclared` (#12800), of two kinds:
///
/// - a name nothing in the unit declares at all (`$zz = 42` with no `my
///   $zz` anywhere), the common typo;
/// - a name the unit's top level declares only later (`sub f { $zz = 42 };
///   my $zz = 1;`, `say $x; my $x`), which a routine would otherwise resolve
///   at run time to its caller's lexical of that name.
///
/// A narrow form of [`first_undeclared_var`] over a whole compunit. A use
/// that some scope of the unit declares, other than a later top level, is
/// left to run time: the scan's scopes follow the AST, and declarations the
/// AST moves away from their source position (a method declared in a nested
/// block of a class body, a phaser body, a package body's statements) would
/// otherwise read as undeclared. So are names a `use` of a module whose
/// exported variables the parse-time scan does not know may provide.
pub(in crate::runtime) fn first_var_used_before_unit_declares(
    interp: &Interpreter,
    stmts: &[Stmt],
) -> Option<Undeclared> {
    let mut top_names = HashMap::new();
    collect_top_level_decls(stmts, &mut top_names);
    let mut all_names = DeclaredNames::default();
    walk_stmts(&mut all_names, stmts);
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
            all_names: all_names.0,
            bail: false,
        }),
        found: None,
    };
    walk_stmts(&mut scan, stmts);
    scan.found
}

/// The variable names the unit's top level declares, in `declared_var_name`
/// form, including those of a desugared multi-declaration, with how many
/// declarations of each it holds.
// Cost: O(t), t = top-level statements.
fn collect_top_level_decls(stmts: &[Stmt], out: &mut HashMap<String, usize>) {
    // The top level and the synthetic blocks a desugaring left there; no
    // scope is entered.
    let mut pending = vec![stmts];
    while let Some(list) = pending.pop() {
        for stmt in list {
            if let Stmt::VarDecl { name, .. } = stmt {
                *out.entry(declared_var_name(name).to_string()).or_default() += 1;
            } else if let Stmt::SyntheticBlock(inner) = stmt {
                pending.push(inner);
            }
        }
    }
}

/// [`first_var_used_before_unit_declares`]'s mode of the scan.
pub(super) struct ForwardUses {
    /// What the unit's top level declares, with the number of declarations.
    pub(super) top_names: HashMap<String, usize>,
    /// What any scope of the unit declares, with the number of declarations.
    pub(super) all_names: HashMap<String, usize>,
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
        match crate::parser::scanned_exported_vars_complete(module) {
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
    /// run time: some scope of the unit other than a later top level
    /// declares it, or the scan gave up.
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
        let top = forward.top_names.get(&key).copied().unwrap_or(0);
        let all = forward.all_names.get(&key).copied().unwrap_or(0);
        // Reported: a name declared nowhere, and one only the unit's top
        // level declares (later). A name some inner scope declares as well is
        // left alone: the AST moves some declarations away from their source
        // scope (a method declared in a nested block of a class body).
        forward.bail
            || (all > 0 && all > top)
            // An extended identifier (`$foo:bar<42>`) may name one a
            // declaration spells with an interpolated adverb, which only the
            // compiler resolves.
            || (key.contains(':') && forward.all_names.keys().any(|n| crate::adverb_name::needs_interp(n)))
    }
}

/// Every variable name any scope of a unit declares, in `declared_var_name`
/// form: its `my`/`our`/`state` declarations, parameters and attribute
/// aliases, wherever the AST holds them.
#[derive(Default)]
struct DeclaredNames(HashMap<String, usize>);

impl<'ast> Visit<'ast> for DeclaredNames {
    // A pointy `given`'s parameters live only in its source form: the
    // lowering binds a destructured one (`-> ($a, |rest)`) by assignment.
    fn visit_source_form(&mut self, form: &'ast crate::ast::SourceForm) {
        crate::ast_visit::walk_source_form(self, form);
    }

    fn visit_name(&mut self, name: &str, kind: NameKind) {
        match kind {
            NameKind::VarDecl | NameKind::Param | NameKind::BlockParam => {
                if name == "self" {
                    *self.0.entry(crate::env::LEX_SELF.to_string()).or_default() += 1;
                }
                if name.starts_with('$') {
                    *self.0.entry(name.to_string()).or_default() += 1;
                }
                *self.0.entry(declared_var_name(name).to_string()).or_default() += 1;
            }
            _ => {}
        }
    }
}
