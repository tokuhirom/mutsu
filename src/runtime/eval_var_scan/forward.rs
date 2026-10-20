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
            type_names: all_names.1,
            moved: false,
            lenient: false,
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
    /// The types and packages the unit declares.
    pub(super) type_names: HashSet<String>,
    /// Inside code the AST moved from its source position (a phaser body):
    /// a use of a name the top level declares is not judged there.
    pub(super) moved: bool,
    /// Inside a type or package body, or a phaser: the AST moves some
    /// declarations there away from their source scope (a method declared in
    /// a nested block of a class body, a package body's statements), so a
    /// name some other scope declares is not judged there.
    pub(super) lenient: bool,
    /// A `use` whose exported variables are unknown was seen: nothing is
    /// reported from here on.
    pub(super) bail: bool,
}


impl UndeclaredVars<'_> {
    /// Runs `f` over code the AST moved from its source position.
    pub(super) fn in_moved_code(&mut self, f: impl FnOnce(&mut Self)) {
        let saved = self.forward.as_mut().map(|forward| std::mem::replace(&mut forward.moved, true));
        f(self);
        if let (Some(forward), Some(saved)) = (self.forward.as_mut(), saved) {
            forward.moved = saved;
        }
    }

    /// Runs `f` over a type or package body, or a phaser body, where a name
    /// some other scope declares is not judged.
    pub(super) fn in_lenient_code(&mut self, f: impl FnOnce(&mut Self)) {
        let saved = self
            .forward
            .as_mut()
            .map(|forward| std::mem::replace(&mut forward.lenient, true));
        f(self);
        if let (Some(forward), Some(saved)) = (self.forward.as_mut(), saved) {
            forward.lenient = saved;
        }
    }

    /// In the compunit mode, a `use` declares the variables its module
    /// exports, when the parse-time scan knows them; otherwise nothing after
    /// it is reported.
    pub(super) fn forward_use(&mut self, module: &str, tags: &[String]) {
        let Some(forward) = &self.forward else {
            // An `EVAL`'d snippet: the `use` declares the variables the scan
            // knows its module exports, a `sub EXPORT` hook's literal keys
            // included (ADR-8751); whatever else it exports is the env's.
            for var in crate::parser::scanned_exported_vars(module)
                .iter()
                .flat_map(|vars| vars.iter())
                .filter(|var| !var.term && var.imported_by(tags))
            {
                self.declare(&var.name);
            }
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
        // Reported: a name declared nowhere, one only the unit's top level
        // declares (later), and one only a scope that does not enclose the
        // use declares (`sub f { $x }; { my $x }`). Inside a type or package
        // body or a phaser, a name some other scope declares is left alone:
        // the AST moves some declarations there away from their source scope
        // (a method declared in a nested block of a class body).
        forward.bail
            || (forward.lenient && all > top)
            || (forward.moved && top > 0)
            || forward.type_names.contains(&key)
            // An extended identifier (`$foo:bar<42>`) may name one a
            // declaration spells with an interpolated adverb, which only the
            // compiler resolves.
            || (key.contains(':') && forward.all_names.keys().any(|n| crate::adverb_name::needs_interp(n)))
    }
}

/// Every variable name any scope of a unit declares, in `declared_var_name`
/// form: its `my`/`our`/`state` declarations, parameters and attribute
/// aliases, wherever the AST holds them; and the types and packages it
/// declares (`RT .= new` assigns to a type, not to an undeclared `$RT`).
#[derive(Default)]
struct DeclaredNames(HashMap<String, usize>, HashSet<String>);

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
            NameKind::Decl => {
                self.1.insert(name.to_string());
            }
            _ => {}
        }
    }
}
