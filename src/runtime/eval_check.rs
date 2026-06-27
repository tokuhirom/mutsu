use super::*;
use crate::ast::{PhaserKind, Stmt};

/// Collect the names of all types (class/role/enum/subset) declared anywhere in
/// `stmts`, descending into block-like bodies. Used so a sub parameter type that
/// names a type declared in the same compilation unit is not falsely rejected.
fn collect_declared_type_names(
    stmts: &[Stmt],
    out: &mut std::collections::HashSet<String>,
    packages: &mut std::collections::HashSet<String>,
    classes: &mut std::collections::HashSet<String>,
) {
    for stmt in stmts {
        match stmt {
            Stmt::ClassDecl { name, body, .. } => {
                // A plain `class` is type-like but NOT parametric (parameterizing
                // it with `[T]`/`of T` is X::NotParametric); roles ARE parametric.
                out.insert(name.resolve().to_string());
                classes.insert(name.resolve().to_string());
                collect_declared_type_names(body, out, packages, classes);
            }
            Stmt::RoleDecl { name, body, .. } => {
                out.insert(name.resolve().to_string());
                collect_declared_type_names(body, out, packages, classes);
            }
            Stmt::EnumDecl { name, variants, .. } => {
                out.insert(name.resolve().to_string());
                // Enum values are valid value-params (`sub f(SomeEnumValue)`),
                // and serve as suggestions for a mistyped one.
                for (vname, _) in variants {
                    out.insert(vname.clone());
                }
            }
            Stmt::SubsetDecl { name, .. } => {
                out.insert(name.resolve().to_string());
            }
            Stmt::Package {
                name, kind, body, ..
            } => {
                // `module`/`package` are not type-like (a parameter typed by one
                // is X::Parameter::BadType); `grammar` is a real type.
                if matches!(
                    kind,
                    crate::ast::PackageKind::Module | crate::ast::PackageKind::Package
                ) {
                    packages.insert(name.resolve().to_string());
                } else {
                    out.insert(name.resolve().to_string());
                }
                collect_declared_type_names(body, out, packages, classes);
            }
            Stmt::Block(body) | Stmt::SyntheticBlock(body) => {
                collect_declared_type_names(body, out, packages, classes);
            }
            _ => {}
        }
    }
}

/// Validate parameter type constraints for plain subs in `stmts` (descending
/// into bare blocks/packages, but not class/role bodies whose methods may
/// legitimately forward-reference the enclosing type).
fn walk_validate_sub_param_types(
    interp: &Interpreter,
    stmts: &[Stmt],
    declared: &std::collections::HashSet<String>,
    packages: &std::collections::HashSet<String>,
    classes: &std::collections::HashSet<String>,
) -> Result<(), RuntimeError> {
    for stmt in stmts {
        match stmt {
            Stmt::SubDecl {
                param_defs,
                body,
                return_type,
                custom_traits,
                ..
            } => {
                interp.validate_param_type_constraints(param_defs, declared, packages, classes)?;
                let via_trait = custom_traits.iter().any(|(t, _)| t == "__return_via_trait");
                interp.validate_return_type_constraint(
                    return_type.as_deref(),
                    param_defs,
                    declared,
                    via_trait,
                )?;
                walk_validate_sub_param_types(interp, body, declared, packages, classes)?;
            }
            Stmt::Block(body) | Stmt::SyntheticBlock(body) | Stmt::Package { body, .. } => {
                walk_validate_sub_param_types(interp, body, declared, packages, classes)?;
            }
            _ => {}
        }
    }
    Ok(())
}

impl Interpreter {
    /// Reject sub parameter types that name a type unknown to this compilation
    /// unit (e.g. `sub yoink(Junctoin $barf)`) -> X::Parameter::InvalidType.
    pub(crate) fn check_eval_param_type_constraints(
        &self,
        stmts: &[Stmt],
    ) -> Result<(), RuntimeError> {
        let mut declared = std::collections::HashSet::new();
        let mut packages = std::collections::HashSet::new();
        let mut classes = std::collections::HashSet::new();
        collect_declared_type_names(stmts, &mut declared, &mut packages, &mut classes);
        walk_validate_sub_param_types(self, stmts, &declared, &packages, &classes)
    }

    /// Reject a type-parameter argument that names an undeclared type, e.g.
    /// `my Array[Numerix] $x` -> X::Undeclared::Symbols (gist mentions `Numerix`).
    /// Only the inner `[...]` arguments are checked here (the base type's
    /// parametric-ness is X::NotParametric, handled elsewhere). All declared type
    /// names are collected first so forward references are honored.
    pub(crate) fn check_eval_undeclared_type_args(
        &self,
        stmts: &[Stmt],
    ) -> Result<(), RuntimeError> {
        let mut declared = std::collections::HashSet::new();
        let mut packages = std::collections::HashSet::new();
        let mut classes = std::collections::HashSet::new();
        collect_declared_type_names(stmts, &mut declared, &mut packages, &mut classes);
        self.walk_validate_type_args(stmts, &declared)
    }

    /// If `tc` is `Base[arg, ...]`, return the first inner argument that names an
    /// undeclared type (an uppercase bareword that is neither declared nor a known
    /// built-in / resolvable type). `::T` capture args and lowercase/native args
    /// are ignored.
    fn first_undeclared_type_arg(
        &self,
        tc: &str,
        declared: &std::collections::HashSet<String>,
    ) -> Option<String> {
        let open = tc.find('[')?;
        let close = tc.rfind(']')?;
        if close <= open + 1 {
            return None;
        }
        let inner = &tc[open + 1..close];
        for raw in inner.split(',') {
            let mut arg = raw.trim();
            // Strip a trailing type smiley (`:D`/`:U`/`:_`) ONLY — must not split a
            // `::`-qualified name like `Ber::Meow` (which contains `:`).
            for smiley in [":D", ":U", ":_"] {
                if let Some(stripped) = arg.strip_suffix(smiley) {
                    arg = stripped;
                    break;
                }
            }
            let arg = arg.trim();
            if arg.is_empty() || arg.starts_with("::") {
                continue;
            }
            // Only consider a bare uppercase type identifier.
            if !arg.starts_with(|c: char| c.is_ascii_uppercase())
                || !arg
                    .chars()
                    .all(|c| c.is_ascii_alphanumeric() || c == '_' || c == '-' || c == ':')
            {
                continue;
            }
            if declared.contains(arg) || self.has_type(arg) || self.is_resolvable_type(arg) {
                continue;
            }
            return Some(arg.to_string());
        }
        None
    }

    fn walk_validate_type_args(
        &self,
        stmts: &[Stmt],
        declared: &std::collections::HashSet<String>,
    ) -> Result<(), RuntimeError> {
        let check = |tc: Option<&str>| -> Option<String> {
            tc.and_then(|t| self.first_undeclared_type_arg(t, declared))
        };
        for stmt in stmts {
            let bad = match stmt {
                Stmt::VarDecl {
                    type_constraint, ..
                } => check(type_constraint.as_deref()),
                Stmt::SubDecl {
                    param_defs,
                    return_type,
                    ..
                } => param_defs
                    .iter()
                    .find_map(|pd| check(pd.type_constraint.as_deref()))
                    .or_else(|| check(return_type.as_deref())),
                _ => None,
            };
            if let Some(name) = bad {
                let suggestions = self.suggest_type_names(&name);
                return Err(RuntimeError::undeclared_type_symbols(
                    &name,
                    format!("Undeclared name:\n    {} used at line 1", name),
                    suggestions,
                ));
            }
            // Recurse into nested bodies.
            match stmt {
                Stmt::ClassDecl { body, .. }
                | Stmt::RoleDecl { body, .. }
                | Stmt::Package { body, .. }
                | Stmt::Block(body)
                | Stmt::SyntheticBlock(body)
                | Stmt::SubDecl { body, .. } => {
                    self.walk_validate_type_args(body, declared)?;
                }
                _ => {}
            }
        }
        Ok(())
    }

    /// Reject a `trusts T` declaration whose target type `T` is not declared
    /// anywhere in this compilation unit (nor a known built-in type)
    /// -> X::Undeclared (symbol => T, what => "Type"). Forward references are
    /// honored because all declared type names are collected first.
    pub(crate) fn check_eval_undeclared_trusts(&self, stmts: &[Stmt]) -> Result<(), RuntimeError> {
        let mut declared = std::collections::HashSet::new();
        let mut packages = std::collections::HashSet::new();
        let mut classes = std::collections::HashSet::new();
        collect_declared_type_names(stmts, &mut declared, &mut packages, &mut classes);
        self.walk_validate_trusts(stmts, &declared)
    }

    fn walk_validate_trusts(
        &self,
        stmts: &[Stmt],
        declared: &std::collections::HashSet<String>,
    ) -> Result<(), RuntimeError> {
        for stmt in stmts {
            match stmt {
                Stmt::TrustsDecl { name } => {
                    let target = name.resolve();
                    // A bare identifier (e.g. `Bar`) that names no declared type and
                    // is not a known built-in/resolvable type is undeclared.
                    let known = declared.contains(target.as_str())
                        || self.has_type(&target)
                        || self.is_resolvable_type(&target);
                    if !known {
                        let mut attrs = std::collections::HashMap::new();
                        attrs.insert("symbol".to_string(), Value::str(target.to_string()));
                        attrs.insert("what".to_string(), Value::str("Type".to_string()));
                        attrs.insert(
                            "message".to_string(),
                            Value::str(format!("Type '{}' is not declared", target)),
                        );
                        return Err(RuntimeError::typed("X::Undeclared", attrs));
                    }
                }
                Stmt::ClassDecl { body, .. }
                | Stmt::RoleDecl { body, .. }
                | Stmt::Package { body, .. }
                | Stmt::Block(body)
                | Stmt::SyntheticBlock(body) => {
                    self.walk_validate_trusts(body, declared)?;
                }
                _ => {}
            }
        }
        Ok(())
    }

    /// Parse and run only BEGIN/CHECK phasers from EVAL'd code (`:check` mode).
    fn parse_and_check_only_with_operators(
        &mut self,
        src: &str,
        op_names: &[String],
        op_assoc: &HashMap<String, String>,
        imported_names: &[String],
    ) -> Result<Value, RuntimeError> {
        let user_sub_names = self.collect_eval_user_sub_names();
        match crate::parser::parse_program_with_operators_and_user_subs(
            src,
            op_names,
            op_assoc,
            imported_names,
            &user_sub_names,
        ) {
            Ok((stmts, _)) => {
                self.check_eval_class_redeclarations(&stmts)?;
                self.check_eval_undeclared_trusts(&stmts)?;
                self.check_eval_undeclared_type_args(&stmts)?;
                self.check_eval_undeclared_vars(&stmts)?;
                self.check_eval_undeclared_names(&stmts)?;
                let mut stmts = self.inject_eval_methods_into_class(stmts);
                crate::runtime::phasers::reorder_phasers_for_eval(&mut stmts);
                let phaser_stmts: Vec<Stmt> = stmts
                    .into_iter()
                    .filter(|s| {
                        matches!(
                            s,
                            Stmt::Phaser {
                                kind: PhaserKind::Begin,
                                ..
                            } | Stmt::Phaser {
                                kind: PhaserKind::Check,
                                ..
                            }
                        )
                    })
                    .collect();
                if !phaser_stmts.is_empty() {
                    self.eval_block_value(&phaser_stmts)?;
                }
                Ok(Value::Nil)
            }
            Err(parse_err) => {
                let (partial_stmts, _) = crate::parser::parse_program_partial_with_operators(
                    src,
                    op_names,
                    op_assoc,
                    imported_names,
                );
                self.execute_begin_phasers(&partial_stmts);
                Err(parse_err)
            }
        }
    }

    /// EVAL with :check -- parse, run BEGIN/CHECK phasers, skip main body.
    pub(super) fn eval_eval_string_check_only(
        &mut self,
        code: &str,
    ) -> Result<Value, RuntimeError> {
        let trimmed = code.trim();
        let saved_in_eval = self.env.get("__mutsu_in_eval").cloned();
        self.env
            .insert("__mutsu_in_eval".to_string(), Value::Bool(true));
        let op_names = self.collect_operator_sub_names();
        let op_assoc = self.collect_operator_assoc_map();
        let imported_names = self.collect_eval_imported_function_names();
        let result = self.parse_and_check_only_with_operators(
            trimmed,
            &op_names,
            &op_assoc,
            &imported_names,
        );
        if let Some(saved) = saved_in_eval {
            self.env.insert("__mutsu_in_eval".to_string(), saved);
        } else {
            self.env.remove("__mutsu_in_eval");
        }
        result
    }
}
