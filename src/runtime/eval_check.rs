use super::eval_type_scans::{
    CaptureInheritance, Captures, DeclaredTypes, SubParamTypes, Trusts, TypeArgs, TypeDecls,
    UseLibDirs, scan,
};
use super::*;
use crate::ast::{PhaserKind, Stmt};

/// Type names a `use`d module declares, harvested straight from its source text.
///
/// The parameter-type pre-pass runs *before* the mainline executes, so a `use`
/// has not loaded anything yet and a class the module exports is invisible to
/// the runtime registry — `use URI; sub f(URI $u)` was rejected as an invalid
/// typename. Fully parsing every used module here would duplicate the load the
/// mainline is about to do, so this scans the source for declaration keywords
/// instead. Over-collecting is harmless (the set only ever *widens* what the
/// check accepts, and a genuine typo still will not appear in any module's
/// source); under-collecting just restores the old behaviour.
fn collect_use_declared_type_names(
    interp: &Interpreter,
    module: &str,
    extra_dirs: &[String],
    out: &mut HashSet<String>,
) {
    let path = module_source_in_dirs(module, extra_dirs)
        .or_else(|| interp.resolve_module_path(module).map(|(p, _)| p));
    let Some(path) = path else {
        return;
    };
    let Ok(source) = std::fs::read_to_string(&path) else {
        return;
    };
    const DECLARATORS: [&str; 5] = ["class", "role", "grammar", "enum", "subset"];
    let bytes: Vec<char> = source.chars().collect();
    let is_ident = |c: char| c.is_alphanumeric() || c == '_' || c == '-';
    let mut i = 0usize;
    while i < bytes.len() {
        if i > 0 && is_ident(bytes[i - 1]) {
            i += 1;
            continue;
        }
        let rest: String = bytes[i..bytes.len().min(i + 8)].iter().collect();
        let Some(kw) = DECLARATORS.iter().find(|kw| {
            rest.starts_with(**kw) && !is_ident(*bytes.get(i + kw.len()).unwrap_or(&' '))
        }) else {
            i += 1;
            continue;
        };
        let mut j = i + kw.len();
        while j < bytes.len() && (bytes[j] == ' ' || bytes[j] == '\t') {
            j += 1;
        }
        let start = j;
        while j < bytes.len() && (is_ident(bytes[j]) || bytes[j] == ':') {
            j += 1;
        }
        if j > start {
            let name: String = bytes[start..j].iter().collect();
            if name.starts_with(|c: char| c.is_ascii_uppercase()) {
                out.insert(name);
            }
        }
        i = (i + kw.len()).max(j);
    }
    collect_source_constant_names(&bytes, out);
}

/// Record the `constant NAME = ...;` names a used module's source declares.
///
/// A `constant` bound to a bare type name aliases that type and is usable
/// wherever a type name is — `Gnome::N`'s `constant \GType is export = uint64`,
/// named by `sub g_value_init(N-GValue $value, GType $g_type)`. A `constant`
/// bound to a *value* (`constant G = Point.new(...)`) is usable there too, as a
/// value constraint (`multi f(G)`) — the same line the in-unit collector draws
/// for a `Stmt::VarDecl` constant — so every name is recorded.
///
/// This cannot ride the declarator loop above: the name may be spelled
/// sigillessly (`\GType`) with traits (`is export`) in between, and only a
/// `constant NAME ... =` declaration (not a stray `constant` word) counts.
fn collect_source_constant_names(bytes: &[char], out: &mut HashSet<String>) {
    let is_ident = |c: char| c.is_alphanumeric() || c == '_' || c == '-';
    let mut i = 0usize;
    while i < bytes.len() {
        if (i > 0 && is_ident(bytes[i - 1]))
            || !bytes[i..].starts_with(&['c', 'o', 'n', 's', 't', 'a', 'n', 't'])
            || bytes.get(i + 8).is_some_and(|c| is_ident(*c))
        {
            i += 1;
            continue;
        }
        let mut j = i + 8;
        while bytes.get(j).is_some_and(|c| c.is_whitespace()) {
            j += 1;
        }
        // A sigilless `constant \GType` names the same thing as `constant GType`.
        if bytes.get(j) == Some(&'\\') {
            j += 1;
        }
        let start = j;
        while bytes.get(j).is_some_and(|c| is_ident(*c)) {
            j += 1;
        }
        let name: String = bytes[start..j].iter().collect();
        // Everything between the name and `=` is traits (`is export`); stop at
        // the end of the statement so a runaway scan cannot pair a `constant`
        // with a later statement's `=`.
        while bytes
            .get(j)
            .is_some_and(|c| !matches!(c, '=' | ';' | '\n' | '{'))
        {
            j += 1;
        }
        if bytes.get(j) != Some(&'=') || bytes.get(j + 1) == Some(&'=') {
            i = (i + 8).max(j);
            continue;
        }
        if !name.is_empty() {
            out.insert(name);
        }
        i = (i + 8).max(j);
    }
}

/// Locate `module`'s source under one of `dirs` (or its `lib/` subdirectory).
/// Covers the `use lib '...'` paths, which the runtime has not registered yet
/// when this compile-time pre-pass runs.
fn module_source_in_dirs(module: &str, dirs: &[String]) -> Option<std::path::PathBuf> {
    let base = module.replace("::", "/");
    for dir in dirs {
        for ext in [".rakumod", ".pm6", ".pm"] {
            let name = format!("{}{}", base, ext);
            let root = std::path::Path::new(dir);
            for candidate in [root.join(&name), root.join("lib").join(&name)] {
                if candidate.is_file() {
                    return Some(candidate);
                }
            }
        }
    }
    None
}

impl Interpreter {
    /// Reject inheriting from a type capture in scope
    /// (`-> ::T { class C is T {} }`) at compile time, like rakudo.
    // Cost: O(n * k), n = size of the unit's AST, k = type captures in scope.
    pub(crate) fn check_type_capture_inheritance(
        &self,
        stmts: &[Stmt],
    ) -> Result<(), RuntimeError> {
        match scan(CaptureInheritance::default(), stmts).error {
            Some(e) => Err(e),
            None => Ok(()),
        }
    }

    /// Every type name this unit declares, plus those its `use`d modules
    /// declare (found through the unit's own `use lib` paths too).
    // Cost: O(n + m), n = size of the unit's AST, m = size of the used
    // modules' sources.
    pub(super) fn eval_declared_types(&self, stmts: &[Stmt]) -> DeclaredTypes {
        let file = self.env.get("?FILE").map(|v| v.to_string_value());
        let lib_dirs = scan(
            UseLibDirs {
                file: file.as_deref(),
                program: self.program_path.as_deref(),
                out: Vec::new(),
            },
            stmts,
        )
        .out;
        let harvest = |module: &str, out: &mut HashSet<String>| {
            collect_use_declared_type_names(self, module, &lib_dirs, out)
        };
        scan(
            TypeDecls {
                harvest: Some(&harvest),
                out: DeclaredTypes::default(),
            },
            stmts,
        )
        .out
    }

    /// Reject sub parameter types that name a type unknown to this compilation
    /// unit (e.g. `sub yoink(Junctoin $barf)`) -> X::Parameter::InvalidType.
    // Cost: O(n + m), as `eval_declared_types`, plus one validation per sub.
    pub(crate) fn check_eval_param_type_constraints(
        &self,
        stmts: &[Stmt],
    ) -> Result<(), RuntimeError> {
        let declared = self.eval_declared_types(stmts);
        let checker = SubParamTypes {
            interp: self,
            declared: &declared,
            captures: Captures::default(),
            error: None,
        };
        match scan(checker, stmts).error {
            Some(e) => Err(e),
            None => Ok(()),
        }
    }

    /// The type names this unit declares, without harvesting `use`d modules.
    fn unit_declared_types(stmts: &[Stmt]) -> DeclaredTypes {
        scan(
            TypeDecls {
                harvest: None,
                out: DeclaredTypes::default(),
            },
            stmts,
        )
        .out
    }

    /// Reject a type-parameter argument that names an undeclared type, e.g.
    /// `my Array[Numerix] $x` -> X::Undeclared::Symbols (gist mentions `Numerix`).
    /// Only the inner `[...]` arguments are checked here (the base type's
    /// parametric-ness is X::NotParametric, handled elsewhere). All declared type
    /// names are collected first so forward references are honored.
    // Cost: O(n * k), n = size of the unit's AST, k = type captures in scope.
    pub(crate) fn check_eval_undeclared_type_args(
        &self,
        stmts: &[Stmt],
    ) -> Result<(), RuntimeError> {
        let declared = Self::unit_declared_types(stmts).types;
        if let Some(name) = scan(TypeArgs::new(self, &declared), stmts).found {
            let suggestions = self.suggest_type_names(&name);
            return Err(RuntimeError::undeclared_type_symbols(
                &name,
                format!("Undeclared name:\n    {} used at line 1", name),
                suggestions,
            ));
        }
        Ok(())
    }

    /// If `tc` is `Base[arg, ...]`, return the first inner argument that names an
    /// undeclared type (an uppercase bareword that is neither declared nor a known
    /// built-in / resolvable type). `::T` capture args and lowercase/native args
    /// are ignored.
    pub(super) fn first_undeclared_type_arg(
        &self,
        tc: &str,
        declared: &HashSet<String>,
        captures: &Captures,
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
            if arg.is_empty() || arg.starts_with("::") || captures.contains(arg) {
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

    /// Reject a `trusts T` declaration whose target type `T` is not declared
    /// anywhere in this compilation unit (nor a known built-in type)
    /// -> X::Undeclared (symbol => T, what => "Type"). Forward references are
    /// honored because all declared type names are collected first.
    // Cost: O(n), n = size of the unit's AST.
    pub(crate) fn check_eval_undeclared_trusts(&self, stmts: &[Stmt]) -> Result<(), RuntimeError> {
        let declared = Self::unit_declared_types(stmts).types;
        let checker = Trusts {
            interp: self,
            declared: &declared,
            found: None,
        };
        if let Some(target) = scan(checker, stmts).found {
            let mut attrs = ValueMap::default();
            attrs.insert("symbol".to_string(), Value::str(target.clone()));
            attrs.insert("what".to_string(), Value::str("Type".to_string()));
            attrs.insert(
                "message".to_string(),
                Value::str(format!("Type '{}' is not declared", target)),
            );
            return Err(RuntimeError::typed("X::Undeclared", attrs));
        }
        Ok(())
    }

    /// Parse and run only BEGIN/CHECK phasers from EVAL'd code (`:check` mode).
    fn parse_and_check_only_with_operators(
        &mut self,
        src: &str,
        op_names: &[String],
        op_assoc: &HashMap<String, String>,
    ) -> Result<Value, RuntimeError> {
        let user_sub_names = self.collect_eval_user_sub_names();
        let user_type_names = self.collect_eval_user_type_names();
        let user_value_term_names = self.collect_eval_user_value_term_names();
        match crate::parser::parse_program_with_operators_and_user_subs(
            src,
            op_names,
            op_assoc,
            &user_sub_names,
            &user_type_names,
            &user_value_term_names,
        ) {
            Ok((stmts, _)) => {
                self.check_eval_class_redeclarations(&stmts)?;
                self.check_eval_undeclared_trusts(&stmts)?;
                self.check_eval_undeclared_type_args(&stmts)?;
                self.check_eval_undeclared_vars(&stmts)?;
                self.check_eval_undeclared_names(&stmts)?;
                self.check_eval_undeclared_routines(&stmts)?;
                self.check_eval_post_declared_types(&stmts)?;
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
                Ok(Value::NIL)
            }
            Err(parse_err) => {
                let (partial_stmts, _) =
                    crate::parser::parse_program_partial_with_operators(src, op_names, op_assoc);
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
        self.env.insert("__mutsu_in_eval".to_string(), Value::TRUE);
        let op_names = self.collect_operator_sub_names();
        let op_assoc = self.collect_operator_assoc_map();
        let result = self.parse_and_check_only_with_operators(trimmed, &op_names, &op_assoc);
        if let Some(saved) = saved_in_eval {
            self.env.insert("__mutsu_in_eval".to_string(), saved);
        } else {
            self.env.remove("__mutsu_in_eval");
        }
        result
    }
}
