use super::*;

/// Register exported function names for a module (called when parsing `use` statements).
/// Exports are added to the current (innermost) lexical scope.
///
/// For `Test`, uses a hardcoded list (Test functions are implemented natively in Rust).
/// For all other modules, dynamically scans the module file to extract `is export` subs.
pub(crate) fn register_module_exports(module: &str) {
    let exports: Vec<InlineModuleExport> = if module == "Test" {
        TEST_EXPORTS
            .iter()
            .map(|s| InlineModuleExport {
                name: (*s).to_string(),
                precedence: None,
                associativity: None,
                is_test_assertion: false,
            })
            .collect()
    } else if module == "JSON::Fast" || module == "JSON::Tiny" {
        // Native modules: `to-json`/`from-json` are implemented in Rust
        // (runtime/json.rs), so there is no source file to scan for exports.
        ["to-json", "from-json"]
            .iter()
            .map(|s| InlineModuleExport {
                name: (*s).to_string(),
                precedence: None,
                associativity: None,
                is_test_assertion: false,
            })
            .collect()
    } else {
        // Check for infinite recursion
        let already_loading = LOADING_MODULES.with(|m| m.borrow().contains(module));
        if already_loading {
            return;
        }
        LOADING_MODULES.with(|m| {
            m.borrow_mut().insert(module.to_string());
        });
        let result = find_and_extract_exports(module);
        LOADING_MODULES.with(|m| {
            m.borrow_mut().remove(module);
        });
        result
    };
    if exports.is_empty() {
        return;
    }
    for export in &exports {
        // Register operator subs into user_subs so that the parser's
        // prefix/infix/postfix/circumfix matchers pick them up.
        if is_operator_sub_name(&export.name) {
            register_user_sub(&export.name);
            register_user_callable_term_symbol(&export.name);
            if let Some(prec) = export.precedence {
                register_op_precedence(&export.name, prec);
            }
            if let Some(assoc) = export.associativity.as_deref() {
                register_user_infix_assoc(&export.name, assoc);
            }
        }
        // Recognize a `is test-assertion` export in the using file's parse so its
        // calls take the same parse path as a locally-declared assertion helper
        // (`known_call_stmt` / `attach_test_callsite_line`, gated on
        // `is_test_assertion_callable`). This routes them through the OTF-compilable
        // dispatch path (§D fallback reduction) and attaches the caller-line
        // marker. (The marker's line value is still subject to the pre-existing
        // ORIGINAL_SOURCE-clobber-on-`use` bug, fixed separately.)
        if export.is_test_assertion {
            register_user_test_assertion_sub(&export.name);
        }
    }
    SCOPES.with(|s| {
        let mut scopes = s.borrow_mut();
        let current = scopes
            .last_mut()
            .expect("scope stack should never be empty");
        for export in &exports {
            current.imported_functions.insert(export.name.clone());
        }
    });
}

fn is_operator_sub_name(name: &str) -> bool {
    name.starts_with("infix:<")
        || name.starts_with("prefix:<")
        || name.starts_with("postfix:<")
        || name.starts_with("circumfix:<")
        || name.starts_with("postcircumfix:<")
}

/// Record exported subs from an inline `module Name { ... }` block.
/// Called after parsing the module body, passing the module name and its exported sub names.
pub(crate) fn register_inline_module_exports(module: &str, exports: Vec<InlineModuleExportSpec>) {
    if exports.is_empty() {
        return;
    }
    let exports = exports
        .into_iter()
        .map(|(name, precedence_trait, associativity)| {
            let precedence = precedence_trait.as_ref().and_then(|(trait_name, ref_op)| {
                resolve_op_precedence(ref_op).map(|ref_level| match trait_name.as_str() {
                    "tighter" => ref_level + 5,
                    "looser" => ref_level - 5,
                    _ => ref_level,
                })
            });
            InlineModuleExport {
                name,
                precedence,
                associativity,
                // Inline `module Foo { ... }` test-assertion subs are registered
                // in scope when their SubDecl is parsed in the same file; the spec
                // tuple does not carry the trait, so default false here.
                is_test_assertion: false,
            }
        })
        .collect();
    INLINE_MODULE_EXPORTS.with(|m| {
        m.borrow_mut().insert(module.to_string(), exports);
    });
}

/// Import exported subs from a previously-parsed inline module into the current scope.
/// Returns true if the inline module was found and its exports were registered.
pub(crate) fn import_inline_module_exports(module: &str) {
    let exports = INLINE_MODULE_EXPORTS.with(|m| m.borrow().get(module).cloned());
    if let Some(exports) = exports {
        for export in &exports {
            register_user_sub(&export.name);
            register_user_callable_term_symbol(&export.name);
            if let Some(precedence) = export.precedence {
                register_op_precedence(&export.name, precedence);
            }
            if let Some(assoc) = export.associativity.as_deref() {
                register_user_infix_assoc(&export.name, assoc);
            }
        }
        // Also register imported functions
        SCOPES.with(|s| {
            let mut scopes = s.borrow_mut();
            let current = scopes
                .last_mut()
                .expect("scope stack should never be empty");
            for export in &exports {
                current.imported_functions.insert(export.name.clone());
            }
        });
    }
}

/// Find a module file and extract its exported function names.
fn find_and_extract_exports(module: &str) -> Vec<InlineModuleExport> {
    let path = find_module_file(module);
    match path {
        Some(p) => {
            if let Ok(source) = std::fs::read_to_string(&p) {
                extract_exported_names(&source)
            } else {
                Vec::new()
            }
        }
        None => Vec::new(),
    }
}

/// Search lib_paths and program directory for a `.rakumod` / `.pm6` / `.pm` file
/// matching the module name.
fn find_module_file(module: &str) -> Option<String> {
    let base_name = module.replace("::", "/");
    let extensions = [".rakumod", ".pm6", ".pm"];
    // First, search configured lib paths
    let result = LIB_PATHS.with(|paths| {
        let paths = paths.borrow();
        for ext in &extensions {
            let filename = format!("{}{}", base_name, ext);
            for base in paths.iter() {
                let base_path = std::path::Path::new(base);
                let candidate = base_path.join(&filename);
                if candidate.exists() {
                    return Some(candidate.to_string_lossy().into_owned());
                }
                // Also check lib/ subdirectory
                let candidate = base_path.join("lib").join(&filename);
                if candidate.exists() {
                    return Some(candidate.to_string_lossy().into_owned());
                }
            }
        }
        None
    });
    if result.is_some() {
        return result;
    }
    // Fall back: search relative to program file (same as runtime's load_module)
    PROGRAM_PATH.with(|pp| {
        let pp = pp.borrow();
        for ext in &extensions {
            let filename = format!("{}{}", base_name, ext);
            if let Some(path) = pp.as_ref()
                && let Some(parent) = std::path::Path::new(path).parent()
            {
                let candidate = parent.join(&filename);
                if candidate.exists() {
                    return Some(candidate.to_string_lossy().into_owned());
                }
            }
            // Last resort: current directory
            let candidate = std::path::Path::new(".").join(&filename);
            if candidate.exists() {
                return Some(candidate.to_string_lossy().into_owned());
            }
        }
        None
    })
}

/// Parse module source and extract names of `is export` sub/proto declarations.
/// Saves and restores the parser's scope state to avoid clobbering the caller's scopes.
pub(crate) fn extract_exported_names(source: &str) -> Vec<InlineModuleExport> {
    // Save current scopes — parse_program_partial calls reset_user_subs which clears them
    let saved_scopes = SCOPES.with(|s| s.borrow().clone());
    // Save the language version — parsing the module may change it via `use v6.*`
    let saved_language_version = current_language_version();
    let (stmts, _) = crate::parser::parse_program_partial(source);
    // Restore scopes and language version
    SCOPES.with(|s| {
        *s.borrow_mut() = saved_scopes;
    });
    set_current_language_version(&saved_language_version);
    let mut exports: HashMap<String, InlineModuleExport> = HashMap::new();
    for stmt in &stmts {
        match stmt {
            Stmt::SubDecl {
                name,
                is_export,
                export_tags,
                associativity,
                precedence_trait,
                is_test_assertion,
                ..
            } if *is_export => {
                // Only include subs that are in the DEFAULT or MANDATORY export tags.
                // Subs tagged only with custom tags (e.g. :others) should not be
                // imported by a plain `use Module`.
                if export_tags
                    .iter()
                    .any(|t| t == "DEFAULT" || t == "MANDATORY")
                {
                    let precedence = precedence_trait.as_ref().and_then(|(trait_name, ref_op)| {
                        resolve_op_precedence(ref_op).map(|ref_level| match trait_name.as_str() {
                            "tighter" => ref_level + 5,
                            "looser" => ref_level - 5,
                            _ => ref_level,
                        })
                    });
                    let resolved = name.resolve();
                    exports.insert(
                        resolved.clone(),
                        InlineModuleExport {
                            name: resolved,
                            precedence,
                            associativity: associativity.clone(),
                            is_test_assertion: *is_test_assertion,
                        },
                    );
                }
            }
            Stmt::ProtoDecl {
                name, is_export, ..
            } if *is_export => {
                // ProtoDecl doesn't carry export_tags; proto declarations with
                // `is export` default to DEFAULT so always include them.
                let resolved = name.resolve();
                exports
                    .entry(resolved.clone())
                    .or_insert(InlineModuleExport {
                        name: resolved,
                        precedence: None,
                        associativity: None,
                        is_test_assertion: false,
                    });
            }
            _ => {}
        }
    }
    // Fallback scan for modules that use syntax not yet fully covered by parse_program_partial.
    // This keeps imported exported-callables discoverable for statement-call parsing.
    for (name, is_test_assertion) in extract_exported_names_fallback(source) {
        exports.entry(name.clone()).or_insert(InlineModuleExport {
            name,
            precedence: None,
            associativity: None,
            is_test_assertion,
        });
    }

    let mut result: Vec<InlineModuleExport> = exports.into_values().collect();
    result.sort_by(|a, b| a.name.cmp(&b.name));
    result
}

fn extract_exported_names_fallback(source: &str) -> Vec<(String, bool)> {
    // `sub foo(...) is export`
    // `multi sub foo(...) is export`
    // `proto sub foo(|) is export`
    // Group 2 captures the declaration text between the name and `is export`,
    // which may include a `is test-assertion` trait; group 3 is the export tag list.
    let sub_re = Regex::new(
        r"\b(?:our\s+)?(?:proto\s+|multi\s+)?sub\s+([A-Za-z_][A-Za-z0-9_'\-]*)\b([^;{]*)\bis\s+export\b(\s*\([^)]*\))?",
    )
    .expect("valid exported-sub regex");
    // `proto foo(|) is export` (without the `sub` keyword)
    let proto_re = Regex::new(
        r"\bproto\s+([A-Za-z_][A-Za-z0-9_'\-]*)\b([^;{]*)\bis\s+export\b(\s*\([^)]*\))?",
    )
    .expect("valid exported-proto regex");

    let test_assertion_re =
        Regex::new(r"\bis\s+test-assertion\b").expect("valid test-assertion regex");

    let mut names: HashMap<String, bool> = HashMap::new();
    for re in [&sub_re, &proto_re] {
        for caps in re.captures_iter(source) {
            if let Some(name) = caps.get(1)
                && is_default_export_from_regex_match_group(&caps, 3)
            {
                let prefix = caps.get(2).map(|m| m.as_str()).unwrap_or("");
                let is_ta = test_assertion_re.is_match(prefix);
                let entry = names.entry(name.as_str().to_string()).or_insert(false);
                *entry = *entry || is_ta;
            }
        }
    }

    let mut names: Vec<(String, bool)> = names.into_iter().collect();
    names.sort();
    names
}

/// Check if an `is export(...)` match should be included in the DEFAULT import set.
/// If no tag list is present (`is export` bare), it's DEFAULT.
/// If a tag list is present, include only if it mentions DEFAULT or MANDATORY.
fn is_default_export_from_regex_match_group(caps: &regex::Captures, group: usize) -> bool {
    match caps.get(group) {
        None => true, // bare `is export` → DEFAULT
        Some(tag_match) => {
            let tag_text = tag_match.as_str();
            tag_text.contains("DEFAULT") || tag_text.contains("MANDATORY")
        }
    }
}

/// Functions exported by `use Test`.
/// Test functions are implemented natively in Rust (`test_functions.rs`),
/// not loaded from a `.rakumod` file, so they must be hardcoded here.
const TEST_EXPORTS: &[&str] = &[
    "ok",
    "nok",
    "is",
    "isnt",
    "is-deeply",
    "is-approx",
    "cmp-ok",
    "like",
    "unlike",
    "isa-ok",
    "does-ok",
    "can-ok",
    "lives-ok",
    "dies-ok",
    "eval-lives-ok",
    "eval-dies-ok",
    "throws-like",
    "fails-like",
    "pass",
    "flunk",
    "skip",
    "skip-rest",
    "todo",
    "diag",
    "plan",
    "done-testing",
    "bail-out",
    "subtest",
    "use-ok",
    "force_todo",
    "force-todo",
    "tap-ok",
];
