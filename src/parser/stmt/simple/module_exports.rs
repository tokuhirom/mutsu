use super::*;
use std::cell::Cell;
use std::rc::Rc;

/// Everything one module-file scan learns that importers need replayed:
/// the `is export` subs, the declared type names (own + transitive), and the
/// declared enum values (own + transitive). Cached per resolved file path so
/// each module file is scan-parsed at most once per process — without the
/// cache a diamond-heavy dependency graph re-parses the same file once per
/// reachable `use` mention (Template::HAML re-read its `X.rakumod` 222 times).
struct ModuleScanResult {
    exports: Vec<InlineModuleExport>,
    type_names: Vec<String>,
    enum_values: Vec<String>,
    /// Sigilless value terms the module declares: `constant SQLT_NUM is export
    /// = 2;`, `my \foo = ...`. Harvested from the scan's own scope stack, the
    /// same way `enum_values` is, because the parser records them as term
    /// symbols while parsing the declaration rather than in the AST node.
    ///
    /// Without these an imported constant looks like an undeclared bareword to
    /// the `when`-matcher gobbled-block check, which then rejects valid code —
    /// DBDish::Oracle::StatementHandle's `when SQLT_NUM { ... }`, whose
    /// `SQLT_NUM` is a `constant ... is export` in the sibling
    /// DBDish::Oracle::Native.
    value_terms: Vec<String>,
    /// EXPORTHOW::DECLARE declarator keywords the module exports, as
    /// `(keyword, HOW type name)` pairs. A `use` of the module makes each
    /// keyword parse as a class-like declarator for the rest of the unit.
    declare_keywords: Vec<(String, String)>,
    /// Whether this module's own scan left the parse-time type index
    /// incomplete (it imported something that could not be resolved and
    /// scanned). Replayed into the importer on a cache hit, exactly like
    /// `type_names`: an importer whose dependency has an unscannable
    /// dependency does not have a complete view either.
    type_index_incomplete: bool,
    /// Whether the module's own source directly `use`s Slangify — the
    /// ADR-0026 gate for parse-time slang activation: a `use` of such a
    /// module must execute it at parse time so its slang registration can
    /// switch parser modes for the rest of the importing unit.
    uses_slangify: bool,
}

thread_local! {
    /// Scan results memoized by resolved module file path. Keyed by path, not
    /// module name, so a `use lib` that changes resolution mid-parse gets a
    /// fresh scan for the newly-resolved file.
    static MODULE_SCAN_CACHE: RefCell<HashMap<String, Rc<ModuleScanResult>>> =
        RefCell::new(HashMap::new());
    /// Bumped whenever the LOADING_MODULES recursion guard suppresses a nested
    /// scan. A scan during which this fired (a `use` cycle) is missing the
    /// cycle partner's transitive contribution, so it must not be cached —
    /// an importer outside the cycle would otherwise be pinned to the
    /// truncated view forever.
    static SCAN_GUARD_SKIPS: Cell<u64> = const { Cell::new(0) };
    /// The outcome of the `use` statement scan that just ran: module name →
    /// does it activate a slang. Written by `register_module_exports` on
    /// every path (including the ones that deliberately do NOT scan), read
    /// once by the parse-time slang hook. The hook must never trigger a scan
    /// of its own: modules the register step skips (native Test/JSON,
    /// pragmas, unresolvable names) would otherwise be file-scanned on every
    /// `use` — Test.rakumod on every test process, ~200ms of debug-build
    /// parsing each, which 5x'd the CI TAP suite before this record existed.
    static LAST_USE_SCAN_ACTIVATES_SLANG: RefCell<Option<(String, bool)>> =
        const { RefCell::new(None) };
    /// Set when this compilation unit imported a module the parser could not
    /// resolve to a file and scan, so the parse-time type index does not
    /// cover every name the unit can legally see. Diagnostics that conclude
    /// "this name is declared nowhere" — see `when_stmt`'s gobbled-block
    /// check — must stay silent while it is set, or they reject valid code
    /// whose type merely came from an unscannable module (an `inst#`
    /// installed repository, a `require`, a module absent from this
    /// environment).
    static TYPE_INDEX_INCOMPLETE: Cell<bool> = const { Cell::new(false) };
}

/// Record that a `use`/`need`/`require` could not be resolved and scanned, so
/// the parse-time type index for this unit is not exhaustive.
pub(crate) fn note_type_index_incomplete() {
    TYPE_INDEX_INCOMPLETE.with(|c| c.set(true));
}

/// Whether the parse-time type index covers every name this unit can see.
pub(crate) fn type_index_is_complete() -> bool {
    TYPE_INDEX_INCOMPLETE.with(|c| !c.get())
}

/// Clear the flag, returning its previous value. Called at parse start
/// (`reset_user_subs`) and around a nested module scan, whose own imports must
/// be attributed to the module rather than leaking into the importer until the
/// scan result is replayed.
pub(crate) fn take_type_index_incomplete() -> bool {
    TYPE_INDEX_INCOMPLETE.with(|c| c.replace(false))
}

/// Modules that resolve to no source file yet leave the type index complete:
/// core pragmas (lowercase by convention) and the handful of uppercase
/// pragma-like names and native providers mutsu implements in Rust. Anything
/// else that fails to resolve is a real module whose types we cannot see.
fn import_is_pragma_like(module: &str) -> bool {
    if module.starts_with(|c: char| !c.is_ascii_uppercase()) {
        return true;
    }
    matches!(
        module,
        "MONKEY" | "MONKEY-SEE-NO-EVAL" | "MONKEY-TYPING" | "MONKEY-GUTS" | "NativeCall"
    )
}

fn record_use_scan_outcome(module: &str, activates: bool) {
    LAST_USE_SCAN_ACTIVATES_SLANG.with(|c| {
        *c.borrow_mut() = Some((module.to_string(), activates));
    });
}

fn note_scan_guard_skip() {
    SCAN_GUARD_SKIPS.with(|c| c.set(c.get() + 1));
}

/// Register exported function names for a module (called when parsing `use` statements).
/// Exports are added to the current (innermost) lexical scope.
///
/// For `Test`, uses a hardcoded list (Test functions are implemented natively in Rust).
/// For all other modules, dynamically scans the module file to extract `is export` subs.
pub(crate) fn register_module_exports(module: &str) {
    record_use_scan_outcome(module, false);
    if module == "Test" {
        let exports: Vec<InlineModuleExport> = TEST_EXPORTS
            .iter()
            .map(|s| InlineModuleExport {
                name: (*s).to_string(),
                precedence: None,
                associativity: None,
                is_test_assertion: false,
            })
            .collect();
        apply_module_exports(&exports);
        return;
    }
    if module == "JSON::Fast" || module == "JSON::Tiny" {
        // Native modules: `to-json`/`from-json` are implemented in Rust
        // (runtime/json.rs), so there is no source file to scan for exports.
        // JSON::Fast also exports the X::JSON::AdditionalContent exception
        // class, and JSON::Tiny its own X::JSON::Tiny::Invalid; register
        // whichever applies as a declared type so `when X::JSON::...Invalid {`
        // is not misread as an undeclared-bareword block gobble.
        if module == "JSON::Fast" {
            register_user_type("X::JSON::AdditionalContent");
        }
        if module == "JSON::Tiny" {
            register_user_type("X::JSON::Tiny::Invalid");
        }
        let exports: Vec<InlineModuleExport> = ["to-json", "from-json"]
            .iter()
            .map(|s| InlineModuleExport {
                name: (*s).to_string(),
                precedence: None,
                associativity: None,
                is_test_assertion: false,
            })
            .collect();
        apply_module_exports(&exports);
        return;
    }
    // Check for infinite recursion
    let already_loading = LOADING_MODULES.with(|m| m.borrow().contains(module));
    if already_loading {
        note_scan_guard_skip();
        return;
    }
    LOADING_MODULES.with(|m| {
        m.borrow_mut().insert(module.to_string());
    });
    let scan = find_and_scan_module(module);
    LOADING_MODULES.with(|m| {
        m.borrow_mut().remove(module);
    });
    if let Some(scan) = scan {
        record_use_scan_outcome(module, scan.uses_slangify);
        apply_scan_types(&scan);
        apply_module_exports(&scan.exports);
        for (keyword, how_type) in &scan.declare_keywords {
            register_declare_keyword(keyword, how_type);
        }
    } else if !import_is_pragma_like(module) {
        note_type_index_incomplete();
    }
}

/// Replay a scan's declared type/enum names into the importer's current scope.
fn apply_scan_types(scan: &ModuleScanResult) {
    // A dependency whose own type index was incomplete makes the importer's
    // incomplete too. Replayed here (not only at scan time) so a cache hit,
    // which skips the nested parse entirely, still propagates it.
    if scan.type_index_incomplete {
        note_type_index_incomplete();
    }
    for name in &scan.type_names {
        // The names are already fully composed; a `use` that appears inside
        // a package block must not compose them a second time.
        register_user_type_verbatim(name);
    }
    // An enum's *values* travel with it. Without this a bare
    // `MYSQL_TYPE_BLOB` in the importing file is an unknown identifier, and
    // the `?? then !!` guard reads it as a listop head that gobbled the
    // `!!` (see `is_user_declared_enum_value`).
    for name in &scan.enum_values {
        register_user_enum_value(name);
    }
    // An exported `constant` is a complete nullary term wherever the importer
    // can see it, exactly like an enum value.
    for name in &scan.value_terms {
        register_imported_value_term(name);
    }
}

/// Register a module's exported subs into the importer's current scope.
fn apply_module_exports(exports: &[InlineModuleExport]) {
    if exports.is_empty() {
        return;
    }
    for export in exports {
        // Register operator subs into user_subs so that the parser's
        // prefix/infix/postfix/circumfix matchers pick them up.
        // An imported `trait_mod:<is>` is what makes a custom parameter trait
        // (`:$x is query`) legal, so the parser has to know it was imported
        // before it decides whether an unknown trait name is an error. It is not
        // an operator sub — it needs none of the precedence/term machinery below.
        if export.name.starts_with("trait_mod:<") {
            register_user_sub(&export.name);
        }
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
        for export in exports {
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
        // An exported `sub term:<foo>` makes a bareword `foo` a call to it.
        // Without registering the term symbol the importer parses `foo` as a
        // plain bareword string (Cro exports `term:<request>`/`term:<response>`).
        || name.starts_with("term:<")
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
/// Scan a module for the type names it declares, without importing its exports.
/// This is what `need Module;` does: the module is loaded — so its `our`-scoped
/// and `package`-installed types become visible — but nothing is imported into
/// the caller's lexical scope. The type registration is a side effect of
/// `extract_exported_names`, so the returned export list is simply discarded.
pub(crate) fn register_module_type_names(module: &str) {
    let already_loading = LOADING_MODULES.with(|m| m.borrow().contains(module));
    if already_loading {
        note_scan_guard_skip();
        return;
    }
    LOADING_MODULES.with(|m| {
        m.borrow_mut().insert(module.to_string());
    });
    let scan = find_and_scan_module(module);
    LOADING_MODULES.with(|m| {
        m.borrow_mut().remove(module);
    });
    if let Some(scan) = scan {
        apply_scan_types(&scan);
    } else if !import_is_pragma_like(module) {
        note_type_index_incomplete();
    }
}

/// Resolve a module name to its source file and scan it, memoized per file
/// path. A cache hit performs no I/O and no parse — the callers replay the
/// stored registrations into their own scope instead.
fn find_and_scan_module(module: &str) -> Option<Rc<ModuleScanResult>> {
    let path = find_module_file(module)?;
    if let Some(hit) = MODULE_SCAN_CACHE.with(|c| c.borrow().get(&path).cloned()) {
        return Some(hit);
    }
    let source = std::fs::read_to_string(&path).ok()?;
    let skips_before = SCAN_GUARD_SKIPS.with(|c| c.get());
    let result = Rc::new(scan_module_source(&source, &path));
    // Only a scan the recursion guard never truncated is complete enough to
    // cache (see SCAN_GUARD_SKIPS).
    if SCAN_GUARD_SKIPS.with(|c| c.get()) == skips_before {
        MODULE_SCAN_CACHE.with(|c| {
            c.borrow_mut().insert(path, Rc::clone(&result));
        });
    }
    Some(result)
}

/// Search lib_paths and program directory for a `.rakumod` / `.pm6` / `.pm` file
/// matching the module name.
fn find_module_file(module: &str) -> Option<String> {
    let base_name = module.replace("::", "/");
    let extensions = [".rakumod", ".pm6", ".pm"];
    // First, search configured lib paths. Iterate path-major (and extension-minor
    // within one path), matching `Interpreter::resolve_module_path`: the parser
    // and the runtime must agree on which file a module is, or the parser can
    // extract exports from one file while the runtime loads another. An `inst#`
    // entry names an installed repository, not a directory; the runtime resolves
    // those through the dist metadata, which this scan does not do yet, so skip
    // them rather than probing a path that can never exist.
    let result = LIB_PATHS.with(|paths| {
        let paths = paths.borrow();
        for base in paths.iter() {
            if base.starts_with("inst#") {
                continue;
            }
            let base_path = std::path::Path::new(base);
            for ext in &extensions {
                let filename = format!("{}{}", base_name, ext);
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
/// Kept as a thin wrapper over `scan_module_source` for unit tests.
#[cfg(test)]
pub(crate) fn extract_exported_names(source: &str) -> Vec<InlineModuleExport> {
    scan_module_source(source, "<test>").exports
}

/// Parse module source and collect its `is export` subs, declared type names,
/// and declared enum values — without registering anything into the caller's
/// scope. Saves and restores the parser's scope state (and package path) so
/// the nested parse cannot clobber the caller's, and so a cache hit (which
/// skips the nested parse entirely) is indistinguishable from a miss.
fn scan_module_source(source: &str, path: &str) -> ModuleScanResult {
    // Save current scopes — parse_program_partial calls reset_user_subs which clears them
    let saved_scopes = SCOPES.with(|s| s.borrow().clone());
    // reset_user_subs also clears the package path; snapshot it too, or a `use`
    // inside a `package Foo { ... }` body would leave the rest of the body
    // composing its declarations against an empty path.
    let saved_package_path = PACKAGE_PATH.with(|p| p.borrow().clone());
    // Save the language version — parsing the module may change it via `use v6.*`
    let saved_language_version = current_language_version();
    // The EXPORTHOW::DECLARE keyword table is unit-scoped state the nested
    // parse's reset would clobber (`use OO::Monitors; monitor Foo {...}`
    // scans the module between the `use` and the declaration). Restored
    // wholesale, so keywords the scanned module itself imports stay lexical
    // to that module.
    let saved_declare_keywords = declare_keywords_snapshot();
    // Slang modes are unit-scoped the same way: the scanned module's slang
    // activation is lexical to that module, and the importer's modes must
    // survive the nested parse's reset (ADR-0026 §2.1 scoping).
    let saved_slang_modes = super::slang_modes_snapshot();
    // This is a scan of `path`, not of whatever file the importer is being
    // parsed from — tag it so any warning this scan raises is attributed to
    // the module, not the importer, and so a later re-parse of the same file
    // (e.g. the run-time module load once the `use` actually executes) can
    // be recognized as re-raising the same warning rather than a new one.
    // See `add_parse_warning` / `todo/tickets/module-parse-warning-reported-twice.md`.
    let saved_source_file = set_parser_source_file(Some(path.to_string()));
    // The nested parse's own unresolvable imports belong to *this* module, not
    // to the importer: park the importer's flag, and hand the module's back
    // through `ModuleScanResult` so `apply_scan_types` replays it on every
    // importer — cache hits included.
    let saved_type_index_incomplete = take_type_index_incomplete();
    let skips_before = super::super::partial_parse_skips();
    let (stmts, _) = crate::parser::parse_program_partial(source);
    // A best-effort parse silently drops every statement it cannot parse — a
    // `class`/`constant` among them. The names in such a statement are missing
    // from this scan, so the importer's view of the module is partial and it
    // must not conclude "declared nowhere" about anything.
    if super::super::partial_parse_skips() != skips_before {
        note_type_index_incomplete();
    }
    let type_index_incomplete = take_type_index_incomplete();
    if saved_type_index_incomplete {
        note_type_index_incomplete();
    }
    set_parser_source_file(saved_source_file);
    // A `package X::Foo { }` block installs its contents into GLOBAL, so the
    // types it declares are visible to whoever loads the module — including
    // through an intermediate module that merely `use`d it. Those transitive
    // names are not in `stmts` (they belong to a module this one used), but the
    // nested parse did register them, so harvest them before the scopes are
    // dropped and re-register them into the importer's scope below.
    let transitive_types: Vec<String> = SCOPES.with(|s| {
        s.borrow()
            .iter()
            .flat_map(|scope| scope.user_types.iter().cloned())
            .filter(|name| name.contains("::"))
            .collect()
    });
    // The same for enum *values* declared by a module this one used: they are
    // complete nullary terms wherever the importer can see them, and unlike type
    // names they are never qualified, so there is nothing to filter on.
    let transitive_enum_values: Vec<String> = SCOPES.with(|s| {
        s.borrow()
            .iter()
            .flat_map(|scope| scope.user_enum_values.iter().cloned())
            .collect()
    });
    // The module's `constant`s (and any it re-exports from a module it used).
    // The parser registers these as `TermBinding::Value` term symbols while
    // parsing the declaration, so — unlike a type name — there is no AST node
    // to walk; the scan's own scope stack is the record. Callable term symbols
    // (`sub term:<foo>`) are excluded: those really can take arguments.
    let mut value_terms: Vec<String> = SCOPES.with(|s| {
        let scopes = s.borrow();
        let own = scopes.iter().flat_map(|scope| {
            scope
                .term_symbols
                .iter()
                .filter(|(_, binding)| matches!(binding, TermBinding::Value(_)))
                .map(|(symbol, _)| symbol.clone())
        });
        let transitive = scopes
            .iter()
            .flat_map(|scope| scope.imported_value_terms.iter().cloned());
        own.chain(transitive).collect()
    });
    // Restore scopes, package path, and language version
    SCOPES.with(|s| {
        *s.borrow_mut() = saved_scopes;
    });
    PACKAGE_PATH.with(|p| {
        *p.borrow_mut() = saved_package_path;
    });
    set_current_language_version(&saved_language_version);
    restore_declare_keywords(saved_declare_keywords);
    super::restore_slang_modes(saved_slang_modes);
    // Collect the module's declared type names (classes/roles/enums/grammars)
    // for the importer's scope. A `use`d module makes its `our`-scoped and
    // exported types visible to the importer, but mutsu loads modules at run
    // time, so without this the parser treats those imported types as
    // undeclared. That in turn misfires heuristics like the `when X::Foo {}`
    // undeclared-exception gobble check (see `given_when::when_stmt`), breaking
    // valid code such as `when X::Zef::UnsatisfiableDependency { ... }` in a
    // file that `use Zef`. Registration (`apply_scan_types`) happens in the
    // caller after this scan returns, so the names land in the importer's
    // current scope, not the module's discarded parse scope.
    let mut type_names: Vec<String> = transitive_types;
    collect_module_type_names(&stmts, &mut type_names);
    let mut enum_values: Vec<String> = transitive_enum_values;
    collect_module_enum_values(&stmts, &mut enum_values);
    collect_module_constant_names(&stmts, &mut value_terms);
    let mut exports: HashMap<String, InlineModuleExport> = HashMap::new();
    collect_exported_subs(&stmts, &mut exports);
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
    let mut declare_keywords = Vec::new();
    collect_exporthow_declare(&stmts, &mut declare_keywords);
    let uses_slangify = stmts.iter().any(|s| {
        matches!(s, Stmt::Use { module, .. }
            if module == "Slangify" || module.starts_with("Slangify:"))
    });
    ModuleScanResult {
        exports: result,
        type_names,
        enum_values,
        value_terms,
        declare_keywords,
        type_index_incomplete,
        uses_slangify,
    }
}

/// ADR-0026 gate: does `module`'s source directly `use` Slangify? A pure
/// lookup of the outcome `register_module_exports` just recorded for this
/// `use` statement — this must NOT trigger a scan of its own (see
/// `LAST_USE_SCAN_ACTIVATES_SLANG`), so modules the register step skips
/// (native providers, pragmas, cycle-guarded or unresolvable names) are
/// simply not slang-activating.
pub(super) fn module_activates_slang(module: &str) -> bool {
    LAST_USE_SCAN_ACTIVATES_SLANG.with(|c| {
        c.borrow()
            .as_ref()
            .is_some_and(|(m, activates)| m == module && *activates)
    })
}

/// Collect `(keyword, HOW type name)` pairs from a scanned module's
/// `my package EXPORTHOW { package DECLARE { constant kw = SomeHOW } }`
/// blocks. A `constant` inside a package parses as an our-scoped VarDecl
/// carrying the `__constant` marker trait, with the HOW type name as a
/// bareword initializer. Descends into non-EXPORTHOW packages so a
/// `unit module Foo;`-wrapped EXPORTHOW block is found too.
fn collect_exporthow_declare(stmts: &[Stmt], out: &mut Vec<(String, String)>) {
    for stmt in stmts {
        let Stmt::Package { name, body, .. } = stmt else {
            continue;
        };
        if name.resolve() != "EXPORTHOW" {
            collect_exporthow_declare(body, out);
            continue;
        }
        for inner in body {
            let Stmt::Package { name, body, .. } = inner else {
                continue;
            };
            if name.resolve() != "DECLARE" {
                continue;
            }
            for decl in body {
                if let Stmt::VarDecl {
                    name,
                    expr,
                    custom_traits,
                    ..
                } = decl
                    && custom_traits.iter().any(|(t, _)| t == "__constant")
                    && let Expr::BareWord(how_type) = expr
                {
                    out.push((name.clone(), how_type.clone()));
                }
            }
        }
    }
}

/// Collect the names of `constant` declarations at any nesting depth.
///
/// The scope-stack harvest in `scan_module_source` only sees constants that are
/// still in scope when the module's parse ends, so a `constant` declared inside
/// a `class`/`role`/`package` body — whose scope has been popped by then — needs
/// the AST walk. Only the bare name is collected: that is the spelling an
/// importer writes for an `is export` constant, and the qualified spelling is
/// matched on its last segment.
fn collect_module_constant_names(stmts: &[Stmt], out: &mut Vec<String>) {
    for stmt in stmts {
        match stmt {
            Stmt::VarDecl {
                name,
                custom_traits,
                ..
            } if custom_traits.iter().any(|(t, _)| t == "__constant") => {
                // Sigiled constants (`constant $x = 1`) are not barewords.
                if !name.is_empty() && !name.starts_with(['$', '@', '%', '&']) {
                    out.push(name.clone());
                }
            }
            Stmt::ClassDecl { body, .. }
            | Stmt::RoleDecl { body, .. }
            | Stmt::Package { body, .. } => collect_module_constant_names(body, out),
            _ => {}
        }
    }
}

/// Recursively collect the names of type declarations (class/role/enum/grammar)
/// found in a parsed module's statement list. Descends into `package`/`module`/
/// `grammar` bodies (whose members are `our`-scoped) so nested type names are
/// captured too. These names are registered into the importer's scope so the
/// parser knows they are declared types rather than undeclared barewords.
fn collect_module_type_names(stmts: &[Stmt], out: &mut Vec<String>) {
    collect_module_type_names_under(stmts, "", out);
}

/// The value names of every `enum` a module declares, at any nesting depth.
///
/// Unlike a type name an enum value is never package-composed here: the
/// importer spells it bare, which is the only spelling the `?? then !!` guard
/// ever sees.
fn collect_module_enum_values(stmts: &[Stmt], out: &mut Vec<String>) {
    for stmt in stmts {
        match stmt {
            Stmt::EnumDecl { variants, .. } => {
                out.extend(
                    variants
                        .iter()
                        .map(|(name, _)| name.clone())
                        .filter(|name| name != "__DYNAMIC__" && !name.is_empty()),
                );
            }
            Stmt::ClassDecl { body, .. }
            | Stmt::RoleDecl { body, .. }
            | Stmt::Package { body, .. } => collect_module_enum_values(body, out),
            _ => {}
        }
    }
}

/// `prefix` is the `::`-joined path of the enclosing package-like declarators.
/// A nested declaration is installed under its composed name, so both spellings
/// are collected: the literal one (visible inside the declaring body) and
/// `<prefix>::<name>` (how the importer must spell it).
///
/// A `unit` declarator (`unit module Foo;`, `unit class Foo;`) parses to a
/// declaration with an *empty* body followed by its contents as siblings, so
/// the prefix it establishes has to be carried forward across the rest of the
/// statement list rather than descended into. Without that, a nested
/// `class Part` under `unit module Cro::HTTP::Body;` is only ever harvested as
/// `MultiPartFormData::Part`, never as the
/// `Cro::HTTP::Body::MultiPartFormData::Part` spelling an importer writes.
fn collect_module_type_names_under(stmts: &[Stmt], prefix: &str, out: &mut Vec<String>) {
    let mut prefix = prefix.to_string();
    for stmt in stmts {
        match stmt {
            Stmt::EnumDecl { name, .. } => {
                let name = name.resolve();
                out.push(compose_type_name(&prefix, &name));
                out.push(name);
            }
            Stmt::ClassDecl {
                name,
                body,
                is_unit,
                ..
            } => {
                let name = name.resolve();
                let composed = compose_type_name(&prefix, &name);
                collect_module_type_names_under(body, &composed, out);
                out.push(composed.clone());
                out.push(name);
                if *is_unit {
                    prefix = composed;
                }
            }
            Stmt::RoleDecl { name, body, .. } => {
                let name = name.resolve();
                let composed = compose_type_name(&prefix, &name);
                collect_module_type_names_under(body, &composed, out);
                out.push(composed);
                out.push(name);
            }
            Stmt::Package {
                name,
                body,
                is_unit,
                ..
            } => {
                // `grammar Foo { ... }` is a Package with kind Grammar; its name
                // is itself a type. `module`/`package` names are namespaces, but
                // registering them is harmless and covers grammar declarations.
                let name = name.resolve();
                // `GLOBAL` is a pseudo-package: `package GLOBAL::X::Foo` installs
                // `X::Foo`, so it must not appear in the composed name.
                let composed =
                    compose_type_name(&prefix, name.strip_prefix("GLOBAL::").unwrap_or(&name));
                collect_module_type_names_under(body, &composed, out);
                out.push(composed.clone());
                out.push(name);
                if *is_unit {
                    prefix = composed;
                }
            }
            _ => {}
        }
    }
}

fn compose_type_name(prefix: &str, name: &str) -> String {
    if prefix.is_empty() {
        name.to_string()
    } else {
        format!("{}::{}", prefix, name)
    }
}

/// Collect `is export` sub/proto declarations from a statement list,
/// recursing into `module`/`package` (and class/role) bodies: exported subs
/// routinely live inside a `module Foo { ... }` block (e.g. Cro::HTTP::Router's
/// `multi route(&route-definition) is export`). The regex fallback misses the
/// bare-`multi` form (no `sub` keyword), so the AST walk must see them.
fn collect_exported_subs(stmts: &[Stmt], exports: &mut HashMap<String, InlineModuleExport>) {
    for stmt in stmts {
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
            Stmt::Package { body, .. }
            | Stmt::ClassDecl { body, .. }
            | Stmt::RoleDecl { body, .. } => {
                collect_exported_subs(body, exports);
            }
            _ => {}
        }
    }
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

/// Functions exported by `use Test`, re-exported from the runtime so the
/// parser and the dispatcher cannot drift apart. Test functions are implemented
/// natively in Rust (`runtime/test_functions/`), not loaded from a `.rakumod`
/// file, so the set has to be spelled out somewhere; that somewhere is
/// `runtime::TEST_MODULE_EXPORTS`.
use crate::runtime::TEST_MODULE_EXPORTS as TEST_EXPORTS;
