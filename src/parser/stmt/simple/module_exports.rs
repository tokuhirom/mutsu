use super::*;
use crate::scan_cache::{self, FileStamp, ScanDep};
use std::cell::Cell;
use std::rc::Rc;

mod decl_scan;
mod dynamic_stash;
mod enum_values;
mod export_hook;
mod source_scan;
use decl_scan::scan_module_decls;
use dynamic_stash::probe_dynamic_exports;
use enum_values::import_admits;
use export_hook::{
    collect_export_hook_literal_keys, collect_export_hook_operator_subs,
    collect_export_hook_value_terms, collect_unit_scope_routines, declares_export_sub,
};

/// Everything one module-file scan learns that importers need replayed:
/// the `is export` subs, the declared type names (own + transitive), and the
/// declared enum values (own + transitive). Cached per resolved file path so
/// each module file is scan-parsed at most once per process — without the
/// cache a diamond-heavy dependency graph re-parses the same file once per
/// reachable `use` mention (Template::HAML re-read its `X.rakumod` 222 times).
#[derive(serde::Serialize, serde::Deserialize)]
struct ModuleScanResult {
    exports: Vec<InlineModuleExport>,
    type_names: Vec<String>,
    /// The subset of `type_names` that are enums. Qualified enum members need
    /// this distinction during parse-time `when` disambiguation.
    #[serde(default)]
    enum_type_names: Vec<String>,
    enum_values: Vec<String>,
    /// Enum values the module exports only under explicit non-default tags
    /// (`my enum Time is export(:Time) < s ms >`), each with those tags. Unlike
    /// `enum_values` these reach an importer's parse only when its `use` names
    /// a matching tag (see `enum_values.rs`).
    #[serde(default)]
    tagged_enum_values: Vec<(String, Vec<String>)>,
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
    /// Whether the module computes its export set with a run-time `sub EXPORT`
    /// hook. Such a hook can install ANY name into the importer's scope, the
    /// CORE term keywords `True`/`False`/`Nil`/`Empty`/`Any` included, and
    /// *which* names is generally not statically knowable (Logic::Ternary
    /// derives them from the `use` arguments). The importer only needs to know
    /// that it happened: the five keywords then compile to a run-time-resolved
    /// term instead of a folded constant, so the hook's installation can win
    /// (#9047). Defaulted rather than required so an on-disk scan cache written
    /// before this field existed still decodes.
    #[serde(default)]
    declares_export_hook: bool,
    /// Whether the module binds into an export stash under a computed key
    /// (`OUR::{'&postfix:<' ~ $code ~ '>'} := ...` in a loop — Moneys). The
    /// names exist only once the module has run, so a `use` of it runs the
    /// module at parse time to learn them (#9500). Defaulted for the same
    /// cache-compatibility reason as `declares_export_hook`.
    #[serde(default)]
    dynamic_export_stash: bool,
    /// `module Foo { sub bar is export }` blocks declared *inside* the scanned
    /// module. The nested parse registers these in the process-wide inline
    /// export table, which — unlike the scopes — the scan does not restore, so
    /// an `import Foo;` in the importing file finds them today purely as a
    /// side effect of the scan having run. Captured here and replayed on every
    /// importer so a cache hit, which runs no nested parse, behaves the same.
    inline_module_exports: Vec<(String, Vec<InlineModuleExport>)>,
    /// Every module this scan resolved, transitively — the invalidation input
    /// the on-disk cache needs because the fields above carry names that came
    /// from those modules. Not part of the serialized payload: the cache
    /// stores it in its own metadata, where it can be checked before the
    /// payload is decoded. See [`crate::scan_cache`].
    #[serde(skip)]
    deps: Vec<crate::scan_cache::ScanDep>,
    /// This module file's own stamp, so an importer can record it as a
    /// dependency without re-reading the file.
    #[serde(skip)]
    stamp: Option<crate::scan_cache::FileStamp>,
}

thread_local! {
    /// Exports of every module the scan in progress `use`d or `need`ed, so a
    /// `sub EXPORT` hook module that re-exports an import
    /// (`Map.new(Other::EXPORT::DEFAULT.WHO.pairs)`) can contribute them to
    /// its own importer's parse. One scan owns the vector at a time:
    /// `scan_module_source` parks the outer one and restores it.
    static NESTED_IMPORT_EXPORTS: RefCell<Vec<InlineModuleExport>> =
        const { RefCell::new(Vec::new()) };
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
    /// of its own: modules the register step skips (`Test`, native JSON,
    /// pragmas, unresolvable names) would otherwise be file-scanned on every
    /// `use` — Test.rakumod on every test process, which 5x'd the CI TAP suite
    /// before this record existed.
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
    /// One frame per module scan currently in progress; each collects the
    /// modules that scan resolved, so the finished scan can be cached on disk
    /// with the dependency list its validity depends on (see
    /// [`crate::scan_cache`]). Empty while the top-level unit is parsed —
    /// nothing caches that, so nothing needs to record it.
    static SCAN_DEP_FRAMES: RefCell<Vec<Vec<ScanDep>>> = const { RefCell::new(Vec::new()) };
}

/// Record that the scan currently in progress resolved `module` to `path`.
/// A module that resolved to nothing is recorded too: it becoming resolvable
/// later changes what the scan would find.
fn record_scan_dep(module: &str, path: Option<&str>, stamp: Option<FileStamp>) {
    push_scan_deps(std::iter::once(ScanDep {
        module: module.to_string(),
        path: path.map(str::to_string),
        stamp,
    }));
}

/// Merge a finished sub-scan's own dependency list into the enclosing frame,
/// so each entry's list is the transitive closure and validating it validates
/// the whole subtree without loading the children's entries.
fn push_scan_deps(deps: impl IntoIterator<Item = ScanDep>) {
    SCAN_DEP_FRAMES.with(|frames| {
        let mut frames = frames.borrow_mut();
        let Some(frame) = frames.last_mut() else {
            return;
        };
        for dep in deps {
            if !frame.contains(&dep) {
                frame.push(dep);
            }
        }
    });
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
        "MONKEY"
            | "MONKEY-SEE-NO-EVAL"
            | "MONKEY-TYPING"
            | "MONKEY-GUTS"
            | "NativeCall"
            | "NativeCall::Types"
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

/// Does a module's source compute its import set in a unit-scope `sub EXPORT`
/// hook (a column-0 `[my|our] sub EXPORT`, Pod and heredocs blanked out)?
/// The names such a `use` imports exist only once the hook has run with the
/// `use`'s arguments, so no source scan can list them (#11062).
// Cost: O(n), n = source length.
pub(crate) fn source_declares_export_hook(source: &str) -> bool {
    source_scan::declares_export_sub(&source_scan::code_text(source))
}

/// Register exported function names for a module (called when parsing `use` statements).
/// Exports are added to the current (innermost) lexical scope.
///
/// For the natively-provided JSON modules, and for `Test`, uses a hardcoded list
/// (see [`TEST_EXPORTS`]). For all other modules, dynamically scans the module
/// file to extract `is export` subs.
pub(crate) fn register_module_exports(module: &str) {
    register_module_exports_with_tags(module, None);
}

/// [`register_module_exports`] for a `use` whose tag list is known to select
/// the module's `is export` traits; see [`apply_scan_types`].
pub(crate) fn register_module_exports_with_tags(module: &str, import_tags: Option<&[String]>) {
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
        // Slangify is the activation helper, not a slang to activate by
        // itself. Its EXPORT implementation contains the same
        // `$*LANG.define_slang` call that L10N modules use, but running it as
        // a zero-argument slang activation would call its four-argument
        // EXPORT with no arguments and fail every Slangify-based module.
        record_use_scan_outcome(module, module != "Slangify" && scan.uses_slangify);
        apply_scan_types(&scan, import_tags);
        note_nested_import_exports(&scan.exports);
        apply_module_exports(&scan.exports);
        if scan.dynamic_export_stash {
            apply_module_exports(&probe_dynamic_exports(module));
        }
        for (keyword, _how_type) in &scan.declare_keywords {
            register_declare_keyword(keyword, false);
        }
    } else if module == "JSON::Fast" {
        // Nothing on the module ladder supplies `JSON::Fast`, so the native
        // provider (runtime/json.rs) will answer it at runtime — there is no
        // source file to scan for exports. Register what it provides: the two
        // routines, plus the `X::JSON::AdditionalContent` exception class it
        // throws, so `when X::JSON::AdditionalContent {` is not misread as an
        // undeclared-bareword block gobble. A real `JSON::Fast` found on the
        // ladder takes the `Some(scan)` arm above instead, like any other
        // module. `JSON::Tiny` needs none of this: it is vendored, so its own
        // source is scanned (#8183).
        register_user_type("X::JSON::AdditionalContent");
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
    } else if !import_is_pragma_like(module) {
        note_type_index_incomplete();
    }
}

/// Replay a scan's declared type/enum names into the importer's current scope.
///
/// `import_tags` is the importing `use`'s tag list when it is known to select
/// the module's `is export` traits (`use M;` → `Some(&[])`, `use M :Time;` →
/// `Some(["Time"])`), and `None` when it is not (`need`, `require`, or a `use`
/// whose positional arguments go to a `sub EXPORT`), in which case every
/// tagged enum value is admitted as before.
fn apply_scan_types(scan: &ModuleScanResult, import_tags: Option<&[String]>) {
    // Replay the scanned module's inline `module Foo { ... is export }` tables
    // so a later `import Foo;` resolves them on a cache hit exactly as it does
    // after a fresh scan.
    if !scan.inline_module_exports.is_empty() {
        INLINE_MODULE_EXPORTS.with(|m| {
            let mut map = m.borrow_mut();
            for (name, exports) in &scan.inline_module_exports {
                map.entry(name.clone()).or_insert_with(|| exports.clone());
            }
        });
    }
    // A dependency whose own type index was incomplete makes the importer's
    // incomplete too. Replayed here (not only at scan time) so a cache hit,
    // which skips the nested parse entirely, still propagates it.
    if scan.type_index_incomplete {
        note_type_index_incomplete();
    }
    for name in &scan.type_names {
        // The names are already fully composed; a `use` that appears inside
        // a package block must not compose them a second time.
        register_imported_type(name);
    }
    for name in &scan.enum_type_names {
        register_imported_enum_type(name);
    }
    // An enum's *values* travel with it. Without this a bare
    // `MYSQL_TYPE_BLOB` in the importing file is an unknown identifier, and
    // the `?? then !!` guard reads it as a listop head that gobbled the
    // `!!` (see `is_user_declared_enum_value`).
    for name in &scan.enum_values {
        register_imported_enum_value(name);
    }
    // A run-time export hook or a computed export stash can import a tagged
    // value whatever tags the `use` names, so only a trait-driven module is
    // filtered.
    let tags_decide = !scan.declares_export_hook && !scan.dynamic_export_stash;
    for (name, export_tags) in &scan.tagged_enum_values {
        let admitted = match import_tags {
            Some(tags) if tags_decide => import_admits(export_tags, tags),
            _ => true,
        };
        if admitted {
            register_imported_enum_value(name);
        }
    }
    // An exported `constant` is a complete nullary term wherever the importer
    // can see it, exactly like an enum value.
    for name in &scan.value_terms {
        register_imported_value_term(name);
    }
    // A module that computes its exports in `sub EXPORT` can install a value
    // under a CORE term keyword's own name, which the parser would otherwise
    // have already folded to a constant (#9047). Replayed here (not only at
    // scan time) so a cache hit, which skips the nested parse entirely, taints
    // the importer the same way — exactly like `type_index_incomplete` above.
    if scan.declares_export_hook {
        note_import_export_hook();
    }
}

/// Remember a nested import's exports for the enclosing scan; see
/// [`NESTED_IMPORT_EXPORTS`].
// Cost: O(e), e = number of exports of the imported module.
fn note_nested_import_exports(exports: &[InlineModuleExport]) {
    if exports.is_empty() {
        return;
    }
    NESTED_IMPORT_EXPORTS.with(|c| c.borrow_mut().extend(exports.iter().cloned()));
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
    let exports: Vec<InlineModuleExport> = exports
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
    // Extend rather than replace: an `augment class` adds to the exports its
    // original declaration registered.
    INLINE_MODULE_EXPORTS.with(|m| {
        m.borrow_mut()
            .entry(module.to_string())
            .or_default()
            .extend(exports);
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
        apply_scan_types(&scan, None);
        note_nested_import_exports(&scan.exports);
    } else if !import_is_pragma_like(module) {
        note_type_index_incomplete();
    }
}

/// Resolve a module name to its source file and scan it, memoized per file
/// path in this process and — since GH-8095 — on disk across processes. Either
/// cache hit performs no parse: the callers replay the stored registrations
/// into their own scope instead.
fn find_and_scan_module(module: &str) -> Option<Rc<ModuleScanResult>> {
    let Some(path) = find_module_file(module) else {
        record_scan_dep(module, None, None);
        return None;
    };
    if let Some(hit) = MODULE_SCAN_CACHE.with(|c| c.borrow().get(&path).cloned()) {
        record_scan_result_as_dep(module, &path, &hit);
        return Some(hit);
    }
    if let Some(hit) = load_scan_from_disk(&path) {
        MODULE_SCAN_CACHE.with(|c| {
            c.borrow_mut().insert(path.clone(), Rc::clone(&hit));
        });
        record_scan_result_as_dep(module, &path, &hit);
        return Some(hit);
    }
    let source = std::fs::read_to_string(&path).ok()?;
    let skips_before = SCAN_GUARD_SKIPS.with(|c| c.get());
    SCAN_DEP_FRAMES.with(|frames| frames.borrow_mut().push(Vec::new()));
    let mut result = scan_module_source(&source, &path);
    result.deps = SCAN_DEP_FRAMES
        .with(|frames| frames.borrow_mut().pop())
        .unwrap_or_default();
    result.stamp = FileStamp::of_source(std::path::Path::new(&path), &source);
    let result = Rc::new(result);
    // Only a scan the recursion guard never truncated is complete enough to
    // cache (see SCAN_GUARD_SKIPS).
    if SCAN_GUARD_SKIPS.with(|c| c.get()) == skips_before {
        MODULE_SCAN_CACHE.with(|c| {
            c.borrow_mut().insert(path.clone(), Rc::clone(&result));
        });
        save_scan_to_disk(&path, &source, &result);
    }
    record_scan_result_as_dep(module, &path, &result);
    Some(result)
}

/// Record a resolved module, and everything its own scan resolved, into the
/// enclosing scan's dependency frame.
fn record_scan_result_as_dep(module: &str, path: &str, result: &ModuleScanResult) {
    record_scan_dep(module, Some(path), result.stamp.clone());
    push_scan_deps(result.deps.iter().cloned());
}

/// Whether the on-disk scan cache may be consulted for the parse in progress.
///
/// An EVAL preseeds the parser with names from its calling unit, and a module
/// scanned under those names can parse differently — so its result is not the
/// pure function of the module sources that the cache key assumes.
fn disk_scan_cache_usable() -> bool {
    !super::eval_preseed_active()
}

fn load_scan_from_disk(path: &str) -> Option<Rc<ModuleScanResult>> {
    if !disk_scan_cache_usable() {
        return None;
    }
    let loaded = scan_cache::load::<ModuleScanResult>(std::path::Path::new(path), &|module| {
        find_module_file(module)
    })?;
    let mut result = loaded.payload;
    result.deps = loaded.deps;
    result.stamp = Some(loaded.stamp);
    Some(Rc::new(result))
}

fn save_scan_to_disk(path: &str, source: &str, result: &ModuleScanResult) {
    if !disk_scan_cache_usable() {
        return;
    }
    // `no precompilation;` opts a module out of the run-time AST cache; honour
    // it for the parse-time scan too, rather than leaving half of the caching
    // on for a module that asked for none.
    if crate::runtime::Interpreter::source_has_no_precompilation(source) {
        return;
    }
    let Some(stamp) = result.stamp.as_ref() else {
        return;
    };
    scan_cache::save(std::path::Path::new(path), result, &result.deps, stamp);
}

/// Search lib_paths for a `.rakumod` / `.pm6` / `.pm` file
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
    // No implicit fallback to the script's directory or the current
    // directory: the runtime searches neither (#11213), and the two must agree.
    LIB_PATHS.with(|paths| {
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
    let saved_l10n_vocabulary = super::l10n_vocabulary_snapshot();
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
    // The inline-export table is process-wide and deliberately not restored
    // (see `ModuleScanResult::inline_module_exports`); note what was already
    // there so the scan's own additions can be told apart and replayed.
    let inline_exports_before: Vec<String> =
        INLINE_MODULE_EXPORTS.with(|m| m.borrow().keys().cloned().collect());
    let skips_before = super::super::partial_parse_skips();
    let saved_nested_exports = NESTED_IMPORT_EXPORTS.with(|c| std::mem::take(&mut *c.borrow_mut()));
    let (stmts, _) = crate::parser::parse_program_partial(source);
    let nested_exports = NESTED_IMPORT_EXPORTS
        .with(|c| std::mem::replace(&mut *c.borrow_mut(), saved_nested_exports));
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
    // The module's enum *values*, as its own parse registered them — its
    // declarations and whatever its own `use`s imported. Imports are lexical
    // and never re-exported, so the imported subset is split off here and
    // dropped below (unless an export hook could re-export it).
    let (scanned_enum_values, imported_enum_values): (Vec<String>, HashSet<String>) =
        SCOPES.with(|s| {
            let scopes = s.borrow();
            (
                scopes
                    .iter()
                    .flat_map(|scope| scope.user_enum_values.iter().cloned())
                    .collect(),
                scopes
                    .iter()
                    .flat_map(|scope| scope.imported_enum_values.iter().cloned())
                    .collect(),
            )
        });
    let transitive_enum_types: Vec<String> = SCOPES.with(|s| {
        s.borrow()
            .iter()
            .flat_map(|scope| scope.user_enum_types.iter().cloned())
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
    super::restore_l10n_vocabulary(saved_l10n_vocabulary);
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
    let decls = scan_module_decls(&stmts);
    let mut type_names: Vec<String> = transitive_types;
    type_names.extend(decls.type_names);
    let mut enum_type_names: Vec<String> = transitive_enum_types;
    enum_type_names.extend(decls.enum_type_names);
    // The source-text fallbacks below see code only: Pod and heredoc bodies
    // are blanked out first.
    let code = source_scan::code_text(source);
    let declares_export_hook =
        declares_export_sub(&stmts) || source_scan::declares_export_sub(&code);
    let mut enum_values: Vec<String> = decls.enum_values;
    let tagged_enum_values: Vec<(String, Vec<String>)> = decls.tagged_enum_values;
    // Keep the scanned values the AST walk cannot see (a computed enum body's
    // names), minus the own tag-restricted ones, which travel only in
    // `tagged_enum_values`, and minus the imported ones: a `use` inside this
    // module does not import anything into *its* importer (ADR-0087's
    // superset stops short of that, because an extra value can shadow a quote
    // construct such as `s///`). A `sub EXPORT` hook can re-export an import,
    // so a hook module keeps them.
    let restricted: HashSet<&str> = tagged_enum_values
        .iter()
        .map(|(name, _)| name.as_str())
        .filter(|name| !enum_values.iter().any(|n| n == name))
        .collect();
    // A package-qualified value (`WS::Msg::Ping`, from `package WS::Msg {
    // enum Opcode is export (...) }` in a module this one used) is not a
    // lexical import: the package is global, so the name resolves in every
    // file that has (transitively) loaded it — Cro::WebSocket's Handler says
    // `when Cro::WebSocket::Message::Ping` having only used the module that
    // used the enum's. Keep those; drop only the bare and enum-type-qualified
    // (`Opcode::Ping`) forms, which do need the import.
    let is_global_package = |pkg: &str| pkg.contains("::") || type_names.iter().any(|t| t == pkg);
    let scanned_enum_values: Vec<String> = scanned_enum_values
        .into_iter()
        .filter(|name| {
            !restricted.contains(name.as_str())
                && (declares_export_hook
                    || !imported_enum_values.contains(name)
                    || name
                        .rsplit_once("::")
                        .is_some_and(|(pkg, _)| is_global_package(pkg)))
        })
        .collect();
    enum_values.extend(scanned_enum_values);
    // The scope-stack harvest above only sees constants still in scope when
    // the module's parse ends; one declared inside a `class`/`role`/`package`
    // body, whose scope has been popped by then, comes from the AST walk.
    value_terms.extend(decls.constant_names);
    let mut exports: HashMap<String, InlineModuleExport> = decls.exports;
    // A module whose exports are computed by a run-time `sub EXPORT` hook has
    // no `is export` traits to find, so the scan above returns nothing at all.
    // Approximate its export set with the routines it declares in its own unit
    // scope, which is what the dominant `UNIT::`-grep idiom exports verbatim
    // (ADR-0087).
    if declares_export_hook {
        collect_unit_scope_routines(&stmts, &mut exports);
        // A third idiom: `is export`-tagged declarations made LOCALLY inside
        // the hook's own body rather than at the module's unit scope
        // (Logic::Ternary's `multi infix:<and3>(...) is export { ... }`,
        // declared inside `sub EXPORT` so it can close over the `use`
        // arguments) are already in `exports`: the declaration scan searches
        // every routine body, and an `is export` routine is exported from any
        // depth. That entry, which carries a custom operator's
        // precedence/associativity, wins over the coarse unit-scope
        // approximation above, which only fills names not yet present.
        // A fourth idiom: operators declared locally in the hook WITHOUT
        // `is export` and handed out through the returned `Map` — see the
        // function's own doc.
        collect_export_hook_operator_subs(&stmts, &mut exports);
        // A sixth idiom: operators and terms named only by a literal pair key
        // of the returned `Map` (`'&term:<today>' => &today`).
        collect_export_hook_literal_keys(&stmts, &mut exports);
        // A second idiom's value terms, declared locally inside the hook's own
        // body rather than drawn from `UNIT::` — see the function's own doc
        // for why a value term (unlike a routine) needs this at all.
        collect_export_hook_value_terms(&stmts, &mut value_terms);
        // A fifth idiom: the hook re-exports what a module it `use`d or
        // `need`ed exports (`Map.new(Other::EXPORT::DEFAULT.WHO.pairs)`,
        // Qwiratry::Query::Slang). The set is only known once that module is
        // scanned, so take it from there; a superset, like the rest.
        for export in nested_exports {
            exports.entry(export.name.clone()).or_insert(export);
        }
        for name in source_scan::unit_scope_routine_names(&code) {
            exports.entry(name.clone()).or_insert(InlineModuleExport {
                name,
                precedence: None,
                associativity: None,
                is_test_assertion: false,
            });
        }
    }
    // Fallback scan for modules that use syntax not yet fully covered by parse_program_partial.
    // This keeps imported exported-callables discoverable for statement-call parsing.
    for (name, is_test_assertion) in source_scan::exported_names(&code) {
        exports.entry(name.clone()).or_insert(InlineModuleExport {
            name,
            precedence: None,
            associativity: None,
            is_test_assertion,
        });
    }

    let mut result: Vec<InlineModuleExport> = exports.into_values().collect();
    result.sort_by(|a, b| a.name.cmp(&b.name));
    let inline_module_exports: Vec<(String, Vec<InlineModuleExport>)> =
        INLINE_MODULE_EXPORTS.with(|m| {
            m.borrow()
                .iter()
                .filter(|(name, _)| !inline_exports_before.contains(name))
                .map(|(name, exports)| (name.clone(), exports.clone()))
                .collect()
        });
    // L10N distributions do not `use Slangify` themselves; their generated
    // EXPORT hook calls `$*LANG.define_slang(...)` (see `decl_scan`).
    let uses_slangify = decls.defines_slang
        || stmts.iter().any(|s| {
            matches!(s, Stmt::Use { module, .. }
            if module == "Slangify" || module.starts_with("Slangify:"))
        });
    ModuleScanResult {
        exports: result,
        type_names,
        enum_type_names,
        enum_values,
        tagged_enum_values,
        value_terms,
        declare_keywords: decls.declare_keywords,
        type_index_incomplete,
        uses_slangify,
        declares_export_hook,
        dynamic_export_stash: decls.dynamic_export_stash,
        inline_module_exports,
        // Filled in by `find_and_scan_module`, which owns the dependency frame
        // and the file stamp.
        deps: Vec::new(),
        stamp: None,
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

fn compose_type_name(prefix: &str, name: &str) -> String {
    if prefix.is_empty() {
        name.to_string()
    } else {
        format!("{}::{}", prefix, name)
    }
}

/// Whether `package` names a module's own export stash directly —
/// `EXPORT::DEFAULT`, `Foo::EXPORT::ALL` — but not a deeper `EXPORT::A::B`.
/// Parse-time twin of `Interpreter::export_stash_tag` (`runtime_module_exports.rs`):
/// the two cannot share code (parser and runtime are different crate
/// modules), but must agree on the naming rule, since a sub this recognises
/// as exported must be one the runtime's `import_module` can actually find.
fn is_export_stash_package(package: &str) -> bool {
    let tag = match package.strip_prefix("EXPORT::") {
        Some(tag) => tag,
        None => match package.split_once("::EXPORT::") {
            Some((_, tag)) => tag,
            None => return false,
        },
    };
    !tag.is_empty() && !tag.contains("::")
}

/// Whether a declaration's custom traits mark it `our`-scoped (the
/// `__our_scoped` marker `my_decl_dispatch.rs` attaches to `our sub`/
/// `our multi sub`).
fn is_our_scoped(custom_traits: &[(String, Option<Expr>)]) -> bool {
    custom_traits.iter().any(|(t, _)| t == "__our_scoped")
}

/// The parser-facing export record of one routine declaration: its name plus
/// a custom operator's precedence (resolved from an `is tighter/looser/equiv`
/// trait against the referenced operator) and associativity.
pub(super) fn sub_export_entry(
    name: String,
    precedence_trait: Option<&(String, String)>,
    associativity: Option<String>,
    is_test_assertion: bool,
) -> InlineModuleExport {
    let precedence = precedence_trait.and_then(|(trait_name, ref_op)| {
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
        is_test_assertion,
    }
}

/// What `use Test` puts in scope, as a parse-time shortcut.
///
/// `Test` is an ordinary bundled module now — `modules/Rakudo-Core/lib/Test.rakumod`,
/// rakudo's own, loaded and run like any other (#7566) — so this list is NOT a
/// native provider's export surface any more. It exists purely for parse speed:
/// `find_and_scan_module` would otherwise parse those 953 lines once per process
/// on top of the runtime's own (precompilation-cached) load, which measured
/// **7 ms -> 93 ms** for `mutsu -e 'use Test; plan 1; ok 1, "x"'`. Every `t/` and
/// roast file pays that, so the list stays.
///
/// It is the scanner's own answer for the vendored file, minus three names the
/// regex-assisted scan picks up spuriously (`sub` and `trait_mod` from
/// declaration syntax, `fail` from a comment). `test_exports_match_the_vendored_module`
/// re-derives it from `Test.rakumod` on every `cargo test`, so bumping the
/// vendored module cannot silently leave this behind.
pub(crate) const TEST_EXPORTS: &[&str] = &[
    "MONKEY-SEE-NO-EVAL",
    "bail-out",
    "can-ok",
    "cmp-ok",
    "diag",
    "dies-ok",
    "does-ok",
    "done-testing",
    "eval-dies-ok",
    "eval-lives-ok",
    "exit-ok",
    "exits-ok",
    "fails-like",
    "flunk",
    "is",
    "is-approx",
    "is-deeply",
    "isa-ok",
    "isnt",
    "like",
    "lives-ok",
    "nok",
    "ok",
    "pass",
    "plan",
    "skip",
    "skip-rest",
    "subtest",
    "throws-like",
    "todo",
    "trait_mod:<is>",
    "unlike",
    "use-ok",
];

#[cfg(test)]
mod test_exports_tests {
    /// Names the module-file scanner reports for any Raku source but that are
    /// not routines: `sub` and `trait_mod` fall out of declaration syntax, and
    /// `fail` out of the "In earlier Perls, this is spelled \"sub fail\"" comment.
    const SCAN_ARTIFACTS: [&str; 3] = ["fail", "sub", "trait_mod"];

    /// [`super::TEST_EXPORTS`] is a hand-maintained copy of what a scan of the
    /// vendored `Test.rakumod` yields. Re-derive it here so a re-vendoring that
    /// adds or drops an export fails the build instead of silently leaving the
    /// parser with a stale view of what `use Test` brings into scope.
    #[test]
    fn test_exports_match_the_vendored_module() {
        let path = concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/modules/Rakudo-Core/lib/Test.rakumod"
        );
        let src = std::fs::read_to_string(path).expect("the vendored Test.rakumod is readable");
        let mut scanned: Vec<String> = super::extract_exported_names(&src)
            .into_iter()
            .map(|e| e.name)
            .filter(|n| !SCAN_ARTIFACTS.contains(&n.as_str()))
            .collect();
        scanned.sort();
        scanned.dedup();
        let mut declared: Vec<String> = super::TEST_EXPORTS.iter().map(|s| s.to_string()).collect();
        declared.sort();
        assert_eq!(
            declared, scanned,
            "TEST_EXPORTS has drifted from modules/Rakudo-Core/lib/Test.rakumod"
        );
    }

    #[test]
    fn slang_activation_scan_uses_ast_not_source_comments() {
        let commented = super::scan_module_source(
            "# $*LANG.define_slang(\"MAIN\", $grammar)\nmy sub exported() { 1 }",
            "<test>",
        );
        assert!(!commented.uses_slangify);

        let registered = super::scan_module_source(
            "my sub EXPORT() { $*LANG.define_slang(\"MAIN\", $grammar) }",
            "<test>",
        );
        assert!(registered.uses_slangify);
    }

    #[test]
    fn dynamic_export_stash_needs_a_computed_key_in_an_export_stash() {
        let computed = super::scan_module_source(
            "my package EXPORT::ALL { for <a b> -> $c { OUR::{'&postfix:<' ~ $c ~ '>'} := sub ($n) { $n } } }",
            "<test>",
        );
        assert!(computed.dynamic_export_stash);

        let literal = super::scan_module_source(
            "my package EXPORT::DEFAULT { OUR::{'&infix:<%%%>'} := sub ($a, $b) { $a } }",
            "<test>",
        );
        assert!(!literal.dynamic_export_stash);

        let not_a_stash = super::scan_module_source(
            "package Foo { for <a> -> $c { OUR::{'&' ~ $c} := sub { 1 } } }",
            "<test>",
        );
        assert!(!not_a_stash.dynamic_export_stash);
    }
}
