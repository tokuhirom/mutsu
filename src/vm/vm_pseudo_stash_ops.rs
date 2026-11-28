//! Pseudo-stash reads: `Pkg::`, `OUTER::`, `MY::`, `DYNAMIC::`, `CALLER::`, and the
//! fused one-key read `Pkg::<$x>` (`GetPseudoStashKeyed`).
use super::*;
use crate::opcode::{LEXICAL_STASH_ROUTINE_SLOT, LexicalStashRoutines};
use crate::value::ValueMap;

#[path = "vm_pseudo_stash_lexical.rs"]
mod lexical;

/// The pseudo-packages the root stash `::` names.
const PSEUDO_PACKAGES: &[&str] = &[
    "MY", "OUR", "CORE", "GLOBAL", "PROCESS", "DYNAMIC", "CALLER", "CALLERS", "OUTER", "OUTERS",
    "LEXICAL", "UNIT", "SETTING", "CLIENT",
];

impl Interpreter {
    // Cost: O(v), v = entries of the whole env (plus `our_vars` for GLOBAL, locals for MY::),
    // for a pseudo-package stash: it materializes a fresh map by scanning them all. A named
    // package's stash is O(k + t), k = interned names under it, t = registered types, plus
    // each routine member's code value (see `package_stash_value`). Rakudo: O(1) -- see #9171.
    pub(super) fn exec_get_pseudo_stash_op(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
    ) -> Result<(), RuntimeError> {
        let name = Self::const_str(code, name_idx);
        if let Some(depth) = Self::caller_stash_depth(name) {
            let origin = self.caller_frame_package();
            let origin_routine = self.caller_frame_enclosing_routine();
            let stash = self.caller_stash_value(name, depth);
            Self::stamp_stash_origin_package(&stash, &origin);
            Self::stamp_stash_origin_routine(&stash, origin_routine.as_deref());
            Self::stamp_stash_origin_unit(&stash, self.caller_frame_unit());
            if let Some(index) = self.callframe_stack_len().checked_sub(depth) {
                Self::stamp_stash_caller_frame(&stash, index);
            }
            self.stack.push(stash);
            return Ok(());
        }
        if let Some(depth) = Self::caller_lexical_stash_depth(name) {
            let entries = self.caller_lexical_stash_entries(depth);
            let stash = self.pseudo_stash_hash(entries);
            self.stack.push(stash);
            return Ok(());
        }
        if name == "OUTER::" {
            // OUTER:: is lexical, not package-based. Expose captured lexical vars
            // from the current interpreter environment as stash entries.
            let mut entries: ValueMap = ValueMap::default();
            for (key, val) in self.env().iter() {
                let key_str = key.resolve();
                if self.should_hide_from_my_global_stash(&key_str) {
                    continue;
                }
                let display_key = Self::add_sigil_prefix(&key_str);
                entries.insert(display_key, val.clone());
            }
            let stash = self.pseudo_stash_hash(entries);
            self.stack.push(stash);
            return Ok(());
        }
        if name == "OUR::" {
            let stash = self.our_pseudo_stash();
            self.stack.push(stash);
            return Ok(());
        }
        if name == "DYNAMIC::" {
            let mut entries = self.dynamic_pseudo_stash_entries();
            // 6.e: a `$*x` falls back to `GLOBAL::<$x>` as well as PROCESS.
            if crate::parser::current_language_version_starts_with("6.e") {
                for (key, value) in self.global_scalar_dynamics() {
                    entries.entry(key).or_insert(value);
                }
            }
            let stash = self.pseudo_stash_hash_without_topic(entries);
            self.stack.push(stash);
            return Ok(());
        }
        if name == "CALLERS::" {
            let kind = "CALLERS";
            // `CALLER::` is only useful as an `EVAL` context here, and that use
            // needs the package of the frame it was taken from — which is gone
            // by the time EVAL runs. Record it on the value itself.
            let origin = self.caller_frame_package();
            let origin_routine = self.caller_frame_enclosing_routine();
            let stash = loan_env!(self, package_stash_value(kind));
            Self::stamp_stash_origin_package(&stash, &origin);
            Self::stamp_stash_origin_routine(&stash, origin_routine.as_deref());
            Self::stamp_stash_origin_unit(&stash, self.caller_frame_unit());
            self.stack.push(stash);
            return Ok(());
        }
        if let Some(target) = self.named_pseudo_stash_target(name) {
            let stash = loan_env!(self, package_stash_value(&target));
            self.stack.push(stash);
            return Ok(());
        }

        // MY:: pseudo-stash: collect all variable names from current scope.
        let mut entries: ValueMap = ValueMap::default();
        for (i, var_name) in code.locals.iter().enumerate() {
            let val = self.locals[i].clone();
            let key = Self::add_sigil_prefix(var_name);
            entries.insert(key, val);
        }
        for (key, val) in self.env().iter() {
            let key_str = key.resolve();
            if self.should_hide_from_my_global_stash(&key_str)
                || self.module_name_hidden_here(&key_str)
            {
                continue;
            }
            let display_key = Self::add_sigil_prefix(&key_str);
            entries.entry(display_key).or_insert_with(|| val.clone());
        }
        self.add_visible_routines_to_pseudo_stash(&mut entries);
        self.add_merged_module_packages(self.modules_merged_in_view(), &mut entries);
        if name == "::" {
            self.add_all_imports_to_pseudo_stash(&mut entries);
            // The root stash answers each pseudo-package by name
            // (`::<OUTERS>:exists`, `::.<MY>.WHO.<$l>`).
            // TODO: rakudo's root PseudoStash answers these in AT-KEY /
            // EXISTS-KEY without listing them in `.keys`; this snapshot
            // hash has no separate key view, so it lists them too.
            for pseudo in PSEUDO_PACKAGES {
                entries
                    .entry((*pseudo).to_string())
                    .or_insert_with(|| Value::package(Symbol::intern(pseudo)));
            }
        }
        if name == "UNIT::" {
            for (display, value) in self.unit_lexical_types() {
                entries.entry(display).or_insert(value);
            }
            // The unit's own names live at the chain's tail, below every frame
            // the read runs in: a module's `sub EXPORT` runs over the module
            // scope, and a closure inside it over that frame. Iterating the
            // running env sees its overlay only, so a `my constant &name` of
            // the unit was missing (#13272).
            let env = self.env();
            // A module loaded by a block-scoped `use` runs over the block's
            // scoped env: its own lexicals sit in a tier above the chain's
            // tail, which is the importer's program scope (#14115). A
            // module's `UNIT::` reads every tier of its chain instead.
            // Tree EVAL retains its caller's routine-resolution unit, but its
            // UNIT:: reads the tree's own lexical tiers after an import.
            let module_unit = self.current_unit != crate::runtime::main_unit()
                || crate::unit_source_file::tree_eval_active();
            let module_view = module_unit.then(|| env.flattened());
            if env.is_scoped() {
                let tail = module_view.as_ref().unwrap_or_else(|| env.chain_tail());
                for (key, val) in tail.iter() {
                    let key_str = key.resolve();
                    if self.should_hide_from_my_global_stash(&key_str) {
                        continue;
                    }
                    entries
                        .entry(Self::add_sigil_prefix(&key_str))
                        .or_insert_with(|| val.clone());
                }
            }
        }
        let stash = self.pseudo_stash_hash(entries);
        self.stack.push(stash);
        Ok(())
    }

    /// The package a `Name::` pseudo-stash read names, when it is an ordinary
    /// package stash (not `CALLER::`, `OUTER::`, `OUR::`, `DYNAMIC::`,
    /// `CALLERS::`, `MY::`, `LEXICAL::` or `UNIT::`, which have their own
    /// builders in [`Self::exec_get_pseudo_stash_op`]).
    // Cost: O(m), m = bytes of the name, plus one env probe.
    fn named_pseudo_stash_target(&self, name: &str) -> Option<String> {
        if Self::caller_stash_depth(name).is_some()
            || Self::caller_lexical_stash_depth(name).is_some()
        {
            return None;
        }
        // `CALLER::OUR::` is the package stash of the caller frame's package
        // (P5reset's `reset` walks `CALLER::OUR::.kv` to clear the caller's
        // `our` variables), not a package literally named `CALLER::OUR`.
        if let Some(depth) = Self::caller_our_stash_depth(name) {
            return Some(self.caller_frame_package_at(depth));
        }
        // `Name::` is the stash spelling: the qualifier with an empty last
        // segment.
        if let Some((package, _)) = crate::qualified::split_qualified(Symbol::intern(name))
            .filter(|(_, tail)| tail.as_str().is_empty())
            && let package = package.as_str()
            && !matches!(package, "OUTER" | "OUR" | "DYNAMIC" | "CALLERS")
            && package != "MY"
            && package != "LEXICAL"
            // `UNIT::` is a LEXICAL pseudo-package — the compilation unit's
            // outermost pad — not a package called "UNIT". Reading it as one
            // handed back an empty stash, so `UNIT::.grep: { .key.starts-with('&') }`
            // (the shape a module's `sub EXPORT` uses to export everything it
            // declared: String::Utils, Array::Sorted::Util, ...) iterated the
            // stash object itself and died on `.key`.
            //
            // TODO: this shares the `MY::` pad below, which inside a routine
            // over-reports — it adds that routine's own locals, where rakudo's
            // `UNIT::` shows only the unit's. Answering it exactly needs the
            // compiler to keep a per-unit symbol table rather than deriving the
            // pad from the running frame.
            && package != "UNIT"
            && !package.is_empty()
        {
            // A lexical bound to a *type object* names that package:
            // `my \NCexports = ::('NativeCall::EXPORT::ALL'); NCexports::{$_}`
            // must read the bound package's stash, not a package literally
            // called "NCexports". Only a `Package` value redirects — any other
            // binding leaves the literal-name reading intact.
            // A sigilless binding lives under its term key (ADR-12441).
            let target = self
                .term_binding(package)
                .or_else(|| self.get_env_with_main_alias(package))
                // A binding a reflective closure shares is held in a cell
                // (ADR-12529 phase 3); the package is what the cell holds.
                .and_then(|v| match v.deref_container().view() {
                    ValueView::Package(sym) => Some(
                        self.resolve_type_in_current_package(&sym.resolve())
                            .unwrap_or_else(|| sym.resolve()),
                    ),
                    _ => None,
                })
                // `tmp::Foo::` where `tmp` is a lexical package bound to its
                // storage name (`tmp\0<id>`, #17477).
                .or_else(|| self.resolve_package_alias_prefix(package))
                .unwrap_or_else(|| package.to_string());
            return Some(target);
        }
        None
    }

    /// `Name::<key>` / `Name::{$key}`: the pseudo-stash read fused with its
    /// one-key subscript. An ordinary package answers the key alone
    /// (`package_stash_keyed_value`) instead of materializing its whole stash;
    /// anything else builds the stash exactly as `GetPseudoStash` does. Either
    /// way the ordinary `Index` performs the read, so the result is the same.
    // Cost: O(k) for a sigiled key of an ordinary package, k = interned qualified
    // names ending in the key's bare name (`qualified_tail_index`); O(1) for a
    // `$`/`@`/`%` key of `GLOBAL::` (`global_stash_variable`); otherwise as
    // `GetPseudoStash` plus `Index`, O(v), v = env entries. Rakudo: O(1) -- see #9171.
    pub(super) fn exec_get_pseudo_stash_keyed_op(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
    ) -> Result<(), RuntimeError> {
        let key = self.stack.pop().unwrap_or(Value::NIL);
        self.push_pseudo_stash_for_key(code, name_idx, &key)?;
        self.stack.push(key);
        self.exec_index_op_with_positional(false)
    }

    /// Push the stash `Name::` names, as far as a one-key subscript by `key`
    /// can observe it: an ordinary package's one-entry stash when the key can
    /// be answered alone (`package_stash_keyed_value`), else the whole stash
    /// `GetPseudoStash` builds.
    // Cost: as `GetPseudoStashKeyed`.
    pub(super) fn push_pseudo_stash_for_key(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
        key: &Value,
    ) -> Result<(), RuntimeError> {
        let name = Self::const_str(code, name_idx);
        // `DYNAMIC::<key>` walks the dynamic scope for the one key (#13590).
        if name == "DYNAMIC::"
            && let ValueView::Str(key_str) = key.deref_container().view()
        {
            self.push_dynamic_stash_for_key(code, &key_str);
            return Ok(());
        }
        // `CALLERS::OUTERS::<&name>`: a routine in the lexical scope around a
        // calling *routine* (declared in its file or imported by its unit).
        // `CLIENT::LEXICAL::<CORE-SETTING-REV>` / `CLIENT::CORE::<…>` /
        // `CLIENT::LEXICAL::<$?PACKAGE>`: the language revision and package of
        // the code that called into this routine's package (`client_frame`).
        // TODO: any other `CLIENT::` key needs the client frame's whole pad,
        // which the caller-scope walk (`caller_scope(depth)`, #17733) is to give.
        if name.starts_with("CLIENT::")
            && let ValueView::Str(key_str) = key.deref_container().view()
            && matches!(key_str.as_str(), "CORE-SETTING-REV" | "$?PACKAGE")
        {
            let (package, revision) = self
                .client_code(code)
                .unwrap_or_else(|| self.client_frame(code.source_file));
            let value = if key_str.as_str() == "$?PACKAGE" {
                Value::package(package)
            } else {
                Value::str(revision.to_string())
            };
            let mut entries: ValueMap = ValueMap::default();
            entries.insert(key_str.to_string(), value);
            let stash = self.pseudo_stash_hash(entries);
            self.stack.push(stash);
            return Ok(());
        }
        if name == "CALLERS::OUTERS::"
            && let ValueView::Str(key_str) = key.deref_container().view()
            && let Some(routine) = key_str.strip_prefix('&')
        {
            let found = self.caller_scope_routine(routine, true);
            let mut entries: ValueMap = ValueMap::default();
            if let Some(value) = found {
                entries.insert(key_str.to_string(), value);
            }
            let stash = self.pseudo_stash_hash(entries);
            self.stack.push(stash);
            return Ok(());
        }
        let keyed = match key.deref_container().view() {
            ValueView::Str(key_str) => self.named_pseudo_stash_target(name).and_then(|target| {
                loan_env!(self, package_stash_keyed_value(&target, &key_str))
                    .map(|(stash, _found)| stash)
            }),
            _ => None,
        };
        match keyed {
            Some(stash) => {
                self.stack.push(stash);
                Ok(())
            }
            None => {
                self.exec_get_pseudo_stash_op(code, name_idx)?;
                // `::<M>` inside a routine: `M` is a package of the enclosing
                // scopes, which the routine's own frame does not hold.
                if name == "::"
                    && let ValueView::Str(key_str) = key.deref_container().view()
                    && !key_str.is_empty()
                    && !key_str.starts_with(['$', '@', '%', '&'])
                    && self.is_known_package(&key_str)
                    && let Some(top) = self.stack.pop()
                {
                    let mut entries: ValueMap = ValueMap::default();
                    if let ValueView::Hash(map, ..) = top.view() {
                        for (k, v) in map.iter() {
                            entries.insert(k.clone(), v.clone());
                        }
                    }
                    entries
                        .entry(key_str.to_string())
                        .or_insert_with(|| Value::package(Symbol::intern(&key_str)));
                    let stash = self.pseudo_stash_hash(entries);
                    self.stack.push(stash);
                }
                Ok(())
            }
        }
    }

    /// Wrap a lexical-pad snapshot as a `PseudoStash`.
    ///
    /// Raku reports every pseudo-package view of a pad (`MY::`, `OUTER::`,
    /// `LEXICAL::`, `DYNAMIC::`) as `PseudoStash`, a `Map` descendant that is a
    /// *sibling* of `Stash`, not a subclass. mutsu keeps the snapshot as an
    /// ordinary hash — its `.keys`, `.{...}` and iteration all ride the Hash
    /// paths — so the type travels as the hash's declared type, exactly the way
    /// a `Map` does. Without this the spellings answered a bare `Hash`, with no
    /// symbol-table type at all.
    pub(super) fn pseudo_stash_hash(&mut self, mut entries: ValueMap) -> Value {
        // `$_` is a declared lexical of every scope, `Any` until something
        // topicalizes it, so a lexical stash always answers it.
        if entries.get("$_").is_none_or(Value::is_nil) {
            entries.insert("$_".to_string(), Value::package(Symbol::intern("Any")));
        }
        self.pseudo_stash_hash_without_topic(entries)
    }

    /// [`Self::pseudo_stash_hash`] for a stash that is no lexical pad: the
    /// dynamic chain (`DYNAMIC::`) lists only what binds dynamically.
    // Cost: O(n), n = entries.
    pub(super) fn pseudo_stash_hash_without_topic(&mut self, entries: ValueMap) -> Value {
        let mut data = crate::value::HashData::new(entries);
        // The running code's revision picks the stash's class (ADR-17287
        // keeps the parser's revision that of the running routine's unit).
        data.pseudo_stash_6e = crate::parser::current_language_version_starts_with("6.e");
        let hash = Value::hash_with_data(Value::hash_arc(data));
        self.tag_container_metadata(
            hash,
            crate::runtime::ContainerTypeInfo {
                value_type: String::new(),
                key_type: None,
                declared_type: Some("PseudoStash".to_string()),
            },
        )
    }

    /// Build a pseudo-stash hash for a given pseudo-package name.
    /// Used by .WHO dispatch on pseudo-package Package values.
    pub(super) fn build_pseudo_stash(&mut self, code: &CompiledCode, name: &str) -> Value {
        if name == "OUTER" {
            let mut entries: ValueMap = ValueMap::default();
            for (key, val) in self.env().iter() {
                let key_str = key.resolve();
                if self.should_hide_from_my_global_stash(&key_str) {
                    continue;
                }
                let display_key = Self::add_sigil_prefix(&key_str);
                entries.insert(display_key, val.clone());
            }
            return self.pseudo_stash_hash(entries);
        }
        if name == "OUR" {
            return self.our_pseudo_stash();
        }
        if name != "MY" && name != "LEXICAL" {
            return loan_env!(self, package_stash_value(name));
        }
        // MY / LEXICAL: collect locals + env
        let mut entries: ValueMap = ValueMap::default();
        for (i, var_name) in code.locals.iter().enumerate() {
            let val = self.locals[i].clone();
            let key = Self::add_sigil_prefix(var_name);
            entries.insert(key, val);
        }
        for (key, val) in self.env().iter() {
            let key_str = key.resolve();
            if self.should_hide_from_my_global_stash(&key_str)
                || self.module_name_hidden_here(&key_str)
            {
                continue;
            }
            let display_key = Self::add_sigil_prefix(&key_str);
            entries.entry(display_key).or_insert_with(|| val.clone());
        }
        self.add_visible_routines_to_pseudo_stash(&mut entries);
        self.add_merged_module_packages(self.modules_merged_in_view(), &mut entries);
        self.pseudo_stash_hash(entries)
    }

    /// The value of the term `name` the current scope holds: a sigilless
    /// binding, an enum key read by its bare spelling, or a constant in the
    /// environment.
    // Cost: O(1) expected, two lookups.
    pub(super) fn imported_term(&self, name: &str) -> Option<Value> {
        self.term_binding(name)
            .or_else(|| self.enum_bare_value(name))
            .or_else(|| {
                self.env()
                    .get(name)
                    .cloned()
                    .filter(|v| !matches!(v.view(), ValueView::Package(_) | ValueView::Sub(..)))
            })
    }

    /// Add every import alias the current view holds (`env_imports_here`) to
    /// `entries`: an imported term or type need not sit in env under its
    /// import key (a sigilless constant lives under its term key, an exported
    /// type resolves through the type registry, a `sub EXPORT` hash installs
    /// a qualified key), so each is read the way a bare-word read of the name
    /// does. The bare `::` stash lists these like `MY::` does (#16555).
    // Cost: O(i), i = import aliases of the view.
    fn add_all_imports_to_pseudo_stash(&mut self, entries: &mut ValueMap) {
        for (key, display) in self.env_imports_here() {
            let key = key.resolve();
            let value = self.env().get(&key).cloned().or_else(|| {
                (!key.starts_with(['$', '@', '%', '&']))
                    .then(|| self.imported_term_or_type(&key))
                    .flatten()
            });
            if let Some(value) = value {
                entries
                    .entry(display.resolve().to_string())
                    .or_insert(value);
            }
        }
    }

    /// The value a bare-word read of the imported name `key` (an import
    /// alias key: a type name, or a sigilless term's `\name` key) sees.
    // Cost: O(1) expected, plus the module-scope probe of `term_binding`.
    fn imported_term_or_type(&self, key: &str) -> Option<Value> {
        let bare = key
            .strip_prefix(crate::runtime::term_names::TERM_PREFIX)
            .unwrap_or(key);
        self.term_binding(bare).or_else(|| {
            self.has_type(bare)
                .then(|| Value::package(Symbol::intern(bare)))
        })
    }
}
