//! Pseudo-stash reads: `Pkg::`, `OUTER::`, `MY::`, `DYNAMIC::`, `CALLER::`, and the
//! fused one-key read `Pkg::<$x>` (`GetPseudoStashKeyed`).
use super::*;
use crate::value::ValueMap;

impl Interpreter {
    // Cost: O(v), v = entries of the whole env (plus `our_vars` for GLOBAL, locals for MY::):
    // every pseudo/package stash read materializes a fresh map by scanning them all (see
    // `package_stash_value`), however few symbols the package has. Rakudo: O(1) -- see #9171.
    pub(super) fn exec_get_pseudo_stash_op(&mut self, code: &CompiledCode, name_idx: u32) {
        let name = Self::const_str(code, name_idx);
        if let Some(depth) = Self::caller_stash_depth(name) {
            let origin = self.caller_frame_package();
            let origin_routine = self.caller_frame_enclosing_routine();
            let stash = self.caller_stash_value(name, depth);
            Self::stamp_stash_origin_package(&stash, &origin);
            Self::stamp_stash_origin_routine(&stash, origin_routine.as_deref());
            Self::stamp_stash_origin_unit(&stash, self.caller_frame_unit());
            self.stack.push(stash);
            return;
        }
        if name.strip_suffix("::") == Some("OUTER") {
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
            return;
        }
        if name.strip_suffix("::") == Some("OUR") {
            let stash = self.our_pseudo_stash();
            self.stack.push(stash);
            return;
        }
        if name.strip_suffix("::") == Some("DYNAMIC") {
            let entries = self.dynamic_pseudo_stash_entries();
            let stash = self.pseudo_stash_hash(entries);
            self.stack.push(stash);
            return;
        }
        if let Some(kind) = name.strip_suffix("::")
            && kind == "CALLERS"
        {
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
            return;
        }
        if let Some(target) = self.named_pseudo_stash_target(name) {
            let stash = loan_env!(self, package_stash_value(&target));
            self.stack.push(stash);
            return;
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
            if self.should_hide_from_my_global_stash(&key_str) {
                continue;
            }
            let display_key = Self::add_sigil_prefix(&key_str);
            entries.entry(display_key).or_insert_with(|| val.clone());
        }
        self.add_visible_routines_to_pseudo_stash(&mut entries);
        let stash = self.pseudo_stash_hash(entries);
        self.stack.push(stash);
    }

    /// The package a `Name::` pseudo-stash read names, when it is an ordinary
    /// package stash (not `CALLER::`, `OUTER::`, `OUR::`, `DYNAMIC::`,
    /// `CALLERS::`, `MY::`, `LEXICAL::` or `UNIT::`, which have their own
    /// builders in [`Self::exec_get_pseudo_stash_op`]).
    // Cost: O(m), m = bytes of the name, plus one env probe.
    fn named_pseudo_stash_target(&self, name: &str) -> Option<String> {
        if Self::caller_stash_depth(name).is_some() {
            return None;
        }
        if matches!(
            name.strip_suffix("::"),
            Some("OUTER" | "OUR" | "DYNAMIC" | "CALLERS")
        ) {
            return None;
        }
        if let Some(package) = name.strip_suffix("::")
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
            let target = self
                .get_env_with_main_alias(package)
                .and_then(|v| match v.view() {
                    ValueView::Package(sym) => Some(
                        self.resolve_type_in_current_package(&sym.resolve())
                            .unwrap_or_else(|| sym.resolve()),
                    ),
                    _ => None,
                })
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
    // names ending in the key's bare name (`qualified_tail_index`); otherwise as
    // `GetPseudoStash` plus `Index`, O(v), v = env entries. Rakudo: O(1) -- see #9171.
    pub(super) fn exec_get_pseudo_stash_keyed_op(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
    ) -> Result<(), RuntimeError> {
        let key = self.stack.pop().unwrap_or(Value::NIL);
        self.push_pseudo_stash_for_key(code, name_idx, &key);
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
    ) {
        let name = Self::const_str(code, name_idx);
        let keyed = match key.deref_container().view() {
            ValueView::Str(key_str) => self
                .named_pseudo_stash_target(name)
                .and_then(|target| loan_env!(self, package_stash_keyed_value(&target, &key_str))),
            _ => None,
        };
        match keyed {
            Some(stash) => self.stack.push(stash),
            None => self.exec_get_pseudo_stash_op(code, name_idx),
        }
    }

    /// Build a pseudo-stash for the exact lexical frame selected by the
    /// compiler. Each entry is `[display-key, bare-name, depth, slot]`; a
    /// non-negative slot is live in this frame, while an outer-frame entry is
    /// resolved through the same captured lexical path as `GetOuterVar`.
    pub(super) fn exec_get_lexical_stash_op(&mut self, code: &CompiledCode, spec_idx: u32) {
        let mut entries: ValueMap = ValueMap::default();
        let Some(ValueView::Array(spec, _)) =
            code.constants.get(spec_idx as usize).map(Value::view)
        else {
            let stash = self.pseudo_stash_hash(entries);
            self.stack.push(stash);
            return;
        };
        for item in spec.items() {
            let ValueView::Array(parts, _) = item.view() else {
                continue;
            };
            if parts.items().len() != 4 {
                continue;
            }
            let (
                ValueView::Str(display),
                ValueView::Str(name),
                ValueView::Int(depth),
                ValueView::Int(slot),
            ) = (
                parts.items()[0].view(),
                parts.items()[1].view(),
                parts.items()[2].view(),
                parts.items()[3].view(),
            )
            else {
                continue;
            };
            let depth = depth.max(0) as usize;
            let value = if depth == 0 {
                if slot >= 0 {
                    self.locals
                        .get(slot as usize)
                        .cloned()
                        .unwrap_or(Value::NIL)
                } else {
                    self.get_env_with_main_alias(&name).unwrap_or(Value::NIL)
                }
            } else {
                let slot = (slot >= 0).then_some(slot as u32);
                self.get_outer_var(code, &name, depth, slot)
            };
            entries.insert(display.to_string(), value);
        }
        // Imported type/package aliases are lexical names too, but they are
        // maintained in the runtime environment rather than in a compiler
        // scope frame. Preserve those visible package bindings while keeping
        // ordinary scalar entries frame-local; the latter are precisely what
        // makes MY:: stop leaking enclosing lexicals.
        for (key, value) in self.env().iter() {
            let key = key.resolve();
            if self.should_hide_from_my_global_stash(&key)
                || !matches!(value.view(), ValueView::Package(_))
            {
                continue;
            }
            let display = Self::add_sigil_prefix(&key);
            entries.entry(display).or_insert_with(|| value.clone());
        }
        // Imports are not compiler declarations, but Raku exposes their
        // aliases through the importing compunit's lexical pad. Keep them
        // separate from the flattened environment so `MY::` still excludes
        // enclosing lexicals (and `OUTER::MY::` can see the import).
        for (key, display) in &self.imported_env_aliases {
            let key = key.resolve();
            if self.should_hide_from_my_global_stash(&key) {
                continue;
            }
            if let Some(value) = self.env().get(&key) {
                entries
                    .entry(display.resolve().to_string())
                    .or_insert_with(|| value.clone());
            }
        }
        self.add_visible_routines_to_pseudo_stash(&mut entries);
        let stash = self.pseudo_stash_hash(entries);
        self.stack.push(stash);
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
    fn pseudo_stash_hash(&mut self, entries: ValueMap) -> Value {
        let hash = Value::hash_with_data(Value::hash_arc(entries));
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
            if self.should_hide_from_my_global_stash(&key_str) {
                continue;
            }
            let display_key = Self::add_sigil_prefix(&key_str);
            entries.entry(display_key).or_insert_with(|| val.clone());
        }
        self.add_visible_routines_to_pseudo_stash(&mut entries);
        self.pseudo_stash_hash(entries)
    }
}
