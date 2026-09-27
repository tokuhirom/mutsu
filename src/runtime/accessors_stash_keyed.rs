//! Per-key package stash membership, shared by the whole-stash build
//! (`package_stash_value`) and the single-key read (`package_stash_symbol`)
//! (#9171).
//!
//! A package's symbols are flat qualified keys spread over the env, the `our`
//! store and the routine registries. Each `*_stash_member` helper here answers,
//! for ONE such key, which stash entry of a package it contributes. The whole
//! build runs them over every key of each store; the keyed read runs them only
//! over the keys `qualified_tail_index` names for the requested member, so both
//! agree by construction on what a key means.
use super::*;
use crate::value::ValueMap;

/// What one env key contributes to a package stash.
pub(super) enum EnvStashMember {
    /// The env value itself, under this stash key (overwrites an earlier entry).
    Value(String),
    /// A sub-package of the stash's package, named by this head component
    /// (only if no entry of that name exists yet).
    SubPackage(String),
}

impl Interpreter {
    /// The stash entry of `package_name` that the env key `key_s` contributes,
    /// after the hiding rules (`__mutsu_` markers, transitive-dependency hides,
    /// `my`-scoped items). `key_s` is the resolved key with any enum-bare prefix
    /// already unwrapped.
    // Cost: O(m), m = bytes of the key (a few set probes plus suffix matching).
    pub(super) fn env_stash_member(
        &self,
        key_s: &str,
        package_name: &str,
    ) -> Option<EnvStashMember> {
        // Internal bookkeeping markers use package-like separators (for
        // example, the inline-package prepass marker contains
        // `::Exporter::exported`). They must not appear as pseudo-package
        // members when a named package's stash is assembled.
        if key_s.starts_with("__mutsu_") {
            return None;
        }
        if (package_name == "MY" || package_name == "GLOBAL")
            && self.should_hide_from_my_global_stash(key_s)
        {
            return None;
        }
        // Skip env entries hidden from package stash lookups (transitive deps)
        if package_name != "MY"
            && package_name != "GLOBAL"
            && self.package_stash_hidden.contains(key_s)
        {
            return None;
        }
        // Skip my-scoped items (they should not appear in the package stash).
        // A lexical type is registered in the env under its source-facing
        // qualified name (`M::C`) but marked under its mangled registry
        // storage name (`M::C\0<decl-id>`), so the package-item marker alone
        // cannot identify this env entry.
        if self.is_my_scoped_package_item(key_s) || self.is_my_scoped_type_name(key_s) {
            return None;
        }
        // GLOBAL's env scan sees the self-qualified mirror of a root
        // `our` (`GLOBAL::o` alongside bare `o`, see the `our_vars` loop
        // in `package_stash_value`) -- strip it so it is recognized as the
        // same symbol rather than a sub-package named literally "GLOBAL".
        let effective_key: &str = if package_name == "GLOBAL" {
            key_s.strip_prefix("GLOBAL::").unwrap_or(key_s)
        } else {
            key_s
        };
        if let Some(stash_key) = Self::sigil_leading_stash_member(effective_key, package_name) {
            return Some(EnvStashMember::Value(stash_key));
        }
        let rest = Self::stash_member_tail(effective_key, package_name)?;
        // A member whose tail is itself qualified (`foo::bar` seen from
        // GLOBAL) does not name a symbol of THIS package -- it names a
        // symbol of a *sub-package*. The stash member is that
        // sub-package, exactly once, as a package value whose own
        // `.WHO` carries the members (`my $foo::bar = 1` gives
        // `OUR::.keys` == `(foo)` and `OUR::<foo>.WHO.keys` == `($bar)`,
        // not a flat `foo::bar` key).
        if let Some((head, _)) = rest.split_once("::") {
            // `__mutsu_constant_var::C` and similar internal markers
            // are qualified-looking but are not a real sub-package --
            // only GLOBAL's unconditional `stash_member_tail` match
            // makes them reach this branch at all.
            if head.is_empty() || Self::env_tail_has_sigil(head) || head.starts_with("__mutsu_") {
                return None;
            }
            return Some(EnvStashMember::SubPackage(head.to_string()));
        }
        // GLOBAL is the root package: every flat env key would
        // otherwise pass through unconditionally (`stash_member_tail`
        // treats GLOBAL specially since there is no `GLOBAL::` prefix
        // to strip), which drags in dynamic variables (`$*CWD`),
        // compile-time magicals (`$?FILE`), POD markers (`$=pod`),
        // and internal bookkeeping keys. Real Raku keeps all of
        // those outside the user's own GLOBAL stash. A plain
        // lowercase bare name is a `my` lexical -- genuine `our`
        // scalars are already covered by the dedicated `our_vars` loop,
        // so this flat mirror would only be a harmless-but-wrong
        // duplicate at best.
        if package_name == "GLOBAL" && !Self::is_global_root_symbol(rest) {
            return None;
        }
        Some(EnvStashMember::Value(Self::stash_symbol_key_from_env_tail(
            rest,
        )))
    }

    /// The stash key a named (non-GLOBAL) package's `our`-store key `key`
    /// contributes. That store is where an `our` declared in a branch that
    /// never RAN is pre-installed (`EndWalker::install_our_symbol`).
    // Cost: O(m), m = bytes of the key.
    pub(super) fn our_var_stash_member(key: &str, package_name: &str) -> Option<String> {
        if key.starts_with("__mutsu_") {
            return None;
        }
        if let Some(stash_key) = Self::sigil_leading_stash_member(key, package_name) {
            return Some(stash_key);
        }
        let rest = Self::stash_member_tail(key, package_name)?;
        if rest.is_empty() || rest.contains("::") {
            return None;
        }
        Some(Self::stash_symbol_key_from_env_tail(rest))
    }

    /// The bare routine name (the stash key minus its `&`) that the routine
    /// registry key `key_s` contributes to `package_name`, or `None` when it
    /// contributes nothing. `split_signature` cuts a `name/signature` key at
    /// the `/` (the `functions` table); a proto key is taken whole.
    // Cost: O(m), m = bytes of the key (one `format!` for the `my`-scope probe).
    pub(super) fn routine_stash_member<'a>(
        &self,
        key_s: &'a str,
        package_name: &str,
        split_signature: bool,
    ) -> Option<&'a str> {
        // A top-level sub's registry key is always package-qualified
        // (`GLOBAL::name`), including at the root -- unlike a named
        // package, GLOBAL's `stash_member_tail` special-case returns the
        // whole key unconditionally (there is no `GLOBAL::` prefix to
        // require), so without stripping it here the tail would still
        // carry the self-qualification and get misread as a `::`-nested
        // sub-package name a few lines down, dropping the sub entirely.
        let effective_key: &str = if package_name == "GLOBAL" {
            key_s.strip_prefix("GLOBAL::").unwrap_or(key_s)
        } else {
            key_s
        };
        let rest = Self::stash_member_tail(effective_key, package_name)?;
        let base = if split_signature {
            rest.split('/').next().unwrap_or(rest)
        } else {
            rest
        };
        if base.is_empty() || base.contains("::") || base.contains(':') {
            return None;
        }
        // Skip my-scoped subs (they should not appear in the package stash)
        if self.is_my_scoped_package_item(&format!("{}::{}", package_name, base)) {
            return None;
        }
        Some(base)
    }

    /// Whether `package_name`'s stash is assembled by a rule other than the
    /// per-key membership above (a pseudo-package, a built-in, an `EXPORT`
    /// view, or the root), so a single key cannot be answered by probing.
    fn stash_needs_whole_build(package: &str, package_name: &str) -> bool {
        package_name.is_empty()
            || matches!(package_name, "GLOBAL" | "PROCESS" | "Bool" | "MY")
            || package_name == "EXPORT::all"
            || package_name.ends_with("::EXPORT::all")
            || Self::package_export_tag_parts(package).is_some()
            || Self::package_export_module(package_name).is_some()
    }

    /// The single stash entry `package::<key>` — exactly what
    /// `package_stash_value(package)` would hold under `key` — without
    /// materializing the rest of the stash.
    ///
    /// `Some(entry)` is the answer (`None` inside: no such member). The outer
    /// `None` means this read cannot be answered per key (a pseudo or built-in
    /// package, or a key that is not a sigiled member name) and the caller must
    /// build the whole stash.
    ///
    /// Each store contributes in the order the whole build merges it: an env
    /// key overwrites, every later store only fills a missing entry.
    // Cost: O(k), k = interned qualified names ending in the key's bare name
    // (`qualified_tail_index`), independent of the env, the package and the
    // registries; plus O(e), e = the package's members, when the package is
    // itself an enum.
    pub(crate) fn package_stash_symbol(&self, package: &str, key: &str) -> Option<Option<Value>> {
        let package_name = Self::normalize_stash_package(package);
        if Self::stash_needs_whole_build(package, &package_name) {
            return None;
        }
        let sigil = key.chars().next()?;
        if !matches!(sigil, '$' | '@' | '%' | '&') {
            return None;
        }
        let bare = &key[1..];
        // The index cuts a spelling at its first `/` and never records one
        // that is still qualified, so such a member name must take the whole
        // build to be found. Any `:` (a qualifier, or an operator name like
        // `&infix:<+>`) sends the key there too.
        if bare.is_empty() || bare.contains(':') || bare.contains('/') {
            return None;
        }
        let candidates = crate::qualified_tail_index::names_ending_in(bare);

        // 1. The env (its own tier, as the whole build's `env.iter()` sees it).
        //    A sigiled key never names a sub-package, so only `Value` counts.
        for &name in &candidates {
            let Some(value) = self.env.overlay_get_sym(name) else {
                continue;
            };
            if let Some(EnvStashMember::Value(stash_key)) =
                self.env_stash_member(name.as_str(), &package_name)
                && stash_key == key
            {
                return Some(Some(value.clone()));
            }
        }
        // 2. Code-valued `our constant &alias is export(...)` declarations.
        if self
            .exported_vars
            .get(package_name.as_str())
            .is_some_and(|vars| vars.contains_key(key))
            && let Some(value) = self.exported_var_value(&package_name, key)
        {
            return Some(Some(value));
        }
        // 3. The `our` store.
        for &name in &candidates {
            let Some(value) = self.get_our_var(name.as_str()) else {
                continue;
            };
            if Self::our_var_stash_member(name.as_str(), &package_name).as_deref() == Some(key) {
                return Some(Some(value.clone()));
            }
        }
        // 4. Enum members.
        if let Some(variants) = self.registry().enum_types.get(&package_name)
            && let Some(index) = variants.iter().position(|(name, _)| name == key)
        {
            let (name, value) = &variants[index];
            return Some(Some(Value::enum_parts(
                Symbol::intern(package_name.as_str()),
                Symbol::intern(name),
                value.clone(),
                index,
            )));
        }
        // 5. Routines and protos: only `&` members.
        if sigil == '&' {
            let registry = self.registry();
            for &name in &candidates {
                if let Some(def) = registry.functions.get(&name)
                    && self.routine_stash_member(name.as_str(), &package_name, true) == Some(bare)
                {
                    return Some(Some(Value::routine_parts(def.package, def.name, false)));
                }
            }
            for &name in &candidates {
                if let Some(def) = registry.proto_functions.get(&name)
                    && self.routine_stash_member(name.as_str(), &package_name, false) == Some(bare)
                {
                    return Some(Some(Value::routine_parts(def.package, def.name, false)));
                }
            }
        }
        // The remaining stores (classes, roles, the `EXPORT` member) only ever
        // contribute unsigiled keys.
        Some(None)
    }

    /// `package::<key>` as a one-entry stash (empty when there is no such
    /// member), ready for the ordinary `Index` read — or `None` when the read
    /// needs the whole stash (see [`Self::package_stash_symbol`]).
    // Cost: as `package_stash_symbol`.
    pub(crate) fn package_stash_keyed_value(&self, package: &str, key: &str) -> Option<Value> {
        let entry = self.package_stash_symbol(package, key)?;
        let mut symbols = ValueMap::default();
        if let Some(value) = entry {
            symbols.insert(key.to_string(), value);
        }
        Some(Self::make_stash_instance(package, symbols))
    }
}
