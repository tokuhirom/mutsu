//! `is export` for routines: the `EXPORT::<TAG>::name` aliases a sub or a
//! multi family gets in the routine registry, and the export-tag tables
//! `import_module` reads back.
//!
//! A multi family is exported candidate by candidate as each one is installed
//! (#11761): once a family's tags are recorded, a later candidate aliases only
//! its own keys, so exporting a family of c candidates costs O(c) alias inserts
//! instead of re-aliasing the whole family on every arrival.

use super::*;

/// Record `tags` as exports of `name` in `table[key]`. A family's later
/// candidates re-record tags that are already there, so check before taking
/// the copy-on-write table: `cow_table_mut` copies a shared table whole.
// Cost: O(t), t = tags, when every tag is recorded already; otherwise O(t)
// plus a copy of `table` when it is shared.
fn record_export_tags(
    table: &mut std::sync::Arc<super::module_state::ExportTagTable>,
    key: &str,
    name: &str,
    tags: &[String],
) {
    let recorded = table.get(key).and_then(|exports| exports.get(name));
    if recorded.is_some_and(|recorded| tags.iter().all(|tag| recorded.contains(tag))) {
        return;
    }
    // The package's row is usually there already: probe before `entry`,
    // which would allocate an owned key just to drop it.
    let table = crate::runtime::cow_table_mut(table);
    if !table.contains_key(key) {
        table.insert(key.to_string(), Default::default());
    }
    let entry = table
        .get_mut(key)
        .expect("row inserted above")
        .entry(name.to_string())
        .or_default();
    for tag in tags {
        if !entry.contains(tag) {
            entry.insert(tag.clone());
        }
    }
}

/// Whether `tags` are all recorded for `name` in `table[key]`.
// Cost: O(t), t = tags.
fn tags_recorded(
    table: &super::module_state::ExportTagTable,
    key: &str,
    name: &str,
    tags: &[String],
) -> bool {
    table
        .get(key)
        .and_then(|exports| exports.get(name))
        .is_some_and(|recorded| tags.iter().all(|tag| recorded.contains(tag)))
}

/// The spellings an exported routine is aliased under, as the qualifying
/// package (`None` for the bare spelling) and the tag of a key
/// `[<package>::]EXPORT::<TAG>::<name>[/<suffix>]`: the bare one, `package`'s
/// and, when a different module is being loaded, the `owner`'s, for each of
/// `tags` and for `ALL`.
// Cost: O(t), t = tags.
fn export_alias_spellings<'a>(
    package: &'a str,
    owner: Option<&'a str>,
    tags: &'a [String],
) -> Vec<(Option<&'a str>, &'a str)> {
    let all = (!tags.iter().any(|tag| tag == "ALL")).then_some("ALL");
    let tags = tags.iter().map(String::as_str).chain(all);
    tags.flat_map(|tag| {
        [Some(None), Some(Some(package)), owner.map(Some)]
            .into_iter()
            .flatten()
            .map(move |qualifier| (qualifier, tag))
    })
    .collect()
}

impl Interpreter {
    pub(crate) fn register_exported_sub(
        &mut self,
        package: String,
        name: String,
        tags: Vec<String>,
    ) {
        self.register_exported_sub_inner(&package, &name, &tags, None);
    }

    /// [`Self::register_exported_sub`] for a `multi` candidate that was just
    /// installed under `new_keys`. When every tag is already recorded for the
    /// family, every earlier candidate was aliased when it arrived, so only
    /// `new_keys` need aliases: O(k) instead of a registry scan that re-aliases
    /// the whole family (#11761). Otherwise (first export of the family, a new
    /// tag) it aliases the whole family, listed from the interned-name family
    /// index rather than a registry scan.
    // Cost: O(t·k) once the family's tags are recorded, t = tags, k = new keys;
    // otherwise O(t·c + n), c = family candidates, n = interned names spelled
    // like one of them (plus the index's amortized O(1) per newly interned name).
    pub(crate) fn register_exported_multi_candidates(
        &mut self,
        package: &str,
        name: &str,
        tags: &[String],
        new_keys: &[Symbol],
    ) {
        self.register_exported_sub_inner(package, name, tags, Some(new_keys));
    }

    /// Whether `tags` are already recorded as exports of `name` in every table
    /// [`Self::register_exported_sub_inner`] records them in — i.e. whether an
    /// earlier registration already aliased the family under every tag, and
    /// recording them again would change nothing.
    // Cost: O(t), t = tags.
    fn export_tags_recorded(&self, package: &str, name: &str, tags: &[String]) -> bool {
        let module = &self.module;
        let unit_mod = module.unit_module_loading_stack.last();
        let loading = module.module_load_stack.last();
        tags_recorded(&module.exported_subs, package, name, tags)
            && loading
                .is_none_or(|owner| tags_recorded(&module.module_owned_exports, owner, name, tags))
            && unit_mod.is_none_or(|unit_mod| {
                tags_recorded(&module.unit_module_exported_subs, unit_mod, name, tags)
                    && loading
                        .is_none_or(|owner| tags_recorded(&module.exported_subs, owner, name, tags))
            })
    }

    /// Insert the export aliases of `candidates` — each a `/<suffix>` of a
    /// multi candidate's registry key (`None` for a plain sub) and its def —
    /// under every one of `spellings` (see [`export_alias_spellings`]),
    /// keeping an alias that already exists.
    ///
    /// The keys are built in one reused buffer and installed under a single
    /// registry write, and only when one of them is new. Every alias of one
    /// candidate shares the `::name[/suffix]` tail its base name is read from,
    /// so one representative key per candidate evicts them all from the
    /// base-name index.
    // Cost: O(s·c), s = spellings, c = candidates.
    fn install_export_aliases(
        &mut self,
        spellings: &[(Option<&str>, &str)],
        name: &str,
        candidates: &[(Option<&str>, Arc<FunctionDef>)],
    ) {
        let mut buf = String::with_capacity(96);
        let mut planned: Vec<(Symbol, usize)> =
            Vec::with_capacity(spellings.len() * candidates.len());
        for (idx, (suffix, _)) in candidates.iter().enumerate() {
            for (qualifier, tag) in spellings {
                buf.clear();
                if let Some(qualifier) = qualifier {
                    buf.push_str(qualifier);
                    buf.push_str("::");
                }
                buf.push_str("EXPORT::");
                buf.push_str(tag);
                buf.push_str("::");
                buf.push_str(name);
                if let Some(suffix) = suffix {
                    buf.push('/');
                    buf.push_str(suffix);
                }
                planned.push((Symbol::intern(&buf), idx));
            }
        }
        {
            let registry = self.registry();
            planned.retain(|(key, _)| registry.functions.get(key).is_none());
        }
        if planned.is_empty() {
            return;
        }
        {
            let mut registry = self.registry_mut();
            let functions = registry.functions_mut();
            for (key, idx) in &planned {
                functions
                    .entry(*key)
                    .or_insert_with(|| candidates[*idx].1.clone());
            }
        }
        let mut last = usize::MAX;
        self.invalidate_fn_resolution_for_keys(
            planned
                .iter()
                .filter(|(_, idx)| std::mem::replace(&mut last, *idx) != *idx)
                .map(|(key, _)| *key),
        );
    }

    fn register_exported_sub_inner(
        &mut self,
        package: &str,
        name: &str,
        tags: &[String],
        new_keys: Option<&[Symbol]>,
    ) {
        static DEFAULT_TAGS: std::sync::LazyLock<[String; 1]> =
            std::sync::LazyLock::new(|| ["DEFAULT".to_string()]);
        let tags = if tags.is_empty() {
            &DEFAULT_TAGS[..]
        } else {
            tags
        };
        let recorded = self.export_tags_recorded(package, name, tags);
        // Register EXPORT namespace aliases so that EXPORT::TAG::name and
        // Package::EXPORT::TAG::name resolve via normal function lookup.
        let fq_sym = crate::qualified::qualified_text(package, name);
        // Hoist the clone to a `let` so the read guard drops before the
        // registry_mut writes below (read->write on the same lock deadlocks).
        let def = self.registry().functions.get(&fq_sym).cloned();
        // Multi candidates are stored under an arity-qualified key, so there
        // is no exact `package::name` entry to use for the EXPORT aliases.
        // Snapshot that family before taking mutable registry access. These
        // aliases also let imports recover a family when a distribution's
        // `unit module` name differs from its provided module path.
        let candidates: Vec<(Option<&'static str>, Arc<FunctionDef>)> = if let Some(def) = def {
            vec![(None, def)]
        } else {
            // Once the family's tags are recorded, every earlier candidate was
            // aliased when it arrived and only `new_keys` are new. Otherwise
            // (the family's first export, or a new tag) list the whole family
            // from the interned-name family index, a superset of its registry
            // keys: neither a registry scan nor the base-name index, which
            // every registration in between evicts (#11761).
            let family = fq_sym.as_str();
            let family_keys: std::borrow::Cow<'_, [Symbol]> = match new_keys {
                Some(new_keys) if recorded => new_keys.into(),
                // The index records qualified, sigil-less spellings only.
                _ if crate::str_scan::has_double_colon(family)
                    && !family.starts_with(['$', '@', '%', '&']) =>
                {
                    crate::qualified_tail_index::names_in_family(family).into()
                }
                _ => self
                    .registry()
                    .functions
                    .keys()
                    .copied()
                    .collect::<Vec<_>>()
                    .into(),
            };
            let registry = self.registry();
            family_keys
                .iter()
                .filter_map(|key| {
                    let suffix = key.as_str().strip_prefix(family)?.strip_prefix('/')?;
                    Some((Some(suffix), registry.functions.get(key)?.clone()))
                })
                .collect()
        };
        if !candidates.is_empty() {
            let owner = self
                .module
                .module_load_stack
                .last()
                .filter(|owner| owner.as_str() != package)
                .cloned();
            let spellings = export_alias_spellings(package, owner.as_deref(), tags);
            self.install_export_aliases(&spellings, name, &candidates);
        }
        if recorded {
            return;
        }
        // Mirror this export into the unit-module export table so that
        // `import_module` can validate tags for `unit module X` files whose
        // runtime package registration used "GLOBAL".
        if let Some(unit_mod) = self.module.unit_module_loading_stack.last() {
            record_export_tags(
                &mut self.module.unit_module_exported_subs,
                unit_mod,
                name,
                tags,
            );
        }
        // Attribute this export to the module currently being loaded (any kind:
        // unit, package-block, or bare-file). The `use MOD` tag-filter uses this
        // to hide only MOD's own exports, never a symbol MOD imported from a
        // transitively-`use`d module.
        if let Some(owner) = self.module.module_load_stack.last() {
            record_export_tags(&mut self.module.module_owned_exports, owner, name, tags);
        }
        // The module load stack names the requested compunit path. Keep a
        // second metadata entry under that path when the declared unit package
        // is different, so `use Lingua::EN::Numbers :short` can validate the
        // export even though the file says `unit module Numbers`.
        if self.module.unit_module_loading_stack.last().is_some()
            && let Some(module) = self.module.module_load_stack.last()
        {
            record_export_tags(&mut self.module.exported_subs, module, name, tags);
        }
        record_export_tags(&mut self.module.exported_subs, package, name, tags);
    }

    /// Refresh the export aliases for a multi family after a later candidate
    /// is registered. An exported proto exports its candidates too, but the
    /// proto commonly appears before those candidates in a module body. The
    /// first export registration therefore cannot create the arity-qualified
    /// aliases until the candidates exist.
    ///
    /// `new_keys` are the registry keys the arriving candidate was installed
    /// under (empty when nothing new was installed); see
    /// [`Self::register_exported_multi_candidates`]. `exported_with` names the
    /// tags the caller has just exported this candidate under, in the current
    /// package (`Some(&[])` meaning `DEFAULT`): when they cover every tag the
    /// family is exported under, the candidate is aliased already.
    // Cost: O(t), t = the family's tags, when `exported_with` covers them;
    // otherwise that of `register_exported_multi_candidates`.
    pub(crate) fn refresh_exported_multi_family(
        &mut self,
        name: &str,
        new_keys: &[Symbol],
        exported_with: Option<&[String]>,
    ) {
        let package = self.current_package();
        let recorded = if crate::qualified::is_global_name(&package) {
            self.module
                .module_load_stack
                .last()
                .and_then(|module| self.module.module_owned_exports.get(module))
                .and_then(|exports| exports.get(name))
        } else {
            self.module
                .exported_subs
                .get(&package)
                .and_then(|exports| exports.get(name))
        };
        let tags: Option<Vec<String>> = match recorded {
            Some(recorded) => {
                if let Some(exported_with) = exported_with {
                    let covered = |tag: &String| match exported_with {
                        [] => tag == "DEFAULT",
                        tags => tags.contains(tag),
                    };
                    if recorded.iter().all(covered) {
                        return;
                    }
                }
                Some(recorded.iter().cloned().collect())
            }
            None if self.module.module_load_stack.is_empty() => None,
            None => self
                .imported_exported_proto_tags(&package, name)
                .map(|tags| tags.into_iter().collect()),
        };
        if let Some(tags) = tags {
            self.register_exported_multi_candidates(&package, name, &tags, new_keys);
        }
    }
}
