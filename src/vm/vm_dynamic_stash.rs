//! `DYNAMIC::<key>`: a name looked up through the dynamic scope (#13590,
//! ADR-12529 §2.1).
//!
//! Rakudo resolves a `DYNAMIC::` key by walking the dynamic scope frame by
//! frame, the running frame first, and looking the key up exactly as spelled
//! in each frame's own lexical pad. So `DYNAMIC::<$x>` finds a caller's
//! ordinary `my $x`, a frame's own `my $*d` shadows a caller's from the
//! frame's entry on (its pad entry exists before the declaration runs), and a
//! process-level dynamic such as `$*OUT`, which lives in no pad, is not found.
//!
//! A mutsu frame keeps its lexicals in two places: the slots its code
//! declares (`CompiledCode::locals`) and its env tiers. Each `VmCallFrame`
//! records its caller's code (`caller_code`) next to the caller's slot region
//! and env, so the walk reads every frame the same way the running one is
//! read. A call path that pushes no `VmCallFrame` (a light call) leaves its
//! frame out of the walk; its names are still in the env tiers of the frame
//! the walk reaches next.

use super::*;
use crate::value::ValueMap;

impl Interpreter {
    /// The value `DYNAMIC::<key>` names, or `None` when no frame of the
    /// dynamic scope binds `key`.
    // Cost: O(f * (l + d)), f = frames on the call stack, l = slots each
    // frame's code declares, d = each frame's env tiers. Rakudo: O(f * p),
    // p = pad lookup.
    pub(super) fn dynamic_stash_keyed_value(
        &self,
        code: &CompiledCode,
        key: &str,
    ) -> Option<Value> {
        // A sigil-less key names a term: a constant's binding is its term key
        // (#9962, #17846); any other sigil-less binding (a lexical type) is
        // under the key itself.
        if !key.starts_with(['$', '@', '%', '&'])
            && let Some(value) =
                self.dynamic_stash_lookup(code, &crate::runtime::term_names::term_key(key), false)
        {
            return Some(value);
        }
        let env_key = key.strip_prefix('$').unwrap_or(key);
        self.dynamic_stash_lookup(code, env_key, key.starts_with('$'))
    }

    /// [`Self::dynamic_stash_keyed_value`] for the env key `env_key`; `scalar`
    /// when the key was spelled with `$`.
    // Cost: as `dynamic_stash_keyed_value`.
    fn dynamic_stash_lookup(
        &self,
        code: &CompiledCode,
        env_key: &str,
        scalar: bool,
    ) -> Option<Value> {
        if env_key.is_empty() {
            return None;
        }
        let sym = Symbol::intern(env_key);
        // A dynamic resolves as a `$*d` read does: through the env chain, the
        // running frame's tiers over its callers'. A scope binds its `my $*d`
        // from its entry (`Compiler::hoist_pending_dynamic_decls`), so the
        // chain already holds a pending declaration and never a dormant
        // nested block's. A nested run (the closure `map` calls) starts a
        // `call_frames` of its own; the frames it hides are still on the
        // caller env stack. The base tier (the process-level dynamics) is in
        // no frame.
        if env_key.trim_start_matches(['@', '%', '&']).starts_with('*') {
            return self
                .env()
                .get_sym_in_tiers(sym)
                .or_else(|| self.caller_frame_binding(sym))
                .cloned();
        }
        let any = || Value::package(Symbol::intern("Any"));
        let from_slot = |value: &Value| {
            // A declared scalar no store reached yet is `Any`, as an unset
            // `Scalar` reads.
            if scalar && value.is_nil() {
                any()
            } else {
                value.clone()
            }
        };
        if let Some(i) = code.locals.iter().position(|name| name == env_key)
            && let Some(value) = self.locals.get(i)
        {
            return Some(from_slot(value));
        }
        if let Some(value) = self.env().get_sym_in_frame(sym) {
            return Some(value.clone());
        }
        let slots = self.locals.all_slots();
        let mut region_end = self.locals.base();
        for frame in self.call_frames.iter().rev() {
            // A frame that saved no slot region (a call that kept the
            // caller's slots) still names its caller's code and env.
            let region = match frame.saved_locals_base.as_ref().map(|b| b.base()) {
                Some(base) => {
                    let region = slots.get(base..region_end).unwrap_or(&[]);
                    region_end = base;
                    region
                }
                None => &[],
            };
            if frame.caller_code != 0 {
                // SAFETY: `caller_code` is the address of the `CompiledCode`
                // the caller was running when it pushed this frame; that
                // caller is suspended until the frame pops, so its code is
                // alive while the frame is on `call_frames`.
                let caller = unsafe { &*(frame.caller_code as *const CompiledCode) };
                if let Some(i) = caller.locals.iter().position(|name| name == env_key)
                    && let Some(value) = region.get(i)
                {
                    return Some(from_slot(value));
                }
            }
            if let Some(value) = frame.saved_env.get_sym_in_frame(sym) {
                return Some(value.clone());
            }
        }
        None
    }

    /// Push the stash `DYNAMIC::<key>` indexes: the one entry the dynamic
    /// scope binds under `key`, or none.
    // Cost: as `dynamic_stash_keyed_value`.
    pub(super) fn push_dynamic_stash_for_key(&mut self, code: &CompiledCode, key: &str) {
        let mut entries = ValueMap::default();
        let found = self.dynamic_stash_keyed_value(code, key);
        let found = if crate::parser::current_language_version_starts_with("6.e") {
            self.dynamic_stash_6e_value(key, found)
        } else {
            found
        };
        if let Some(value) = found {
            entries.insert(key.to_string(), value);
        }
        let stash = self.pseudo_stash_hash_without_topic(entries);
        self.stack.push(stash);
    }

    /// `GLOBAL::<$bare>` (or `@`/`%`), read the way `$GLOBAL::bare` is.
    // Cost: O(|bare|), one qualified-name read.
    fn global_variable(&mut self, sigil: char, bare: &str) -> Option<Value> {
        let name = if sigil == '$' {
            format!("GLOBAL::{bare}")
        } else {
            format!("{sigil}GLOBAL::{bare}")
        };
        self.get_env_with_main_alias(&name)
            .filter(|value| !value.is_nil())
    }

    /// `GLOBAL`'s variables as the dynamic chain sees them from 6.e on: each
    /// `$x` / `@x` / `%x` of the stash is a fallback for `$*x` / `@*x` /
    /// `%*x`, keyed by that twigilled spelling.
    // Cost: O(g), g = GLOBAL's stash entries (the stash is built whole).
    pub(super) fn global_scalar_dynamics(&mut self) -> Vec<(String, Value)> {
        let stash = loan_env!(self, package_stash_value("GLOBAL"));
        let ValueView::Instance { attributes, .. } = stash.view() else {
            return Vec::new();
        };
        let Some(symbols) = attributes.as_map().get("symbols").cloned() else {
            return Vec::new();
        };
        let ValueView::Hash(map) = symbols.view() else {
            return Vec::new();
        };
        let mut found: Vec<(String, Value)> = map
            .iter()
            .filter_map(|(key, value)| {
                let sigil = key
                    .chars()
                    .next()
                    .filter(|c| matches!(c, '$' | '@' | '%'))?;
                let bare = &key[1..];
                bare.starts_with(|c: char| c.is_alphabetic() || c == '_')
                    .then(|| (format!("{sigil}*{bare}"), value.clone()))
            })
            .collect();
        // The file-scope `our` variables (`GLOBAL::<$x> = ...` among them),
        // which the `our` store keys by their bare spelling (`x`, `@a`).
        let globals: Vec<Symbol> = self.our_table().global_names().iter().copied().collect();
        for name in globals {
            let key = name.resolve();
            let (sigil, bare) = match key.chars().next() {
                Some(c @ ('@' | '%')) => (c, &key[1..]),
                _ => ('$', key.as_str()),
            };
            if !bare.starts_with(|c: char| c.is_alphabetic() || c == '_') {
                continue;
            }
            if let Some(value) = self.get_our_var_sym(name) {
                found.push((format!("{sigil}*{bare}"), value));
            }
        }
        found.extend(self.global_qualified_dynamics());
        found
    }

    /// 6.e's reading of what the dynamic scope binds under `key` (`found`):
    /// only a dynamic symbol is visible, so a found plain lexical is an
    /// `X::Symbol::NotDynamic` Failure; and a `$*x` no frame declares is
    /// `GLOBAL::<$x>` before `PROCESS::<$x>`, as a `$*x` read falls back.
    // Cost: O(1) for a plain key; a `$*x` key adds one GLOBAL stash probe and
    // one process-dynamic probe.
    fn dynamic_stash_6e_value(&mut self, key: &str, found: Option<Value>) -> Option<Value> {
        let Some(sigil) = key.chars().next().filter(|c| matches!(c, '$' | '@' | '%')) else {
            return found;
        };
        let rest = &key[1..];
        let Some(bare) = rest.strip_prefix('*') else {
            // `$_` is no longer dynamic in 6.e; `$/` and `$!` are.
            let dynamic = rest != "_" && self.is_var_dynamic(if sigil == '$' { rest } else { key });
            return match found {
                Some(_) if !dynamic => Some(Self::symbol_not_dynamic_failure(key)),
                other => other,
            };
        };
        let env_key = if sigil == '$' {
            format!("*{bare}")
        } else {
            format!("{sigil}*{bare}")
        };
        let process = self.get_process_dynamic(&env_key);
        // A frame's own `my $*x` wins; the env's mirror of the process value
        // is not a frame's.
        let from_process = match (&found, &process) {
            (None, _) => true,
            (Some(value), Some(process)) => {
                crate::value::identity::values_identical(value, process)
            }
            (Some(_), None) => false,
        };
        if !from_process {
            return found;
        }
        if let Some(global) = self.global_variable(sigil, bare) {
            return Some(global);
        }
        process.or(found)
    }
}
