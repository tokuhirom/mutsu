//! `OpCode::PackageScope`: a `package`/`module`/`class` block run inline.
//!
//! The body runs over a block tier of its own ([`Env::open_block_tier`],
//! #9170), so its exit reads back only the names the body wrote by name, not
//! the whole env it ran in (#9171). When no tier could be opened (the chain is
//! at its depth cap) or the body replaced the env wholesale, the exit sees the
//! whole current env instead, which is what it always walked before.

use super::*;
use crate::vm::vm_block_env::{BlockEnvSave, BlockWrites, ClosedBlockEnv};

impl Interpreter {
    // Cost: O(t + w * m + k) plus the body -- see the `OpCode::PackageScope` arm.
    pub(super) fn exec_package_scope_op(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
        body_end: u32,
        (lexicals_idx, decls_idx, slots_idx): (u32, u32, u32),
        ip: &mut usize,
        compiled_fns: &CompiledFns,
    ) -> Result<(), RuntimeError> {
        let name = Self::const_str(code, name_idx).to_string();
        let name_sym = code.const_sym(name_idx);
        let body_end = body_end as usize;
        let saved = self.current_package();
        let save = self.open_block_env_tier();
        // The slots the body can touch (`EnvConsumerSlots::package_scope_lists`),
        // or every slot for a chunk that was never finalized.
        let touched: Option<&[u32]> = code
            .env_consumer_slots
            .package_scope_lists
            .get(slots_idx as usize)
            .map(Vec::as_slice);
        let saved_locals: Vec<Value> = match touched {
            Some(slots) => slots
                .iter()
                .map(|&slot| self.locals[slot as usize].clone())
                .collect(),
            None => self.locals.to_vec(),
        };
        self.set_current_package(name.clone());
        let lexicals = (lexicals_idx != crate::opcode::NO_PACKAGE_LEXICALS)
            .then(|| Self::const_str(code, lexicals_idx));
        let class_scope = lexicals.and_then(|lexicals| self.bind_package_body_lexicals(&name, lexicals));
        // The names the body's own scope declares are its bindings, never a
        // write to a same-named enclosing lexical (#13428).
        let declared: Vec<Symbol> = (decls_idx != u32::MAX)
            .then(|| code.constants.get(decls_idx as usize))
            .flatten()
            .and_then(Value::as_list_items)
            .map(|items| {
                items
                    .iter()
                    .filter_map(Value::as_str)
                    .map(Symbol::intern)
                    .collect()
            })
            .unwrap_or_default();
        // The declared element and key types of the block's `@`/`%` lexicals
        // live in env lanes (`__mutsu_type::%seen`,
        // `__mutsu_hash_key_type::%seen`) under names every package block
        // shares. Hide the enclosing scope's for the body, so what is found
        // afterwards is what the body declared; the enclosing scope's are put
        // back below (#12907, PDF's `my %seen{Any}` next to `my %seen{Hash}`).
        let body_lanes: Vec<Symbol> = lexicals
            .into_iter()
            .flat_map(|lexicals| lexicals.split('\n').map(Symbol::intern))
            .chain(declared.iter().copied())
            .filter(|decl| decl.as_str().starts_with(['@', '%']))
            .flat_map(|decl| {
                let mut lanes = vec![Self::type_meta_key_for_sym(decl)];
                if decl.as_str().starts_with('%') {
                    lanes.push(Self::hash_key_meta_key_for_sym(decl));
                }
                lanes
            })
            .collect();
        let outer_lanes: Vec<(Symbol, Option<Value>)> = body_lanes
            .iter()
            .map(|lane| (*lane, self.env().get_sym(*lane).cloned()))
            .collect();
        for lane in &body_lanes {
            self.env_mut().remove_sym(*lane);
        }
        // A body split by the BEGIN prologue declared its typed `@`/`%`
        // lexicals ahead of this run-time part (`my %as{Any:U} = ...` is a
        // declaration there and an assignment here), which recorded their
        // lanes for the package. The assignment checks against that record,
        // so put the package's own lanes back for the body (#15247).
        for (key, value) in self.class_body_static_meta_lanes(&name) {
            let lane = Symbol::intern(&key);
            if body_lanes.contains(&lane) {
                self.env_mut().insert_sym(lane, value);
            }
        }
        let result = self.run_range(code, *ip + 1, body_end, compiled_fns);
        // What the body declared, recorded per package: its routines read the
        // record whatever way they are entered (`declared_meta_lane` asks the
        // running package's record before the calling frames', as a unit's)
        // and the named-call entry re-injects it
        // (`inject_class_body_statics`). Then the enclosing scope's lanes
        // again.
        for lane in &body_lanes {
            if let Some(ValueView::Str(tc)) = self.env().get_sym(*lane).map(Value::view) {
                let tc = tc.as_str().to_owned();
                crate::runtime::cow_table_mut(&mut self.lexicals.unit_lexical_meta_lanes)
                    .entry(name.clone())
                    .or_default()
                    .insert(lane.resolve(), Value::str(tc.clone()));
                crate::runtime::cow_table_mut(&mut self.lexicals.class_body_static_types)
                    .entry(name.clone())
                    .or_default()
                    .insert(lane.resolve(), tc);
            }
        }
        for (lane, previous) in outer_lanes {
            match previous {
                Some(value) => {
                    self.env_mut().insert_sym(lane, value);
                }
                None => {
                    self.env_mut().remove_sym(lane);
                }
            }
        }
        if let Some(lexicals) = lexicals {
            let saved_env = match &save {
                BlockEnvSave::Tier(parent) => &**parent,
                BlockEnvSave::Whole(env) => env,
            };
            self.unbind_package_body_lexicals(&name, lexicals, saved_env);
        }
        if let Some(mark) = class_scope {
            self.end_scoped_fixtures(mark);
        }
        let ClosedBlockEnv {
            base: mut env,
            writes,
            removed,
        } = self.close_block_env_tier(save);
        let removed = removed.unwrap_or_default();
        if let Err(err) = result {
            // An error leaves the body's env in place, as it did when the body
            // wrote straight into it.
            for (k, v) in writes.iter() {
                env.insert_sym(*k, v.clone());
            }
            for k in removed {
                env.remove_sym(k);
            }
            *self.env_mut() = env;
            return Err(err);
        }
        self.set_current_package(saved);
        // Whether `key` is visible in the env the body ended with: a write it
        // made, or (over an intact tier) an entry of the enclosing env it did
        // not remove. On the fallback path `writes` IS that whole env.
        let tier_intact = matches!(writes, BlockWrites::Tier(_));
        let in_body_env = |env: &Env, key: Symbol| {
            writes.contains_key(key)
                || (tier_intact && !removed.contains(&key) && env.contains_key_sym(key))
        };
        // The block's own `my` lexicals (new env keys, not package-qualified or
        // internal) are dropped from the outer scope below (lexical scoping), but
        // a named sub defined in this `package Foo {...}` block closes over them and
        // is called by-name AFTER the block exits. Record them keyed by the package
        // so a `GetGlobal` miss inside Foo's subs can fall back to them. Stored
        // post-body, so the values reflect the block's assignments (e.g. zef's
        // `package Zef::CLI { my $CONFIG = preprocess-args-config-mutate(...); ... }`).
        let mut package_lexicals: Vec<(String, Value)> = Vec::new();
        let mut write_back: Vec<(Symbol, Value)> = Vec::new();
        for (k, v) in writes.iter() {
            let k = *k;
            let in_outer = env.contains_key_sym(k) && !declared.contains(&k);
            if k.contains_str("::") {
                if in_outer || !Self::is_block_binding_shape_marker(k) {
                    write_back.push((k, v.clone()));
                }
                // A new key that only contains `::` (it is not a qualified
                // name) is still considered as a package lexical below.
                if in_outer {
                    continue;
                }
            } else if in_outer {
                // A bare key the outer scope also had is normally a WRITE the
                // block made to an enclosing lexical (`my $x = 1; module M { $x
                // = 2 }`), and must propagate out. But an `our $x` DECLARATION
                // inside this block also leaves a bare key — its lexical alias —
                // and that is a NEW binding for the name, not a write to the
                // outer variable. Copying it back made `my $x = 'top'; module M
                // { our $x = 'our' }` leave the mainline `$x` reading 'our'.
                //
                // The two are told apart by the package-qualified twin: an `our`
                // declaration publishes `Pkg::x` alongside the bare alias, so a
                // qualified key this block introduced (absent from the outer
                // env) marks the bare key as this package's own `our` variable.
                // The twin must be NEW, so an outer `$M::x = ...` set before the
                // block does not suppress a genuine write-through.
                let twin = crate::qualified::qualified_var(name_sym, k);
                if !(in_body_env(&env, twin) && !env.contains_key_sym(twin)) {
                    write_back.push((k, v.clone()));
                }
                continue;
            }
            if crate::qualified::is_qualified(k) || k.starts_with("__") {
                continue;
            }
            let bare = k.resolve();
            // Per-frame special variables are never package-block lexicals a
            // named sub closes over: the topic `$_` (env "_"), `$/` ("/"),
            // `$!` ("!"), `?`-compile-time / `*`-dynamic names, `self`, and
            // digit capture keys ("0", "1", ...). A `use`/`for`/`given` in
            // the module body can leave a load-time topic in env; recording
            // it would make `package_scope_lexical` (consulted BEFORE env in
            // GetGlobal) permanently shadow every later per-call topic bind
            // inside this package's subs (Test::META's
            // `.map({ dist-dir.add($_) })` read the module-load leftover).
            // Mirrors the class-body static exclusion in
            // `register_class_decl`.
            if bare == "_"
                || bare == "/"
                || bare == "self"
                || bare.starts_with('?')
                || bare.starts_with('!')
                || bare.starts_with('*')
                || bare.chars().next().is_some_and(|c| c.is_ascii_digit())
            {
                continue;
            }
            // Record ONLY genuine `my` lexicals. An `our` package variable also
            // leaves a bare env key here, but it has a qualified twin in the
            // `our` store (`Pkg::name`) that is the authoritative value — and it
            // can be set from OUTSIDE the package (`$Pkg::msg = ...`), which this
            // bare snapshot would not see. Recording it would let the stale
            // declaration-time snapshot shadow the real `our` value on read
            // (`package_scope_lexical` is consulted before the `our` fallback).
            let qualified = crate::qualified::qualified(name_sym, k);
            if self.get_our_var(qualified.as_str()).is_some() || in_body_env(&env, qualified) {
                continue;
            }
            package_lexicals.push((bare, v.clone()));
        }
        // The declared element and key types of the block's `@`/`%` lexicals
        // live in env lanes (`__mutsu_type::%seen`,
        // `__mutsu_hash_key_type::%seen`) under names every package block
        // shares: left in the enclosing env, the next block declaring a
        // same-named typed hash overwrote them, and this block's routines then
        // checked their stores against the other's types (#12907, PDF's
        // `my %seen{Any}` next to `my %seen{Hash}`). Record them per package,
        // for the routines' entry to put back (`inject_class_body_statics`),
        // and keep them out of the enclosing env.
        let mut lane_keys: Vec<Symbol> = Vec::new();
        for (bare, _) in &package_lexicals {
            if bare.starts_with(['@', '%']) {
                // The block's subs enter with its `@`/`%` lexicals injected
                // (and the caller's same-named lanes cleared), as a class's
                // methods do with its statics.
                crate::runtime::cow_table_mut(&mut self.lexicals.class_body_static_names)
                    .entry(name.clone())
                    .or_default()
                    .insert(bare.clone());
                let sym = Symbol::intern(bare);
                lane_keys.push(Self::type_meta_key_for_sym(sym));
                if bare.starts_with('%') {
                    lane_keys.push(Self::hash_key_meta_key_for_sym(sym));
                }
            }
        }
        if !lane_keys.is_empty() {
            for (k, v) in writes.iter() {
                if lane_keys.contains(k)
                    && let ValueView::Str(tc) = v.view()
                {
                    crate::runtime::cow_table_mut(&mut self.lexicals.class_body_static_types)
                        .entry(name.clone())
                        .or_default()
                        .insert(k.resolve(), tc.as_str().to_owned());
                }
            }
            write_back.retain(|(k, _)| !lane_keys.contains(k));
        }
        // The block's subs enter with its `@`/`%` lexicals injected (and the
        // caller's same-named lanes cleared), as a class's methods do with
        // its statics.
        for (bare, _) in &package_lexicals {
            if bare.starts_with(['@', '%']) {
                crate::runtime::cow_table_mut(&mut self.lexicals.class_body_static_names)
                    .entry(name.clone())
                    .or_default()
                    .insert(bare.clone());
            }
        }
        drop(writes);
        if !package_lexicals.is_empty() {
            let store = self.package_lexicals_cow_mut().entry(name).or_default();
            for (bare, v) in package_lexicals {
                store.insert(bare, v);
            }
        }
        for (k, v) in write_back {
            env.insert_sym(k, v);
        }
        // Restore the locals the body touched, then pull back from `env` the
        // ones whose env mirror the compiler keeps live (`code.needs_env_sync`,
        // `compute_needs_env_sync` in `opcode.rs`), so a body's write to an
        // enclosing lexical (`my $x = 1; module M { $x = 2 }`) survives while a
        // body `my` that reuses an outer slot does not. A slot the body cannot
        // reach is left alone: restoring it would be a no-op.
        //
        // Only a slot whose mirror is live may be read from `env`: `env`'s bare
        // keys are pre-seeded with a decl-seed placeholder (`Any`) for every
        // mainline lexical, and a "slot-authoritative" local never overwrites
        // that placeholder on assignment, so reading it for any other slot
        // copied the placeholder over a live local
        // (`todo/tickets/package-block-resets-an-outer-lexical-declared-before-any-env-flush.md`).
        let sync = |idx: usize| code.needs_env_sync.get(idx).copied().unwrap_or(true);
        match touched {
            Some(slots) => {
                for (&slot, saved) in slots.iter().zip(saved_locals) {
                    let idx = slot as usize;
                    let synced = sync(idx)
                        .then(|| code.local_sym(idx))
                        .flatten()
                        .and_then(|sym| env.get_sym(sym).cloned());
                    self.locals[idx] = synced.unwrap_or(saved);
                }
            }
            None => {
                self.locals.refill_from(&saved_locals);
                for (idx, local_name) in code.locals.iter().enumerate() {
                    if !sync(idx) {
                        continue;
                    }
                    if let Some(val) = env.get(local_name).cloned() {
                        self.locals[idx] = val;
                    }
                }
            }
        }
        *self.env_mut() = env;
        *ip = body_end;
        Ok(())
    }

    /// Whether `key` is a marker recording the shape of a bare `:=` binding
    /// the block itself made (`my $x := (1, 2)`, `my $n := 42`). Its key
    /// contains `::`, so the qualified-key rule above would carry it out of
    /// the block, but it describes a binding that does not survive the block,
    /// and it stayed behind in every frame env of the program — 20 of them
    /// after `use JSON::Fast` (ADR-0084 §7.7, #7817).
    // Cost: O(|key|), a prefix comparison and a scan of the subject.
    fn is_block_binding_shape_marker(key: crate::symbol::Symbol) -> bool {
        use crate::meta_ns::MetaNs;
        key.with_str(|k| {
            k.strip_prefix(MetaNs::BoundDecont.prefix())
                .is_some_and(|subject| !crate::qualified::is_qualified_str(subject))
        })
    }
}
