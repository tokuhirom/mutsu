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
    // Cost: O(L + w + k) plus the body -- see the `OpCode::PackageScope` arm.
    pub(super) fn exec_package_scope_op(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
        body_end: u32,
        lexicals_idx: u32,
        ip: &mut usize,
        compiled_fns: &CompiledFns,
    ) -> Result<(), RuntimeError> {
        let name = Self::const_str(code, name_idx).to_string();
        let name_sym = code.const_sym(name_idx);
        let body_end = body_end as usize;
        let saved = self.current_package();
        let save = self.open_block_env_tier();
        let saved_locals = self.locals.to_vec();
        self.set_current_package(name.clone());
        let lexicals = (lexicals_idx != crate::opcode::NO_PACKAGE_LEXICALS)
            .then(|| Self::const_str(code, lexicals_idx));
        if let Some(lexicals) = lexicals {
            self.bind_package_body_lexicals(&name, lexicals);
        }
        let result = self.run_range(code, *ip + 1, body_end, compiled_fns);
        if let Some(lexicals) = lexicals {
            let saved_env = match &save {
                BlockEnvSave::Tier(parent) => &**parent,
                BlockEnvSave::Whole(env) => env,
            };
            self.unbind_package_body_lexicals(&name, lexicals, saved_env);
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
            let in_outer = env.contains_key_sym(k);
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
        self.locals.refill_from(&saved_locals);
        // Only pull a slot's value back from `env` when the compiler actually
        // keeps that slot's env mirror live (`code.needs_env_sync`,
        // `compute_needs_env_sync` in `opcode.rs`). `env`'s bare keys are
        // pre-seeded with a decl-seed placeholder (`Any`) for every mainline
        // lexical before any of them run, and the (B) per-store env-write gate
        // means a "slot-authoritative" local (the common case — nothing reads
        // it by name) never overwrites that placeholder on assignment. Reading
        // `env` unconditionally here — for every declared local in the whole
        // compiled unit, not just the ones this package/module body could
        // plausibly have touched — copied that stale placeholder straight over
        // a live, correctly-assigned local the block never went near
        // (`todo/tickets/package-block-resets-an-outer-lexical-declared-before-any-env-flush.md`):
        // `my $x = "top"; module M { }; say $x` read back `(Any)`. Gating on
        // `needs_env_sync` is the same test the write side already trusts, so
        // a slot whose env mirror IS kept current (referenced inside this or
        // another package-scope body, closed over, etc.) still reconciles
        // exactly as before — including the write-through case this loop
        // exists for (`my $x = 1; module M { $x = 2 }`, which the compiler
        // marks needs_env_sync via `EnvConsumerSlots::package_scope`).
        for (idx, local_name) in code.locals.iter().enumerate() {
            if !code.needs_env_sync.get(idx).copied().unwrap_or(true) {
                continue;
            }
            if let Some(val) = env.get(local_name).cloned() {
                self.locals[idx] = val;
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
    // Cost: O(|key|), two prefix comparisons and a scan of the subject.
    fn is_block_binding_shape_marker(key: crate::symbol::Symbol) -> bool {
        use crate::meta_ns::MetaNs;
        key.with_str(|k| {
            [MetaNs::BoundDecont, MetaNs::ScalarBindNoContainer]
                .iter()
                .find_map(|kind| k.strip_prefix(kind.prefix()))
                .is_some_and(|subject| !crate::qualified::is_qualified_str(subject))
        })
    }
}
