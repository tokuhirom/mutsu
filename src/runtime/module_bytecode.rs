//! Serving a module's mainline compile from the compiled-bytecode cache
//! (ADR-11756 §2).
//!
//! `load_module_inner` hands [`Interpreter::run_module_block`] a
//! [`ModuleCodeSlot`] naming the module's source. Only that block's main body
//! goes through [`Interpreter::compile_module_mainline`]. Its phaser queues,
//! everything the mainline later compiles, and every other compile take the
//! ordinary path.
//!
//! On a cache hit the slot is used only if all of the following hold:
//! - the recorded compile context equals the current one;
//! - every recorded input still gets its recorded answer;
//! - this process has not already compiled the module (occurrence 0 of its
//!   content-addressed session is unclaimed).
//!
//! Otherwise the module is compiled as before, under its content-addressed
//! session and with its inputs recorded. A first compile that the codec
//! accepts is then written to the cache.
//!
//! The feature is off unless `MUTSU_PRECOMP_BYTECODE=1`.
//! `MUTSU_PRECOMP_VERIFY=1` makes every hit compile anyway and compare the
//! two encodings byte for byte; a difference stops the run with status 70.

use super::Interpreter;
use crate::ast::Stmt;
use crate::compiler::compile_inputs::{self, Recorded};
use crate::compiler::compile_session;
use crate::opcode::{CompiledCode, CompiledFns};
use crate::precomp::bytecode::{CompileContext, CompileLatches};
use std::path::PathBuf;

/// One load of a module eligible for precompilation: its source, its identity,
/// and which load of it this is in the process.
///
/// The occurrence is claimed before the module is parsed, because the parse
/// already mints values that cached data carries: declaration ids and
/// anonymous type names come from the load's content-addressed parse session
/// (`anon_names::with_content_unit`). Only occurrence 0 may use, or write, the
/// precompilation cache. A second load of the same file in one process
/// re-parses and recompiles under fresh values.
pub(crate) struct ModuleUnit {
    source_path: PathBuf,
    source: String,
    unit_key: u64,
    occurrence: u32,
}

impl ModuleUnit {
    /// Claim this load of the module at `source_path`, or `None` when it is not
    /// eligible for precompilation (then everything mints from the process
    /// counters, as before).
    // Cost: O(n), n = source bytes (read and hashed once).
    pub(crate) fn claim(interp: &Interpreter, source_path: &std::path::Path) -> Option<Self> {
        let source = std::fs::read_to_string(source_path).ok()?;
        if !interp.module_precomp_eligible(&source) {
            return None;
        }
        let unit_key = {
            use std::hash::{Hash, Hasher};
            let mut hasher = std::hash::DefaultHasher::new();
            source_path.hash(&mut hasher);
            crate::precomp::content_hash(source.as_bytes()).hash(&mut hasher);
            hasher.finish()
        };
        let occurrence = compile_session::claim_next_occurrence(unit_key);
        Some(ModuleUnit {
            source_path: source_path.to_path_buf(),
            source,
            unit_key,
            occurrence,
        })
    }

    /// Whether this load may read and write the precompilation cache.
    pub(crate) fn may_use_cache(&self) -> bool {
        self.occurrence == 0
    }

    /// The session the module's parse mints its names and ids from.
    pub(crate) fn parse_session(&self) -> u64 {
        compile_session::content_parse_session_id(self.unit_key, self.occurrence)
    }
}

/// The module whose mainline the next [`Interpreter::run_module_block`] runs.
pub(crate) struct ModuleCodeSlot {
    unit: ModuleUnit,
    guards_fingerprint: u64,
}

impl ModuleCodeSlot {
    /// A slot for `unit`, or `None` when the bytecode cache is off. `guards`
    /// are the statements `load_module_inner` spliced into the AST.
    // Cost: O(g), g = size of the guards.
    pub(crate) fn new(unit: Option<ModuleUnit>, guards: &[Stmt]) -> Option<Self> {
        let unit = unit?;
        if !enabled() {
            return None;
        }
        let guards_fingerprint = crate::ast::function_body_fingerprint(&[], &[], guards);
        Some(ModuleCodeSlot {
            unit,
            guards_fingerprint,
        })
    }
}

// Cost: O(1) after the first call.
fn enabled() -> bool {
    static ON: std::sync::OnceLock<bool> = std::sync::OnceLock::new();
    *ON.get_or_init(|| std::env::var("MUTSU_PRECOMP_BYTECODE").is_ok_and(|v| v != "0"))
}

// Cost: O(1) after the first call.
fn trace_enabled() -> bool {
    static ON: std::sync::OnceLock<bool> = std::sync::OnceLock::new();
    *ON.get_or_init(|| std::env::var("MUTSU_PRECOMP_TRACE").is_ok_and(|v| v != "0"))
}

/// `MUTSU_PRECOMP_TRACE`: one stderr line per module mainline compile, saying
/// how the cache answered.
fn trace(slot: &ModuleCodeSlot, what: &str) {
    if trace_enabled() {
        eprintln!(
            "precomp bytecode: {}: {what}",
            slot.unit.source_path.display()
        );
    }
}

// Cost: O(1) after the first call.
fn verify_enabled() -> bool {
    static ON: std::sync::OnceLock<bool> = std::sync::OnceLock::new();
    *ON.get_or_init(|| std::env::var("MUTSU_PRECOMP_VERIFY").is_ok_and(|v| v != "0"))
}

impl Interpreter {
    /// Compile a module's mainline `stmts`, from the cache when it can serve
    /// them. See the module docs.
    // Cost: O(n), n = size of the compiled code (decoding, or compiling).
    pub(crate) fn compile_module_mainline(
        &self,
        stmts: &[Stmt],
        slot: &ModuleCodeSlot,
    ) -> (CompiledCode, CompiledFns) {
        let (compiler, compile_unit) = self.block_compiler();
        let context = CompileContext {
            environment: compile_inputs::environment_fingerprint(),
            is_routine: compiler.is_routine,
            current_package: self.current_package(),
            enclosing_package: compiler.enclosing_package.clone(),
            has_distribution: compiler.current_distribution.is_some(),
            unit_file: compile_unit.map(|s| s.as_str().to_string()),
            guards_fingerprint: slot.guards_fingerprint,
        };
        let _unit_file = crate::unit_source_file::UnitSourceFileGuard::enter(compile_unit);
        let unit = &slot.unit;
        let session = compile_session::content_session_id(unit.unit_key, unit.occurrence);
        let cached = if unit.may_use_cache() {
            crate::precomp::bytecode::load_cached_code(&unit.source_path, &unit.source, &context)
                .filter(|cached| cached.inputs.still_hold())
                .and_then(|cached| {
                    let decoded = crate::precomp_codec::decode_compiled(&cached.payload).ok()?;
                    Some((cached.latches, decoded))
                })
        } else {
            None
        };
        let (mut code, mut fns) = match cached {
            Some((latches, decoded)) => {
                trace(slot, "hit");
                crate::opcode::replay_compile_latches(
                    latches.reflective_name_access,
                    latches.dispatcher,
                );
                if verify_enabled() {
                    let _session = compile_session::enter_session(session);
                    let fresh = compiler.compile(stmts);
                    verify_same(&unit.source_path, &decoded, &fresh);
                }
                decoded
            }
            None => {
                let _session = compile_session::enter_session(session);
                let recording = compile_inputs::start();
                let compiled = compiler.compile(stmts);
                let recorded = recording.map(compile_inputs::RecordingGuard::finish);
                let encoded = match (&recorded, unit.may_use_cache()) {
                    (Some(Recorded::Cacheable(_)), true) => {
                        crate::precomp_codec::encode_compiled(&compiled.0, &compiled.1)
                            .map_err(|e| trace(slot, &format!("not cached: {e}")))
                            .ok()
                    }
                    (Some(Recorded::Uncacheable(reason)), _) => {
                        trace(slot, &format!("not cached: {reason}"));
                        None
                    }
                    _ => {
                        trace(slot, &format!("not cached: load {}", unit.occurrence));
                        None
                    }
                };
                if let (Some(Recorded::Cacheable(inputs)), Some(payload)) = (recorded, encoded) {
                    trace(slot, "compiled and cached");
                    let latches = CompileLatches {
                        reflective_name_access: crate::opcode::reflective_name_access_possible(),
                        dispatcher: crate::opcode::dispatcher_possible(),
                    };
                    crate::precomp::bytecode::save_cached_code(
                        &unit.source_path,
                        &unit.source,
                        context,
                        inputs,
                        latches,
                        &payload,
                    );
                }
                compiled
            }
        };
        self.inherit_frame_lexical_for_body(stmts, &mut code, &mut fns);
        (code, fns)
    }
}

/// `MUTSU_PRECOMP_VERIFY`: the cached chunk must encode exactly as a fresh
/// compile of the same module under the same session does.
// Cost: O(n), n = size of the compiled code (two encodings).
fn verify_same(
    source_path: &std::path::Path,
    cached: &(CompiledCode, CompiledFns),
    fresh: &(CompiledCode, CompiledFns),
) {
    let encode = |pair: &(CompiledCode, CompiledFns)| {
        crate::precomp_codec::encode_compiled(&pair.0, &pair.1)
    };
    match (encode(cached), encode(fresh)) {
        (Ok(a), Ok(b)) if a == b => {}
        (a, b) => {
            let detail = match (&a, &b) {
                (Ok(a), Ok(b)) => crate::precomp_codec::first_difference(cached, fresh)
                    .unwrap_or_else(|| crate::precomp_codec::describe_difference(a, b)),
                _ => format!(
                    "{:?} vs {:?}",
                    a.as_ref().map(Vec::len),
                    b.as_ref().map(Vec::len)
                ),
            };
            eprintln!(
                "precomp verify: the cached compile of {} differs from a fresh compile: {detail}",
                source_path.display(),
            );
            std::process::exit(70);
        }
    }
}
