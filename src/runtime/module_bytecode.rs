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
//! The feature is on by default (ADR-11756 step 5);
//! `MUTSU_PRECOMP_BYTECODE=0` turns it off, as does `--no-precomp`.
//! `MUTSU_PRECOMP_VERIFY=1` makes every hit compile anyway and compare the
//! two encodings byte for byte; a difference stops the run with status 70.
//!
//! The entry is looked up when the load claims its [`ModuleUnit`], before
//! anything is parsed (ADR-12026 §2.1): a load that finds one takes its parse
//! effects, load facts and AST from it and never reads the AST cache. Whether
//! its *compile* fits is decided here, when the mainline is about to run; a
//! compile that does not fit is redone from the entry's own AST.

use super::Interpreter;
use crate::ast::Stmt;
use crate::compiler::compile_inputs::{self, Recorded};
use crate::compiler::compile_session;
use crate::opcode::{CompiledCode, CompiledFns};
use crate::precomp::ParseEffects;
use crate::precomp::bytecode::{CodeEntry, CompileContext, CompileLatches, NewCodeEntry};
use crate::value::RuntimeError;
use std::path::PathBuf;

use super::module_load_facts::{ModuleAst, ModuleLoadFacts};

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
    pub(crate) source: String,
    unit_key: u64,
    occurrence: u32,
    /// The precompilation entry for exactly this source, when the cache may
    /// serve this load and has one.
    entry: Option<CodeEntry>,
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
        let entry = (occurrence == 0 && enabled())
            .then(|| crate::precomp::bytecode::load_code_entry(source_path, &source))
            .flatten();
        Some(ModuleUnit {
            source_path: source_path.to_path_buf(),
            source,
            unit_key,
            occurrence,
            entry,
        })
    }

    /// The precompilation entry found for this load, taken out of the unit.
    pub(crate) fn take_entry(&mut self) -> Option<CodeEntry> {
        self.entry.take()
    }

    /// Whether this load may read and write the precompilation cache.
    pub(crate) fn may_use_cache(&self) -> bool {
        self.occurrence == 0
    }

    /// The session the module's parse mints its names and ids from.
    pub(crate) fn parse_session(&self) -> u64 {
        compile_session::content_parse_session_id(self.unit_key, self.occurrence)
    }

    /// The session the post-parse rewrites of the module's AST (the BEGIN
    /// prologue's slot names) mint from: apart from the parse's, because a
    /// cached AST skips the parse but not these rewrites.
    pub(crate) fn rewrite_session(&self) -> u64 {
        compile_session::content_parse_session_id(self.unit_key ^ REWRITE_SALT, self.occurrence)
    }
}

/// Keeps [`ModuleUnit::rewrite_session`] apart from the parse session.
const REWRITE_SALT: u64 = 0x7265_7772_6974_6521;

/// The module whose mainline the next [`Interpreter::run_module_block`] runs.
pub(crate) struct ModuleCodeSlot {
    unit: ModuleUnit,
    /// The compile half of the entry the load found, if any.
    cached: Option<CachedCompile>,
    /// What a new entry stores besides the compile.
    effects: ParseEffects,
    facts: ModuleLoadFacts,
    ast: Vec<u8>,
    guards_fingerprint: u64,
}

/// The compile half of a [`CodeEntry`].
struct CachedCompile {
    context: CompileContext,
    inputs: crate::compiler::compile_inputs::CompileInputs,
    latches: CompileLatches,
    payload: Vec<u8>,
}

/// What a load knows about its module's mainline when it sets up the slot.
pub(crate) struct MainlineParts<'a> {
    pub(crate) unit: Option<ModuleUnit>,
    /// The entry the load was served from, if any.
    pub(crate) entry: Option<CodeEntry>,
    pub(crate) effects: ParseEffects,
    pub(crate) facts: &'a ModuleLoadFacts,
    /// The AST before its guards are spliced in.
    pub(crate) ast: &'a ModuleAst,
    pub(crate) guards: &'a [Stmt],
}

impl ModuleCodeSlot {
    /// A slot for the load's unit, or `None` when the bytecode cache is off
    /// or the unit may not use it.
    // Cost: O(n) on a miss (the AST is encoded for the entry), O(g) on a
    // hit, g = size of the guards.
    pub(crate) fn new(parts: MainlineParts<'_>) -> Result<Option<Self>, RuntimeError> {
        let Some(unit) = parts.unit else {
            return Ok(None);
        };
        if !enabled() || !unit.may_use_cache() {
            return Ok(None);
        }
        let (ast, cached) = match parts.entry {
            Some(entry) => (
                entry.ast,
                Some(CachedCompile {
                    context: entry.context,
                    inputs: entry.inputs,
                    latches: entry.latches,
                    payload: entry.payload,
                }),
            ),
            None => match crate::precomp::encode_stmts(parts.ast.stmts()?) {
                Some(ast) => (ast, None),
                None => return Ok(None),
            },
        };
        Ok(Some(ModuleCodeSlot {
            unit,
            cached,
            effects: parts.effects,
            facts: parts.facts.clone(),
            ast,
            guards_fingerprint: crate::ast::stable_hash::stable_hash(parts.guards),
        }))
    }
}

/// The statements a module's mainline compile is handed: the whole module,
/// or the body left after its block phasers were split off.
pub(crate) enum MainlineBody<'a> {
    Module(&'a ModuleAst),
    Split(&'a [Stmt]),
}

impl MainlineBody<'_> {
    // Cost: O(n) on the first request for an encoded AST, else O(1).
    fn stmts(&self) -> Result<&[Stmt], RuntimeError> {
        match self {
            MainlineBody::Module(ast) => ast.stmts(),
            MainlineBody::Split(stmts) => Ok(stmts),
        }
    }
}

// Cost: O(1) after the first call.
fn enabled() -> bool {
    static ON: std::sync::OnceLock<bool> = std::sync::OnceLock::new();
    *ON.get_or_init(|| std::env::var("MUTSU_PRECOMP_BYTECODE").map_or(true, |v| v != "0"))
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
    /// Compile a module's mainline `body`, from the cache when it can serve
    /// it. See the module docs.
    // Cost: O(n), n = size of the compiled code (decoding, or compiling).
    pub(crate) fn compile_module_mainline(
        &self,
        body: MainlineBody<'_>,
        slot: &ModuleCodeSlot,
    ) -> Result<(CompiledCode, CompiledFns), RuntimeError> {
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
        let cached = slot
            .cached
            .as_ref()
            .filter(|cached| cached.context == context && cached.inputs.still_hold())
            .and_then(|cached| {
                let decoded = crate::precomp_codec::decode_compiled(&cached.payload).ok()?;
                Some((cached.latches, decoded))
            });
        let (mut code, mut fns) = match cached {
            Some((latches, decoded)) => {
                trace(slot, "hit");
                crate::opcode::replay_compile_latches(
                    latches.reflective_name_access,
                    latches.dispatcher,
                );
                if verify_enabled() {
                    let _session = compile_session::enter_session(session);
                    let fresh = compiler.compile(body.stmts()?);
                    verify_same(&unit.source_path, &decoded, &fresh);
                }
                decoded
            }
            None => {
                let stmts = body.stmts()?;
                let _session = compile_session::enter_session(session);
                let recording = compile_inputs::start();
                let compiled = compiler.compile(stmts);
                let recorded = recording.map(compile_inputs::RecordingGuard::finish);
                let encoded = match &recorded {
                    Some(Recorded::Cacheable(_)) => {
                        crate::precomp_codec::encode_compiled(&compiled.0, &compiled.1)
                            .map_err(|e| trace(slot, &format!("not cached: {e}")))
                            .ok()
                    }
                    Some(Recorded::Uncacheable(reason)) => {
                        trace(slot, &format!("not cached: {reason}"));
                        None
                    }
                    None => None,
                };
                if let (Some(Recorded::Cacheable(inputs)), Some(payload)) = (recorded, encoded) {
                    trace(slot, "compiled and cached");
                    let latches = CompileLatches {
                        reflective_name_access: crate::opcode::reflective_name_access_possible(),
                        dispatcher: crate::opcode::dispatcher_possible(),
                    };
                    crate::precomp::bytecode::save_code_entry(
                        &unit.source_path,
                        &unit.source,
                        NewCodeEntry {
                            context,
                            inputs,
                            latches,
                            effects: &slot.effects,
                            facts: &slot.facts,
                            ast: &slot.ast,
                            payload: &payload,
                        },
                    );
                }
                compiled
            }
        };
        if let MainlineBody::Split(stmts) = body {
            self.inherit_frame_lexical_for_body(stmts, &mut code, &mut fns);
        }
        Ok((code, fns))
    }

    /// `MUTSU_PRECOMP_VERIFY`: the load facts an entry served must equal the
    /// facts computed afresh from the entry's AST.
    // Cost: O(n), n = size of the AST (decoded and walked again).
    pub(crate) fn verify_load_facts(
        source_path: &std::path::Path,
        ast: &ModuleAst,
        recorded: &ModuleLoadFacts,
    ) -> Result<(), RuntimeError> {
        if !verify_enabled() {
            return Ok(());
        }
        let fresh = ModuleLoadFacts::compute(&ast.decode_unguarded()?, recorded.prologue_len);
        if fresh != *recorded {
            eprintln!(
                "precomp verify: the load facts recorded for {} differ from fresh ones: {:?} != {:?}",
                source_path.display(),
                recorded,
                fresh,
            );
            std::process::exit(70);
        }
        Ok(())
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
