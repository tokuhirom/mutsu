//! Record and replay of a precompiled module's BEGIN-time values (ADR-18012
//! stage S1).
//!
//! A module whose compiled mainline comes from the precompilation cache used
//! to run its value-position `BEGIN` bodies again on every load. uniname-words
//! builds a table over every code point that way: ~10 s on every warm load.
//! Rakudo serializes what BEGIN left behind and runs none of it on a warm load.
//!
//! Here, the first load that runs a module's mainline **records**, for each
//! `BEGIN` expression of that mainline ([`crate::opcode::OpCode::BeginOnceExpr`])
//! that it can prove replay-safe, the value the body produced and the unit
//! lexicals the body changed. The log is stored next to the compiled entry
//! ([`crate::precomp::begin_log`]). A later load that is served that compiled
//! entry **replays**: it binds the recorded lexicals, pushes the recorded value
//! and skips the body. Replay calls no Raku code (ADR-18012 §2.1).
//!
//! A body is recorded only when it is replay-safe by construction (ADR-18012
//! §2.3); every other body runs on every load, as before:
//! - [`scan`]: every opcode of the body is on a small allow-list (no
//!   declaration, no closure, no read of a global or dynamic variable, no
//!   output), and every method, `nqp::` op and routine it names is one whose
//!   built-in form only computes a value;
//! - [`watch`]: while the body runs, no other compiled code runs. A user
//!   routine, method, closure, `augment`ed built-in or regex code block is
//!   other compiled code, so reaching one makes the body unrecordable;
//! - the unit lexicals the body could reach are values the log can hold, so a
//!   change to them is seen by comparing their encodings before and after;
//! - the value and every changed lexical encode ([`crate::value::PortableValue`]).
//!
//! `MUTSU_BEGIN_REPLAY=0` turns recording and replay off (every body runs, as
//! before this module).

pub(crate) mod scan;
pub(crate) mod watch;

use std::cell::RefCell;
use std::collections::HashMap;
use std::path::PathBuf;

use crate::opcode::CompiledCode;
use crate::value::{PortableValue, Value};

/// Whether recording and replay are on (`MUTSU_BEGIN_REPLAY=0` turns them off).
// Cost: O(1) after the first call.
pub(crate) fn enabled() -> bool {
    static ENABLED: std::sync::OnceLock<bool> = std::sync::OnceLock::new();
    *ENABLED.get_or_init(|| std::env::var("MUTSU_BEGIN_REPLAY").map_or(true, |v| v != "0"))
}

/// One recorded `BEGIN` expression.
#[derive(serde::Serialize, serde::Deserialize)]
pub(crate) struct SiteRecord {
    site_id: u64,
    value: PortableValue,
    /// The unit lexicals the body changed: slot and final value.
    locals: Vec<(u32, PortableValue)>,
}

/// The log of one module mainline.
#[derive(serde::Serialize, serde::Deserialize, Default)]
struct BeginLog {
    sites: Vec<SiteRecord>,
}

/// What a module mainline run does with its `BEGIN` expressions.
pub(crate) enum Plan {
    /// Record replay-safe bodies, then store the log for the compiled entry
    /// with this checksum.
    Record { source_path: PathBuf, checksum: u64 },
    /// Replay these recorded bodies.
    Replay { sites: HashMap<u64, SiteRecord> },
}

impl Plan {
    /// The plan for a module served the compiled entry with `checksum`: replay
    /// its log if one is stored, otherwise record one.
    // Cost: O(n), n = size of the stored log.
    pub(crate) fn for_entry(source_path: PathBuf, checksum: u64) -> Option<Plan> {
        if !enabled() {
            return None;
        }
        let Some(body) = crate::precomp::begin_log::load_begin_log(&source_path, checksum) else {
            return Some(Plan::Record {
                source_path,
                checksum,
            });
        };
        let (log, _): (BeginLog, usize) =
            bincode::serde::decode_from_slice(&body, crate::precomp::decode_config()).ok()?;
        Some(Plan::Replay {
            sites: log.sites.into_iter().map(|s| (s.site_id, s)).collect(),
        })
    }

    /// The plan for a module whose entry was just written with `checksum`.
    // Cost: O(1).
    pub(crate) fn record(source_path: PathBuf, checksum: u64) -> Option<Plan> {
        enabled().then_some(Plan::Record {
            source_path,
            checksum,
        })
    }
}

/// A module mainline being run under a plan.
struct Frame {
    /// The mainline's code object: only its own `BEGIN` expressions count.
    code: usize,
    plan: Plan,
    recorded: Vec<SiteRecord>,
}

thread_local! {
    static FRAMES: RefCell<Vec<Frame>> = const { RefCell::new(Vec::new()) };
}

// Cost: O(1).
fn code_key(code: &CompiledCode) -> usize {
    std::ptr::from_ref(code) as usize
}

/// Run a module mainline `code` under `plan`; the returned guard ends it.
// Cost: O(1).
pub(crate) fn begin_mainline(code: &CompiledCode, plan: Option<Plan>) -> MainlineGuard {
    let Some(plan) = plan else {
        return MainlineGuard { active: false };
    };
    FRAMES.with(|frames| {
        frames.borrow_mut().push(Frame {
            code: code_key(code),
            plan,
            recorded: Vec::new(),
        });
    });
    MainlineGuard { active: true }
}

/// Ends a mainline run begun with [`begin_mainline`].
pub(crate) struct MainlineGuard {
    active: bool,
}

impl MainlineGuard {
    /// The mainline finished, `ok` saying whether without an error. A recording
    /// run that finished stores its log; one that died stores nothing, so the
    /// next load records again.
    // Cost: O(n), n = size of the log (encoded and written once).
    pub(crate) fn finish(mut self, ok: bool) {
        self.active = false;
        let Some(frame) = FRAMES.with(|frames| frames.borrow_mut().pop()) else {
            return;
        };
        if !ok {
            return;
        }
        if let Plan::Record {
            source_path,
            checksum,
        } = frame.plan
        {
            let log = BeginLog {
                sites: frame.recorded,
            };
            if let Ok(body) = bincode::serde::encode_to_vec(&log, bincode::config::standard()) {
                crate::precomp::begin_log::save_begin_log(&source_path, checksum, &body);
            }
        }
    }
}

impl Drop for MainlineGuard {
    fn drop(&mut self) {
        if self.active {
            FRAMES.with(|frames| frames.borrow_mut().pop());
        }
    }
}

/// What the innermost mainline run does with a `BEGIN` expression of `code`.
pub(crate) enum SiteAction {
    /// Not a recorded mainline's own `BEGIN`: run it as always.
    Run,
    /// Run it, and record it if it proves replay-safe.
    Record,
    /// Skip the body: bind these lexicals and push this value.
    Replay {
        value: Value,
        locals: Vec<(usize, Value)>,
    },
}

/// The action for the `BEGIN` expression `site_id` of `code`. A replayed
/// record is taken out of the plan: a mainline reaches each of its `BEGIN`
/// expressions once.
// Cost: O(1) to answer Run or Record; O(v) to answer Replay, v = size of the
// recorded values (decoded fresh, as a fresh compile builds them).
pub(crate) fn site_action(code: &CompiledCode, site_id: u64) -> SiteAction {
    let record = FRAMES.with(|frames| {
        let mut frames = frames.borrow_mut();
        if let Some(top) = frames.last()
            && top.code != code_key(code)
        {
            trace(&format!("BEGIN {site_id}: not the recorded mainline's own"));
        }
        let frame = frames.last_mut().filter(|f| f.code == code_key(code))?;
        match &mut frame.plan {
            Plan::Record { .. } => Some(None),
            Plan::Replay { sites } => sites.remove(&site_id).map(Some),
        }
    });
    match record {
        None => SiteAction::Run,
        Some(None) => SiteAction::Record,
        Some(Some(record)) => {
            trace(&format!("replayed BEGIN {site_id}"));
            SiteAction::Replay {
            value: record.value.into_value(),
            locals: record
                .locals
                .into_iter()
                .map(|(slot, v)| (slot as usize, v.into_value()))
                .collect(),
            }
        }
    }
}

/// `MUTSU_PRECOMP_TRACE`: one stderr line per recorded or replayed `BEGIN`.
// Cost: O(1) when tracing is off.
pub(crate) fn trace(what: &str) {
    if super::module_bytecode::trace_enabled() {
        eprintln!("precomp begin: {what}");
    }
}

/// Record that the `BEGIN` expression `site_id` of the innermost recording
/// mainline produced `value` and left `locals` changed. Nothing is recorded
/// when any of them cannot be encoded: that body then runs on every load.
// Cost: O(v), v = size of the values (encoded).
pub(crate) fn record_site(site_id: u64, value: &Value, locals: &[(usize, Value)]) {
    let Ok(value) = PortableValue::from_value(value) else {
        trace(&format!("BEGIN {site_id}: value not recordable"));
        return;
    };
    let mut encoded = Vec::with_capacity(locals.len());
    for (slot, v) in locals {
        let (Ok(slot), Ok(v)) = (u32::try_from(*slot), PortableValue::from_value(v)) else {
            trace(&format!("BEGIN {site_id}: a changed lexical is not recordable"));
            return;
        };
        encoded.push((slot, v));
    }
    FRAMES.with(|frames| {
        if let Some(frame) = frames.borrow_mut().last_mut() {
            trace(&format!("recorded BEGIN {site_id}"));
            frame.recorded.push(SiteRecord {
                site_id,
                value,
                locals: encoded,
            });
        }
    });
}

/// The encoding of `value` for comparing a lexical before and after a body:
/// `None` for a value the log cannot hold.
// Cost: O(v), v = size of the value.
pub(crate) fn encoding_of(value: &Value) -> Option<Vec<u8>> {
    let portable = PortableValue::from_value(value).ok()?;
    bincode::serde::encode_to_vec(&portable, bincode::config::standard()).ok()
}
