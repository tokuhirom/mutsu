//! What a compile read besides its AST (ADR-11756 §2.2).
//!
//! A cached compile result may be reused only if a fresh compile now would see
//! the same inputs. The compiler's reads of state outside the `stmts` it is
//! handed therefore go through the wrappers here. While a recording is open
//! (the compile of a cacheable module mainline), each wrapper logs its
//! question and the answer it got. A later load re-asks every logged question
//! ([`CompileInputs::still_hold`]) and uses the cached result only if every
//! answer is unchanged.
//!
//! A read that cannot be summarised as a question and an answer, such as a
//! compile-time re-parse (it consults the parser's whole scope state), marks
//! the compile uncacheable instead.
//!
//! Outside a recording, every wrapper is a plain pass-through.

use std::cell::RefCell;
use std::collections::BTreeMap;

/// One question a compile asked of state outside its AST.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, serde::Serialize, serde::Deserialize)]
pub(crate) enum InputKey {
    /// `parser::is_imported_function(name)`.
    ImportedFunction(String),
    /// `parser::is_user_declared_type(name)`.
    UserDeclaredType(String),
    /// `parser::is_user_declared_enum_value(name)`.
    UserDeclaredEnumValue(String),
    /// `parser::current_language_version_starts_with(prefix)`.
    LanguageVersionStartsWith(String),
    /// `nqp_ops_sys::uname_const_value(name)`, folded into a constant.
    UnameConst(String),
}

/// The answer a question got.
#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub(crate) enum InputValue {
    Bool(bool),
    Int(Option<i64>),
}

/// Everything one recorded compile asked, with the answers it got.
#[derive(Debug, Default, Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub(crate) struct CompileInputs {
    answers: BTreeMap<InputKey, InputValue>,
}

impl CompileInputs {
    /// Whether every recorded question still gets the recorded answer.
    // Cost: O(q), q = recorded questions (each re-asked once).
    pub(crate) fn still_hold(&self) -> bool {
        self.answers.iter().all(|(key, value)| ask(key) == *value)
    }

    /// Number of recorded questions.
    #[cfg(test)]
    pub(crate) fn len(&self) -> usize {
        self.answers.len()
    }
}

/// The result of a finished recording.
pub(crate) enum Recorded {
    /// The compile may be cached under these inputs.
    Cacheable(CompileInputs),
    /// The compile read something that cannot be validated later.
    Uncacheable(&'static str),
}

#[derive(Default)]
struct Recorder {
    inputs: CompileInputs,
    uncacheable: Option<&'static str>,
}

thread_local! {
    static RECORDING: RefCell<Option<Recorder>> = const { RefCell::new(None) };
}

/// Keeps a recording open; [`RecordingGuard::finish`] closes it.
pub(crate) struct RecordingGuard {
    finished: bool,
}

impl RecordingGuard {
    /// Close the recording and return what it captured.
    // Cost: O(1).
    pub(crate) fn finish(mut self) -> Recorded {
        self.finished = true;
        let recorder = RECORDING
            .with(|r| r.borrow_mut().take())
            .unwrap_or_default();
        match recorder.uncacheable {
            Some(reason) => Recorded::Uncacheable(reason),
            None => Recorded::Cacheable(recorder.inputs),
        }
    }
}

impl Drop for RecordingGuard {
    fn drop(&mut self) {
        if !self.finished {
            RECORDING.with(|r| *r.borrow_mut() = None);
        }
    }
}

/// Open a recording for the compile about to run. `None` if one is already
/// open on this thread: a compile nested in a recorded one is part of it, and
/// its reads are logged by the outer recording.
// Cost: O(1).
pub(crate) fn start() -> Option<RecordingGuard> {
    RECORDING.with(|r| {
        let mut r = r.borrow_mut();
        if r.is_some() {
            return None;
        }
        *r = Some(Recorder::default());
        Some(RecordingGuard { finished: false })
    })
}

// Cost: O(log q), q = questions recorded so far.
fn record(key: InputKey, value: InputValue) {
    RECORDING.with(|r| {
        if let Some(recorder) = r.borrow_mut().as_mut() {
            recorder.inputs.answers.insert(key, value);
        }
    });
}

/// Mark the open recording (if any) uncacheable.
// Cost: O(1).
pub(crate) fn mark_uncacheable(reason: &'static str) {
    RECORDING.with(|r| {
        if let Some(recorder) = r.borrow_mut().as_mut() {
            recorder.uncacheable.get_or_insert(reason);
        }
    });
}

/// Ask a question against the current state, without recording it.
// Cost: O(1) per question (each is a scope lookup or a table probe).
fn ask(key: &InputKey) -> InputValue {
    match key {
        InputKey::ImportedFunction(name) => {
            InputValue::Bool(crate::parser::is_imported_function(name))
        }
        InputKey::UserDeclaredType(name) => {
            InputValue::Bool(crate::parser::is_user_declared_type(name))
        }
        InputKey::UserDeclaredEnumValue(name) => {
            InputValue::Bool(crate::parser::is_user_declared_enum_value(name))
        }
        InputKey::LanguageVersionStartsWith(prefix) => {
            InputValue::Bool(crate::parser::current_language_version_starts_with(prefix))
        }
        InputKey::UnameConst(name) => {
            InputValue::Int(crate::runtime::nqp_ops_sys::uname_const_value(name))
        }
    }
}

// Cost: O(1) plus the recording probe.
fn ask_and_record(key: InputKey) -> InputValue {
    let value = ask(&key);
    record(key, value.clone());
    value
}

/// Recorded `parser::is_imported_function`.
pub(crate) fn is_imported_function(name: &str) -> bool {
    matches!(
        ask_and_record(InputKey::ImportedFunction(name.to_string())),
        InputValue::Bool(true)
    )
}

/// Recorded `parser::is_user_declared_type`.
pub(crate) fn is_user_declared_type(name: &str) -> bool {
    matches!(
        ask_and_record(InputKey::UserDeclaredType(name.to_string())),
        InputValue::Bool(true)
    )
}

/// Recorded `parser::is_user_declared_enum_value`.
pub(crate) fn is_user_declared_enum_value(name: &str) -> bool {
    matches!(
        ask_and_record(InputKey::UserDeclaredEnumValue(name.to_string())),
        InputValue::Bool(true)
    )
}

/// Recorded `parser::current_language_version_starts_with`.
pub(crate) fn current_language_version_starts_with(prefix: &str) -> bool {
    matches!(
        ask_and_record(InputKey::LanguageVersionStartsWith(prefix.to_string())),
        InputValue::Bool(true)
    )
}

/// Recorded `nqp_ops_sys::uname_const_value`.
pub(crate) fn uname_const_value(name: &str) -> Option<i64> {
    match ask_and_record(InputKey::UnameConst(name.to_string())) {
        InputValue::Int(v) => v,
        InputValue::Bool(_) => None,
    }
}

/// The process environment the compiler reads once and keeps
/// (`MUTSU_NO_SHADOW_SLOTS`, `MUTSU_CONST_FOLD`, `MUTSU_SLOT_READ_FILTER`), as
/// one string a cache entry is keyed on.
// Cost: O(1) (three environment reads).
pub(crate) fn environment_fingerprint() -> String {
    let var = |name: &str| std::env::var(name).unwrap_or_else(|_| "\u{0}".to_string());
    format!(
        "{}|{}|{}",
        var("MUTSU_NO_SHADOW_SLOTS"),
        var("MUTSU_CONST_FOLD"),
        var("MUTSU_SLOT_READ_FILTER")
    )
}

#[cfg(test)]
#[path = "compile_inputs_tests.rs"]
mod tests;
