//! The term generator of a `...` sequence, as data: how the next term is
//! derived from the previous ones (`SeqMode`), where the sequence ends
//! (`EndpointKind`), and the per-sequence facts derived once from its seeds.
//!
//! `Interpreter::eval_sequence` builds one and runs it for an eager prefix
//! (`Interpreter::run_seq_gen`). A finite sequence whose prefix ends before
//! its endpoint keeps the generator in its lazy list
//! (`ClosureSeqState::native`), so pulling past the prefix resumes the same
//! generation instead of the sequence being cut off at the prefix length.

use super::{SeqClosureCount, SeqGeneratorShape, Value};

/// How the next term of a sequence is computed.
#[derive(Clone, Debug)]
pub(crate) enum SeqMode {
    /// `last + step` (or `.succ`/`.pred` for a string or a non-numeric term).
    Arithmetic(f64),
    /// `last * ratio`, inexact.
    Geometric(f64),
    /// `last * num / den`, exact.
    GeometricRat(i64, i64),
    /// A user generator closure.
    Closure,
}

/// What ends a sequence.
#[derive(Clone, Debug)]
pub(crate) enum EndpointKind {
    /// A value (or a type object) the sequence stops at or passes.
    Value(Value),
    /// A predicate closure.
    Closure(Value),
    /// A regex pattern a term's string form matches.
    Regex(String),
}

/// Everything the generation step reads, fixed for the whole sequence.
#[derive(Clone, Debug)]
pub(crate) struct SeqGen {
    pub(crate) mode: SeqMode,
    pub(crate) endpoint_kind: Option<EndpointKind>,
    /// `...^`: the endpoint itself is not a term.
    pub(crate) exclusive: bool,
    /// The first seed: a geometric sequence's reachability test reads it.
    pub(crate) first_seed: Option<Value>,
    /// The terms are strings (`.succ`/`.pred` stepping).
    pub(crate) is_string_seq: bool,
    /// Single-character strings step by codepoint.
    pub(crate) use_codepoint: bool,
    /// Zero-padded digit strings step in this radix.
    pub(crate) digit_string_radix: Option<u32>,
    /// Strings of one repeated character step over this alphabet.
    pub(crate) uniform_string_alphabet: Option<Vec<char>>,
    /// An exact fractional arithmetic step.
    pub(crate) rat_step: Option<(i64, i64)>,
    /// The generator closure of a `SeqMode::Closure` sequence.
    pub(crate) generator: Option<Value>,
    pub(crate) generator_shape: SeqGeneratorShape,
    /// Raku's `.count` of a closure endpoint.
    pub(crate) endpoint_count: SeqClosureCount,
}

/// How a run of the generator ended.
#[derive(Clone, Copy, Debug, Default)]
pub(crate) struct SeqGenOutcome {
    /// The sequence ended (endpoint reached or passed, or the generator
    /// stopped) instead of the run reaching its term budget.
    pub(crate) ended: bool,
    /// A generator closure signalled the end (`last`).
    pub(crate) generation_finished: bool,
    /// A value endpoint was reached.
    pub(crate) endpoint_reached: bool,
    /// A generator of an endpoint-less sequence errored; the rest is deferred.
    pub(crate) deferred_after_generator_error: bool,
}
