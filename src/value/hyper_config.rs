//! The `HyperConfiguration` a `.hyper` / `.race` sequence carries (#18690).
//!
//! Rakudo's `HyperSeq` and `RaceSeq` hold only their configuration, so their
//! `.raku` is `Mu.raku` over it (`HyperSeq.new(configuration =>
//! HyperConfiguration.new(batch => 64, degree => 3))`) and never reads an
//! element. The `:batch` / `:degree` a sequence was hyperized with are kept
//! as `Option`s, with `None` meaning the core default.

/// The core default `:batch`.
pub(crate) const DEFAULT_BATCH: i64 = 64;

/// The core default `:degree`: the available CPU cores.
// Cost: O(1), one system query.
pub(crate) fn default_degree() -> i64 {
    std::thread::available_parallelism().map(|n| n.get() as i64).unwrap_or(4)
}

/// The `(batch, degree)` a configuration resolves to, defaults filled in.
// Cost: O(1).
pub(crate) fn resolve(config: Option<(Option<i64>, Option<i64>)>) -> (i64, i64) {
    let (batch, degree) = config.unwrap_or((None, None));
    (batch.unwrap_or(DEFAULT_BATCH), degree.unwrap_or_else(default_degree))
}

/// `.raku` of a `HyperConfiguration` with `config`.
// Cost: O(1).
pub(crate) fn configuration_raku(config: Option<(Option<i64>, Option<i64>)>) -> String {
    let (batch, degree) = resolve(config);
    format!("HyperConfiguration.new(batch => {batch}, degree => {degree})")
}

/// `.raku` of a hyper (`hyper`) or race sequence with `config`: its type and
/// configuration, not its elements.
// Cost: O(1).
pub(crate) fn seq_raku(hyper: bool, config: Option<(Option<i64>, Option<i64>)>) -> String {
    let name = if hyper { "HyperSeq" } else { "RaceSeq" };
    format!("{name}.new(configuration => {})", configuration_raku(config))
}
