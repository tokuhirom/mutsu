//! The profiler's surface: the CLI flags, the environment gates, and the one
//! resolved configuration everything else reads (ADR-0106 D8, Slice 5).
//!
//! Two ways in, deliberately:
//!
//! - `--profile[=FILE]` and friends, parsed by `src/main.rs` and handed here
//!   before the program runs. This is the rakudo-compatible spelling, and the
//!   one that produces an artifact on disk.
//! - `MUTSU_PROFILE=1` (plus `MUTSU_PROFILE_RATE`, `MUTSU_PROFILE_OUT`,
//!   `MUTSU_PROFILE_KIND`, `MUTSU_PROFILE_REPORT`, `MUTSU_PROFILE_TICK`),
//!   which is how every other mutsu instrument is armed and the only way in
//!   for a run mutsu did not spawn itself (an embedder, a `prove` harness).
//!
//! Resolution happens **once**, at or before the first VM poll, and is then
//! fixed for the process: the sampler's tick thread and buffer sizes and the
//! JIT's helper ABI are all chosen from it, so a configuration that could
//! change mid-run would be a configuration two halves of the process disagree
//! about.

use super::sampler::Tick;
use std::path::PathBuf;
use std::sync::OnceLock;

/// The default report file, per ADR-0106 D8. Rakudo's own default is a
/// timestamped `profile-<n>.html`; mutsu's primary artifact is JSON (D7) and a
/// fixed name is what makes `--profile` scriptable.
pub(crate) const DEFAULT_OUT: &str = "mutsu-prof.json";

/// stackprof and py-spy both default here; ADR-0106 §8.1 measured 1.10x on
/// `bench-json-fast` at this rate, which the ADR accepts.
pub(crate) const DEFAULT_RATE_HZ: u64 = 1000;

/// Which half of the profile the document carries (`--profile-kind`).
///
/// A kind that is *not* asked for is **absent** from the document rather than
/// present and empty: an empty table reads as "nothing ran there", which is a
/// different claim from "this run did not collect that".
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub(crate) enum Kind {
    Line,
    Routine,
    Both,
}

impl Kind {
    pub(crate) fn name(self) -> &'static str {
        match self {
            Kind::Line => "line",
            Kind::Routine => "routine",
            Kind::Both => "both",
        }
    }

    pub(crate) fn lines(self) -> bool {
        matches!(self, Kind::Line | Kind::Both)
    }

    pub(crate) fn routines(self) -> bool {
        matches!(self, Kind::Routine | Kind::Both)
    }
}

/// Which renderings of the document to emit (`--profile-report`).
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub(crate) enum Report {
    Json,
    Text,
    Both,
}

impl Report {
    pub(crate) fn name(self) -> &'static str {
        match self {
            Report::Json => "json",
            Report::Text => "text",
            Report::Both => "both",
        }
    }

    pub(crate) fn json(self) -> bool {
        matches!(self, Report::Json | Report::Both)
    }

    pub(crate) fn text(self) -> bool {
        matches!(self, Report::Text | Report::Both)
    }
}

/// The resolved configuration.
pub(crate) struct ProfileOptions {
    /// Where the JSON document goes. `None` only when the report is text-only.
    pub(crate) out: Option<PathBuf>,
    pub(crate) kind: Kind,
    pub(crate) report: Report,
    pub(crate) rate_hz: u64,
    pub(crate) tick: Tick,
}

/// What the CLI parsed, before defaults are applied. Every field is what the
/// user actually typed, so "not given" stays distinguishable from "given the
/// default value" — which is what lets the environment fill the gaps.
#[derive(Default)]
pub struct CliOptions {
    /// `--profile` was present at all (with or without `=FILE`).
    pub profile: bool,
    pub out: Option<String>,
    pub kind: Option<String>,
    pub report: Option<String>,
    pub rate: Option<String>,
    /// `--profile-jit=on|off` (ADR-0106 D6's A/B knob).
    pub jit: Option<bool>,
}

impl CliOptions {
    /// Take one command-line argument if it is one of the profiler's.
    ///
    /// `None` means "not mine", and the caller reports it as the unknown option
    /// it is (ADR-0017: `Illegal option`, usage, exit 0). `Some(Err(message))`
    /// means the flag is the profiler's but its *value* is not implemented,
    /// which rakudo reports on stderr with exit 1 — a different thing from an
    /// unparsable option list, so a different outcome.
    ///
    /// Living here rather than in `src/main.rs` keeps every spelling of the
    /// surface in the module that defines what the spellings mean.
    pub fn take_arg(&mut self, arg: &str) -> Option<Result<(), String>> {
        if arg == "--profile" {
            self.profile = true;
            return Some(Ok(()));
        }
        if let Some(file) = arg.strip_prefix("--profile=") {
            self.profile = true;
            self.out = Some(file.to_string());
            return Some(Ok(()));
        }
        if let Some(value) = arg.strip_prefix("--profile-kind=") {
            return Some(parse_kind(value).map(|_| self.kind = Some(value.to_string())));
        }
        if let Some(value) = arg.strip_prefix("--profile-report=") {
            return Some(parse_report(value).map(|_| self.report = Some(value.to_string())));
        }
        if let Some(value) = arg.strip_prefix("--profile-rate=") {
            return Some(parse_rate(value).map(|_| self.rate = Some(value.to_string())));
        }
        if let Some(value) = arg.strip_prefix("--profile-jit=") {
            return Some(match value {
                "on" | "1" => {
                    self.jit = Some(true);
                    Ok(())
                }
                "off" | "0" => {
                    self.jit = Some(false);
                    Ok(())
                }
                _ => Err(format!(
                    "Invalid profiler JIT setting {value:?} specified, expected on or off"
                )),
            });
        }
        None
    }
}

static OPTIONS: OnceLock<Option<ProfileOptions>> = OnceLock::new();

/// The resolved options, or `None` when this run is not profiling.
///
/// Resolves from the environment alone the first time it is asked, which is the
/// path an embedder and a `prove`-spawned process take: only the `mutsu` binary
/// calls [`configure`], and it does so before the first poll.
pub(crate) fn get() -> Option<&'static ProfileOptions> {
    OPTIONS
        .get_or_init(|| {
            resolve_from(&CliOptions::default()).unwrap_or_else(|message| {
                // Unreachable: with no flags to parse there is nothing to
                // reject. Reported rather than panicked if it ever becomes so.
                warn(&message);
                None
            })
        })
        .as_ref()
}

/// Whether the profiler is armed at all. This is the gate
/// `vm_poll::profile_enabled` consults, and the only thing a disarmed run
/// pays.
pub(crate) fn armed() -> bool {
    get().is_some()
}

/// Apply the CLI's view of the options, before the program runs.
///
/// Returns the message to report on a value mutsu does not implement; the
/// caller owns the stream and the exit status (ADR-0017). A `--profile-kind`
/// mutsu does not do is rakudo's `Unknown profiler specified` on stderr with
/// status 1, which is what rakudo 2026.07 does for `--profile-kind=bogus`.
///
/// Calling this after something has already resolved the options is a no-op,
/// which cannot happen from the CLI: option parsing runs before the first
/// poll.
pub fn configure(cli: CliOptions) -> Result<(), String> {
    if let Some(jit) = cli.jit {
        // Applied whether or not this run profiles: it is an explicit
        // instruction about the JIT, and ADR-0106 D6 wants the A/B pair to be
        // the same program run two ways.
        crate::vm::vm_jit::set_cli_override(jit);
    }
    // Flags are validated even when this run turns out not to profile. That is
    // one deliberate difference from rakudo, which runs the program and ignores
    // a `--profile-kind` given without `--profile`: a value mutsu does not
    // implement was still typed by somebody, and telling them beats silently
    // doing something else.
    let resolved = resolve_from(&cli)?;
    let _ = OPTIONS.set(resolved);
    Ok(())
}

/// The options this run profiles under, from the flags plus the environment.
/// `Ok(None)` means it does not profile at all.
///
/// One function for both entry points on purpose: a `--profile-rate` given
/// beside `MUTSU_PROFILE=1` has to reach the sampler, and a second resolution
/// path is how a typed flag gets quietly dropped.
fn resolve_from(cli: &CliOptions) -> Result<Option<ProfileOptions>, String> {
    let out = cli
        .out
        .clone()
        .or_else(|| std::env::var("MUTSU_PROFILE_OUT").ok());
    if !(cli.profile || cli.out.is_some() || env_profile_on()) {
        return Ok(None);
    }
    let kind = match cli.kind.as_deref() {
        None => env_kind(),
        Some(text) => Some(parse_kind(text)?),
    };
    let report = match cli.report.as_deref() {
        None => env_report(),
        Some(text) => Some(parse_report(text)?),
    };
    let rate = match cli.rate.as_deref() {
        None => env_rate(),
        Some(text) => Some(parse_rate(text)?),
    };
    // `--profile` names a file by default; an environment-armed run is an
    // *instrument* rather than a command and must not litter the working
    // directory of whatever spawned it, so it writes one only when asked.
    Ok(Some(resolve(
        cli.profile || out.is_some(),
        out,
        kind,
        report,
        rate,
    )))
}

fn env_profile_on() -> bool {
    match std::env::var("MUTSU_PROFILE").ok().as_deref() {
        Some("1") => true,
        None | Some("0") => false,
        Some(other) => {
            warn(&format!(
                "unrecognized MUTSU_PROFILE={other:?}, treating as 0"
            ));
            false
        }
    }
}

/// Apply the defaults an unstated option gets.
fn resolve(
    wants_file: bool,
    out: Option<String>,
    kind: Option<Kind>,
    report: Option<Report>,
    rate: Option<u64>,
) -> ProfileOptions {
    let report = report.unwrap_or(if wants_file {
        Report::Both
    } else {
        Report::Text
    });
    // A JSON report has to land somewhere; a text-only report deliberately
    // writes no file at all, so `--profile-report=text` leaves the directory
    // untouched.
    let out = if report.json() {
        Some(PathBuf::from(
            out.unwrap_or_else(|| DEFAULT_OUT.to_string()),
        ))
    } else {
        // Contradictory input: a file was named and then the JSON half was
        // switched off. Say so rather than silently writing nothing, which is
        // indistinguishable from a profiler that failed to arm.
        if let Some(named) = out {
            warn(&format!(
                "a text-only report writes no file; ignoring {named:?}"
            ));
        }
        None
    };
    ProfileOptions {
        out,
        kind: kind.unwrap_or(Kind::Both),
        report,
        rate_hz: rate.unwrap_or(DEFAULT_RATE_HZ),
        tick: env_tick(),
    }
}

fn parse_kind(text: &str) -> Result<Kind, String> {
    match text {
        "line" => Ok(Kind::Line),
        "routine" => Ok(Kind::Routine),
        "both" => Ok(Kind::Both),
        // `heap` and `instrumented` are rakudo spellings for things mutsu does
        // not do; ADR-0106 D8 declines to claim them until it does.
        _ => Err("Unknown profiler specified".to_string()),
    }
}

fn parse_report(text: &str) -> Result<Report, String> {
    match text {
        "json" => Ok(Report::Json),
        "text" => Ok(Report::Text),
        "both" => Ok(Report::Both),
        _ => Err("Unknown profiler report specified".to_string()),
    }
}

fn parse_rate(text: &str) -> Result<u64, String> {
    match text.parse::<u64>() {
        Ok(hz) if (1..=1_000_000).contains(&hz) => Ok(hz),
        _ => Err(format!(
            "Invalid profiler rate {text:?} specified, expected 1..1000000"
        )),
    }
}

/// An environment variable is advice, not a command: a bad value warns and
/// falls back, where a bad *flag* is an error. Same split as `MUTSU_JIT` and
/// `MUTSU_GC`, and it matters because an inherited variable is often not typed
/// by the person whose run it breaks.
fn env_kind() -> Option<Kind> {
    env_parsed("MUTSU_PROFILE_KIND", parse_kind)
}

fn env_report() -> Option<Report> {
    env_parsed("MUTSU_PROFILE_REPORT", parse_report)
}

fn env_rate() -> Option<u64> {
    env_parsed("MUTSU_PROFILE_RATE", parse_rate)
}

fn env_parsed<T>(name: &str, parse: fn(&str) -> Result<T, String>) -> Option<T> {
    let text = std::env::var(name).ok()?;
    match parse(&text) {
        Ok(value) => Some(value),
        Err(message) => {
            warn(&format!("{name}={text:?}: {message}, using the default"));
            None
        }
    }
}

fn env_tick() -> Tick {
    match std::env::var("MUTSU_PROFILE_TICK").ok().as_deref() {
        None | Some("timer") => Tick::Timer,
        Some("every-poll") => Tick::EveryPoll,
        Some(other) => {
            warn(&format!(
                "unrecognized MUTSU_PROFILE_TICK={other:?}, using timer"
            ));
            Tick::Timer
        }
    }
}

fn warn(message: &str) {
    eprintln!("[mutsu profiler] warning: {message}");
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn a_kind_mutsu_does_not_implement_is_rakudos_message() {
        assert_eq!(
            parse_kind("heap").unwrap_err(),
            "Unknown profiler specified",
            "rakudo 2026.07 prints exactly this for --profile-kind=bogus"
        );
        assert_eq!(parse_kind("both").unwrap(), Kind::Both);
    }

    #[test]
    fn a_rate_outside_the_sampler_range_is_rejected_rather_than_clamped() {
        assert!(parse_rate("0").is_err());
        assert!(parse_rate("1000001").is_err());
        assert!(parse_rate("abc").is_err());
        assert_eq!(parse_rate("250").unwrap(), 250);
    }

    #[test]
    fn a_text_only_report_writes_no_file() {
        let options = resolve(false, None, None, Some(Report::Text), None);
        assert!(
            options.out.is_none(),
            "an env-armed instrument must not litter the spawner's directory"
        );
        assert_eq!(options.rate_hz, DEFAULT_RATE_HZ);
    }

    /// A flag beside an environment-armed run reaches the sampler. Resolving
    /// both in one place is what makes that true; a separate path for the
    /// environment is how a typed `--profile-rate` got quietly dropped.
    #[test]
    fn a_flag_wins_over_the_environment_that_armed_the_run() {
        // `out` stands in for `MUTSU_PROFILE=1` as the thing that arms the run:
        // a test cannot set an environment variable without racing every other
        // test in this process.
        let cli = CliOptions {
            out: Some("prof.json".to_string()),
            rate: Some("250".to_string()),
            ..CliOptions::default()
        };
        let options = resolve_from(&cli)
            .expect("a valid rate is not an error")
            .expect("naming an output file arms the run");
        assert_eq!(options.rate_hz, 250);
        assert_eq!(
            options.out.as_deref(),
            Some(std::path::Path::new("prof.json"))
        );
    }

    /// Nothing profile-shaped: the run does not profile, which is the branch
    /// every ordinary mutsu run takes and the only one it is allowed to cost.
    #[test]
    fn an_unprofiled_run_resolves_to_nothing() {
        if std::env::var("MUTSU_PROFILE").as_deref() == Ok("1") {
            return; // this process was itself armed; nothing to assert
        }
        assert!(
            resolve_from(&CliOptions::default())
                .expect("nothing to reject")
                .is_none()
        );
    }

    #[test]
    fn a_json_report_always_has_somewhere_to_land() {
        let options = resolve(true, None, None, None, Some(200));
        assert_eq!(
            options.out.as_deref(),
            Some(std::path::Path::new(DEFAULT_OUT))
        );
        assert_eq!(options.report, Report::Both);
        assert_eq!(options.rate_hz, 200);
    }
}
