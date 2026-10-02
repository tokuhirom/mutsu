//! ADR-0135 D6: the comparison `MUTSU_RX_DIFF=1` applies to every compiled
//! match against the walk's answer for the same start position. Deleted with
//! the walk (D7).
//!
//! Besides the match itself, the two engines must agree on the code they run:
//! which `{ … }` / `<?{ … }>` / `:my` atoms are invoked, in what order, at what
//! position, seeing what captures. Running the walk a second time over a
//! pattern with code would run that code twice — and its side effects with it —
//! so the compiled run *records* each invocation (with the result the code
//! gave) and the walk *replays* them: the walk's n-th invocation must be the
//! recorded n-th one, and is answered from the record instead of being run.

use std::cell::RefCell;

use crate::runtime::Interpreter;
use crate::runtime::regex_types::{CapNode, PosSlot, RegexCaptures};

/// What a code atom's invocation answered: a match (`{ … }`, `<?{ … }>`,
/// `:my`, `<{ … }>`), the list of candidate ends of `$( … )` / `@( … )`, or the
/// bounds of a `** { … }` count.
#[derive(Clone)]
pub(in crate::runtime::regex) enum CodeResult {
    Match(Option<(usize, RegexCaptures)>),
    Ends(Vec<(usize, RegexCaptures)>),
    Count(Option<(usize, Option<usize>)>),
}

/// A value a code atom's invocation can answer with, so one record serves them
/// all.
pub(in crate::runtime::regex) trait CodeValue: Sized {
    fn record(&self) -> CodeResult;
    fn replay(result: &CodeResult) -> Option<Self>;
    /// The answer for an invocation the compiled run never made: a failure.
    fn failed() -> Self;
}

impl CodeValue for Option<(usize, RegexCaptures)> {
    fn record(&self) -> CodeResult {
        CodeResult::Match(self.clone())
    }
    fn replay(result: &CodeResult) -> Option<Self> {
        match result {
            CodeResult::Match(m) => Some(m.clone()),
            _ => None,
        }
    }
    fn failed() -> Self {
        None
    }
}

impl CodeValue for Vec<(usize, RegexCaptures)> {
    fn record(&self) -> CodeResult {
        CodeResult::Ends(self.clone())
    }
    fn replay(result: &CodeResult) -> Option<Self> {
        match result {
            CodeResult::Ends(e) => Some(e.clone()),
            _ => None,
        }
    }
    fn failed() -> Self {
        Vec::new()
    }
}

impl CodeValue for Option<(usize, Option<usize>)> {
    fn record(&self) -> CodeResult {
        CodeResult::Count(*self)
    }
    fn replay(result: &CodeResult) -> Option<Self> {
        match result {
            CodeResult::Count(c) => Some(*c),
            _ => None,
        }
    }
    fn failed() -> Self {
        None
    }
}

/// One code-atom invocation of the compiled run.
#[derive(Clone)]
struct CodeEvent {
    code: String,
    pos: usize,
    /// What the code saw: the captures visible to it (`caps_desc`) and the names
    /// of the `:my` lexicals in scope.
    view: String,
    result: Option<CodeResult>,
    /// How many later events this invocation's own run produced: a code block
    /// that matches a regex of its own (itself holding code) runs those atoms
    /// inside it. A replay answers the invocation from `result` without running
    /// them, so it skips that many events.
    nested: usize,
}

/// The walk's replay of one compiled run's invocations.
struct Replay {
    events: Vec<CodeEvent>,
    next: usize,
    /// The first way the walk diverged from the record.
    mismatch: Option<String>,
}

#[derive(Default)]
struct CodeLog {
    /// Compiled runs recording right now (runs nest: a lookaround's body runs
    /// inside the run that tests it).
    recording: usize,
    events: Vec<CodeEvent>,
    /// Replays in progress, innermost last.
    replays: Vec<Replay>,
}

thread_local! {
    static LOG: RefCell<CodeLog> = RefCell::new(CodeLog::default());
}

/// Is a walk replaying a compiled run's code invocations right now?
pub(super) fn replaying() -> bool {
    LOG.with(|l| !l.borrow().replays.is_empty())
}

/// A compiled run starts recording; the mark is where its events begin.
pub(super) fn begin_record() -> usize {
    LOG.with(|l| {
        let mut l = l.borrow_mut();
        l.recording += 1;
        l.events.len()
    })
}

/// The compiled run that began at `mark` is over: the walk replays what it
/// recorded. (An enclosing run keeps the events for its own replay.)
pub(super) fn begin_replay(mark: usize) {
    LOG.with(|l| {
        let mut l = l.borrow_mut();
        l.recording -= 1;
        let events = l.events[mark..].to_vec();
        if l.recording == 0 {
            l.events.clear();
        }
        l.replays.push(Replay {
            events,
            next: 0,
            mismatch: None,
        });
    });
}

/// The walk is done: how it diverged from the record, if it did.
pub(super) fn end_replay() -> Result<(), String> {
    LOG.with(|l| {
        let Some(replay) = l.borrow_mut().replays.pop() else {
            return Ok(());
        };
        match replay.mismatch {
            Some(why) => Err(why),
            None if replay.next < replay.events.len() => Err(format!(
                "the compiled run invoked {} code atom(s) the walk never reached; the first was \
                 `{}` at {}",
                replay.events.len() - replay.next,
                replay.events[replay.next].code,
                replay.events[replay.next].pos
            )),
            None => Ok(()),
        }
    })
}

impl Interpreter {
    /// Run one code atom through `run` — or, under `MUTSU_RX_DIFF=1`, record it
    /// for the compiled run and replay it for the walk (see the module doc).
    /// `code` and `pos` identify the invocation; `caps` is what the code sees.
    // Cost: O(1) plus one call of `run`; under `MUTSU_RX_DIFF` also O(c),
    // c = the captures visible to the code (the fingerprint).
    pub(in crate::runtime::regex) fn rx_code_call<R: CodeValue>(
        &mut self,
        code: &str,
        pos: usize,
        caps: &RegexCaptures,
        run: impl FnOnce(&mut Interpreter) -> R,
    ) -> R {
        if !super::rx_diff_enabled() {
            return run(self);
        }
        let mut vars: Vec<&String> = caps.regex_vars().keys().collect();
        vars.sort();
        let view = format!("{} vars={vars:?}", caps_desc(&caps.inline_capture_view()));
        let replayed = LOG.with(|l| {
            let mut l = l.borrow_mut();
            let replay = l.replays.last_mut()?;
            let Some(event) = replay.events.get(replay.next) else {
                replay.mismatch.get_or_insert_with(|| {
                    format!("the walk invoked `{code}` at {pos}, which the compiled run never did")
                });
                return Some(R::failed());
            };
            if event.code == code && event.pos == pos && event.view == view {
                let answer = event.result.as_ref().and_then(R::replay);
                if answer.is_some() {
                    replay.next += 1 + event.nested;
                    return answer;
                }
                replay.mismatch.get_or_insert_with(|| {
                    format!(
                        "invocation {}: `{code}` at {pos} answered a different kind of result",
                        replay.next
                    )
                });
                return Some(R::failed());
            }
            replay.mismatch.get_or_insert_with(|| {
                format!(
                    "invocation {}: the compiled run ran `{}` at {} seeing [{}], the walk `{code}` \
                     at {pos} seeing [{view}]",
                    replay.next, event.code, event.pos, event.view
                )
            });
            Some(R::failed())
        });
        if let Some(answer) = replayed {
            return answer;
        }
        // The event is reserved before the run, so the record keeps call order
        // when the run invokes code atoms of its own.
        let slot = LOG.with(|l| {
            let mut l = l.borrow_mut();
            (l.recording > 0).then(|| {
                l.events.push(CodeEvent {
                    code: code.to_string(),
                    pos,
                    view,
                    result: None,
                    nested: 0,
                });
                l.events.len() - 1
            })
        });
        let result = run(self);
        if let Some(slot) = slot {
            LOG.with(|l| {
                let mut l = l.borrow_mut();
                let nested = l.events.len() - slot - 1;
                let event = &mut l.events[slot];
                event.result = Some(result.record());
                event.nested = nested;
            });
        }
        result
    }
}

/// A capture node, children included: a subrule's Match is a whole tree, and
/// the two engines must build the same one.
fn node_span(node: &CapNode) -> String {
    let kids = node.kids();
    let mut named: Vec<String> = kids
        .named
        .iter()
        .map(|(k, v)| {
            format!(
                "{}{}=[{}]",
                k.resolve(),
                if v.quantified { "(q)" } else { "" },
                v.nodes
                    .iter()
                    .map(|n| node_span(n))
                    .collect::<Vec<_>>()
                    .join(", ")
            )
        })
        .collect();
    named.sort();
    format!(
        "{}..{} sym={:?} action={:?} positional=[{}] named={{{}}}",
        node.from,
        node.to,
        node.sym,
        node.action_name,
        kids.positional
            .iter()
            .map(slot_desc)
            .collect::<Vec<_>>()
            .join("; "),
        named.join(" ")
    )
}

fn slot_desc(slot: &PosSlot) -> String {
    format!(
        "{}..{} subcap={:?} quantified={:?} nil={}",
        slot.from,
        slot.to,
        slot.subcap.as_deref().map(node_span),
        slot.quantified
            .as_ref()
            .map(|q| q.iter().map(|(f, t, _)| (*f, *t)).collect::<Vec<_>>()),
        slot.nil
    )
}

fn caps_desc(caps: &RegexCaptures) -> String {
    let mut named: Vec<String> = caps
        .named
        .iter()
        .map(|(k, v)| {
            format!(
                "{}{}=[{}]",
                k.resolve(),
                if v.quantified { "(q)" } else { "" },
                v.nodes
                    .iter()
                    .map(|n| node_span(n))
                    .collect::<Vec<_>>()
                    .join(", ")
            )
        })
        .collect();
    named.sort();
    format!(
        "positional=[{}] named={{{}}} capture_start={:?} capture_end={:?} match_from={}",
        caps.positional
            .iter()
            .map(slot_desc)
            .collect::<Vec<_>>()
            .join("; "),
        named.join(" "),
        caps.capture_start,
        caps.capture_end,
        caps.match_from
    )
}

/// `Ok` when the two engines reported the same match (or both none).
pub(super) fn same_match(
    compiled: &Option<(usize, RegexCaptures)>,
    walked: &Option<(usize, RegexCaptures)>,
) -> Result<(), String> {
    let render = |m: &Option<(usize, RegexCaptures)>| match m {
        None => "no match".to_string(),
        Some((end, caps)) => format!("end {end}, {}", caps_desc(caps)),
    };
    let (c, w) = (render(compiled), render(walked));
    if c == w {
        Ok(())
    } else {
        Err(format!("compiled: {c}\n  walked: {w}"))
    }
}

/// `Ok` when the two engines reported the same ends, in the same order.
pub(super) fn same_ends(
    compiled: &[(usize, RegexCaptures)],
    walked: &[(usize, RegexCaptures)],
) -> Result<(), String> {
    let render = |m: &[(usize, RegexCaptures)]| {
        m.iter()
            .map(|(end, caps)| format!("end {end}, {}", caps_desc(caps)))
            .collect::<Vec<_>>()
            .join("\n    ")
    };
    let (c, w) = (render(compiled), render(walked));
    if c == w {
        Ok(())
    } else {
        Err(format!("compiled:\n    {c}\n  walked:\n    {w}"))
    }
}

/// Abort: under `MUTSU_RX_DIFF=1` the compiled engine and the walk disagree.
/// The one place that does, so the panic surface stays one site.
pub(super) fn disagreement(what: String) -> ! {
    panic!("MUTSU_RX_DIFF: compiled engine and walk disagree {what}")
}
