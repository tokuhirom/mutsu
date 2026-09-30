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

/// One code-atom invocation of the compiled run.
#[derive(Clone)]
struct CodeEvent {
    code: String,
    pos: usize,
    /// What the code saw: the captures visible to it (`caps_desc`) and the names
    /// of the `:my` lexicals in scope.
    view: String,
    result: Option<(usize, RegexCaptures)>,
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
        let replay = l.borrow_mut().replays.pop().expect("a replay in progress");
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
    // Cost: one call of `run`; under `MUTSU_RX_DIFF` also O(c), c = the visible
    // captures (the fingerprint).
    pub(in crate::runtime::regex) fn rx_code_call(
        &mut self,
        code: &str,
        pos: usize,
        caps: &RegexCaptures,
        run: impl FnOnce(&mut Interpreter) -> Option<(usize, RegexCaptures)>,
    ) -> Option<(usize, RegexCaptures)> {
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
                return Some(None);
            };
            if event.code == code && event.pos == pos && event.view == view {
                let result = event.result.clone();
                replay.next += 1;
                return Some(result);
            }
            replay.mismatch.get_or_insert_with(|| {
                format!(
                    "invocation {}: the compiled run ran `{}` at {} seeing [{}], the walk `{code}` \
                     at {pos} seeing [{view}]",
                    replay.next, event.code, event.pos, event.view
                )
            });
            Some(None)
        });
        if let Some(result) = replayed {
            return result;
        }
        let result = run(self);
        LOG.with(|l| {
            let mut l = l.borrow_mut();
            if l.recording > 0 {
                l.events.push(CodeEvent {
                    code: code.to_string(),
                    pos,
                    view,
                    result: result.clone(),
                });
            }
        });
        result
    }
}

fn node_span(node: &CapNode) -> String {
    let kids = node
        .children
        .as_ref()
        .map_or(0, |c| c.named.len() + c.positional.len());
    format!(
        "{}..{} kids={kids} action={:?}",
        node.from, node.to, node.action_name
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
