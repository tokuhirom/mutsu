//! The connection between the parser and a slang host (ADR-15762 §2.2): two
//! channels the threads alternate on. A leaf module -- the parser names it, the
//! runtime's `slang_host` serves the other end.

use crate::value::Value;
use std::sync::mpsc::{Receiver, Sender};

/// A parser-to-host message.
pub(crate) enum ToHost {
    /// Match the slang's token `rule` (`statement-control:sym<twice>`) at
    /// character offset `start` of `source`, then run its action. `source` is
    /// the unit's text when the parser has it (the cursor's `.orig`, which a
    /// slang reads to place what it matched), else the text from the
    /// production on.
    Hook {
        rule: String,
        source: String,
        start: usize,
        /// For an action override (`INFIX-EXPR`): the node the core action built.
        node: Option<Value>,
        /// For a `statementlist` wrapper: the routine whose body the list is,
        /// bound to `$*BLOCK` while the slang's method runs (ADR-16888 §2.6).
        block: Option<Value>,
    },
    /// The answer to a [`FromHost::BackCall`].
    BackReply(HostAnswer),
}

/// A host-to-parser message.
pub(crate) enum FromHost {
    /// Parse the core production `rule` at character offset `pos` of the hook's
    /// source, and answer with [`ToHost::BackReply`].
    BackCall {
        rule: String,
        pos: usize,
        dynamics: BackCallDynamics,
    },
    /// The hook finished.
    Done(HostAnswer),
}

/// The core dynamics in force where the host asked for a core production, which
/// the parser's own grammar consults while it parses that production.
#[derive(Clone, Copy, Debug, Default)]
pub(crate) struct BackCallDynamics {
    /// `$*IN_REGEX_ASSERTION`: a `>` ends the expression (rakudo's
    /// `infixstopper`), as in a regex assertion's `<name: args>` arguments.
    pub(crate) in_regex_assertion: bool,
}

/// `Ok(Some((end, node)))`: the production matched up to character offset `end`
/// and built `node`. `Ok(None)`: no match. `Err`: a failure to report.
pub(crate) type HostAnswer = Result<Option<(usize, Value)>, String>;

/// The host's end of the connection.
pub(crate) struct HostEnd {
    pub(crate) to_parser: Sender<FromHost>,
    pub(crate) from_parser: Receiver<ToHost>,
}

/// The parser's end of a slang host.
pub(crate) struct SlangHost {
    to_host: Sender<ToHost>,
    from_host: Receiver<FromHost>,
}

/// Both ends of a fresh host connection.
pub(crate) fn connect() -> (SlangHost, HostEnd) {
    let (to_host, from_parser) = std::sync::mpsc::channel();
    let (to_parser, from_host) = std::sync::mpsc::channel();
    (
        SlangHost { to_host, from_host },
        HostEnd {
            to_parser,
            from_parser,
        },
    )
}

impl SlangHost {
    /// Run the hook for `rule` on `source`, answering the host's back-calls
    /// through `back_call` until it reports `Done`. The parser blocks while the
    /// host runs; it is quiescent for the GC meanwhile.
    // Cost: the slang's own token and action, plus one `back_call` per core
    // rule the token uses.
    /// `unit` is the unit's text and the character offset of `source` in it,
    /// when the parser has them: the host then matches in the whole text, and
    /// the positions it reports are translated back to `source`'s.
    pub(crate) fn run_hook(
        &self,
        rule: &str,
        source: &str,
        unit: Option<(&str, usize)>,
        block: Option<Value>,
        back_call: &mut dyn FnMut(&str, usize, BackCallDynamics) -> HostAnswer,
    ) -> HostAnswer {
        let (text, start) = unit.unwrap_or((source, 0));
        // The parser sends code-point offsets, the engine's own unit. The host
        // converts the initial offset for its public `subparse(:pos)` call;
        // shim cursor positions remain code points. A finished production's
        // visible `.to` counts graphemes and is converted back here (#17486).
        use crate::value::match_helpers::grapheme_to_cp_index;
        self.to_host
            .send(ToHost::Hook {
                rule: rule.to_string(),
                source: text.to_string(),
                start,
                node: None,
                block,
            })
            .map_err(|_| "the slang host is gone".to_string())?;
        let shift = |answer: HostAnswer| {
            answer.map(|found| {
                found.map(|(end, node)| {
                    (grapheme_to_cp_index(text, end).saturating_sub(start), node)
                })
            })
        };
        loop {
            let message = crate::gc::block_quiescent(|| self.from_host.recv())
                .map_err(|_| "the slang host stopped answering".to_string())?;
            match message {
                FromHost::Done(answer) => return shift(answer),
                FromHost::BackCall {
                    rule,
                    pos,
                    dynamics,
                } => {
                    // The parser answers in `source`'s offsets; the host reads
                    // them in the unit text's.
                    // A shim's cursor position is the engine's code-point
                    // offset, which the parser counts in too.
                    let reply = back_call(&rule, pos.saturating_sub(start), dynamics)
                        .map(|found| found.map(|(end, node)| (end + start, node)));
                    self.to_host
                        .send(ToHost::BackReply(reply))
                        .map_err(|_| "the slang host is gone".to_string())?;
                }
            }
        }
    }

    /// Run the slang's override of the core action `rule` (`INFIX-EXPR`) over
    /// the `node` the core action built, and answer the node it attached.
    /// `source` is the text the node was parsed from (the `Match`'s `.Str`),
    /// empty when the caller has none.
    // Cost: the slang's own action.
    pub(crate) fn run_action(
        &self,
        rule: &str,
        node: Value,
        source: &str,
    ) -> Result<Value, String> {
        self.to_host
            .send(ToHost::Hook {
                rule: rule.to_string(),
                source: source.to_string(),
                start: 0,
                node: Some(node),
                block: None,
            })
            .map_err(|_| "the slang host is gone".to_string())?;
        let message = crate::gc::block_quiescent(|| self.from_host.recv())
            .map_err(|_| "the slang host stopped answering".to_string())?;
        match message {
            FromHost::Done(Ok(Some((_, node)))) => Ok(node),
            FromHost::Done(Ok(None)) => Err(format!("the action override {rule} answered no node")),
            FromHost::Done(Err(message)) => Err(message),
            FromHost::BackCall { .. } => Err(format!(
                "the action override {rule} asked the parser for a production"
            )),
        }
    }
}
