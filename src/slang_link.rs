//! The connection between the parser and a slang host (ADR-15762 §2.2): two
//! channels the threads alternate on. A leaf module -- the parser names it, the
//! runtime's `slang_host` serves the other end.

use crate::value::Value;
use std::sync::mpsc::{Receiver, Sender};

/// A parser-to-host message.
pub(crate) enum ToHost {
    /// Match the slang's token `rule` (`statement-control:sym<twice>`) at the
    /// start of `source`, then run its action.
    Hook {
        rule: String,
        source: String,
        /// For an action override (`INFIX-EXPR`): the node the core action built.
        node: Option<Value>,
    },
    /// The answer to a [`FromHost::BackCall`].
    BackReply(HostAnswer),
}

/// A host-to-parser message.
pub(crate) enum FromHost {
    /// Parse the core production `rule` at character offset `pos` of the hook's
    /// source, and answer with [`ToHost::BackReply`].
    BackCall { rule: String, pos: usize },
    /// The hook finished.
    Done(HostAnswer),
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
    pub(crate) fn run_hook(
        &self,
        rule: &str,
        source: &str,
        back_call: &mut dyn FnMut(&str, usize) -> HostAnswer,
    ) -> HostAnswer {
        self.to_host
            .send(ToHost::Hook {
                rule: rule.to_string(),
                source: source.to_string(),
                node: None,
            })
            .map_err(|_| "the slang host is gone".to_string())?;
        loop {
            let message = crate::gc::block_quiescent(|| self.from_host.recv())
                .map_err(|_| "the slang host stopped answering".to_string())?;
            match message {
                FromHost::Done(answer) => return answer,
                FromHost::BackCall { rule, pos } => {
                    let reply = back_call(&rule, pos);
                    self.to_host
                        .send(ToHost::BackReply(reply))
                        .map_err(|_| "the slang host is gone".to_string())?;
                }
            }
        }
    }

    /// Run the slang's override of the core action `rule` (`INFIX-EXPR`) over
    /// the `node` the core action built, and answer the node it attached.
    // Cost: the slang's own action.
    pub(crate) fn run_action(&self, rule: &str, node: Value) -> Result<Value, String> {
        self.to_host
            .send(ToHost::Hook {
                rule: rule.to_string(),
                source: String::new(),
                node: Some(node),
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
