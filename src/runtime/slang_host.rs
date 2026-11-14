//! The slang host (ADR-15762 §2.2): the interpreter that loaded a slang module
//! stays alive for the rest of the unit's parse and runs the slang's tokens and
//! actions when the parser reaches a hookable production.
//!
//! The parser thread and the host thread alternate on a strict call stack over
//! two channels, so neither touches the other's state:
//!
//! ```text
//! parser  --Hook{rule, source}------------------>  host   runs the token
//! parser  <--BackCall{rule, pos}----------------   host   a core rule (`<block>`)
//! parser  --BackReply(end, node)---------------->  host   needs the parser
//! parser  <--Done(end, node)--------------------   host   token + action done
//! ```
//!
//! Positions are character offsets into `source`, the unit's text when the
//! parser has it (the production then starts at the hook's `start`, and the
//! cursor's `.orig` and `.from` are the unit's, as rakudo's are), else the text
//! from the start of the production to the end of the unit. The host owns the composed grammar
//! and actions objects; a RakuAST node crosses the boundary as the `Value` it
//! is (both threads are registered GC mutators).
//!
//! The channel types are in [`crate::slang_link`], a leaf the parser may name.

use super::*;
use std::cell::RefCell;
use crate::slang_link::{FromHost, HostAnswer, HostEnd, ToHost};

thread_local! {
    static HOST_LINK: RefCell<Option<HostEnd>> = const { RefCell::new(None) };
    /// The entry of the Raku side of the host, evaluated on the first hook.
    static HOST_ENTRY: RefCell<Option<Value>> = const { RefCell::new(None) };
    /// How many hooks are running inside one another (a slang production in a
    /// block another one's shim parsed).
    static HOOK_DEPTH: std::cell::Cell<u32> = const { std::cell::Cell::new(0) };
}

/// Whether a slang hook is running on this thread: the host sees none of the
/// importing unit's lexicals, so it must not judge a variable undeclared.
// Cost: O(1).
pub(super) fn in_hook() -> bool {
    HOOK_DEPTH.with(std::cell::Cell::get) > 0
}

/// The bound on nested hooks: the source's nesting is untrusted input
/// (docs/security.md, resource bounds), so a deeper one is a catchable error.
const MAX_HOOK_DEPTH: u32 = 64;

/// The Raku side of the host: the base grammar and actions the slang's roles
/// are composed onto, and the entry the loop calls (ADR-15762 §2.3, §2.4).
/// `block`, `signature` and `trait` are shims: the core production is the Rust parser's, reached by a
/// back-call. `end-keyword` is rakudo's own token.
const HOST_PRELUDE: &str = include_str!("slang_host_raku/prelude.raku");

/// The stubs of the resolver, compilation unit and base actions a production reads
/// (`$*R`, `$*CU`, `attach`). The unit evaluates them too, to replay an action there
/// (ADR-17243).
pub(super) const HOST_STUBS: &str = include_str!("slang_host_raku/stubs.raku");

/// The entry of the host: runs one production and answers its node.
const HOST_ENTRY_SOURCE: &str = include_str!("slang_host_raku/entry.raku");

impl Interpreter {
    /// Serve parser requests until the parser drops its end (the unit's parse
    /// is over). The slang's roles were recorded by `define_slang`.
    // Cost: the slang's own code per request.
    pub(super) fn serve_slang_host(&mut self) {
        loop {
            let message = HOST_LINK.with(|l| {
                let link = l.borrow();
                let link = link.as_ref()?;
                crate::gc::block_quiescent(|| link.from_parser.recv()).ok()
            });
            let Some(ToHost::Hook {
                rule,
                source,
                start,
                node,
                block,
                span,
            }) = message
            else {
                return;
            };
            if !self.answer_hook(&rule, &source, start, node, block, span) {
                return;
            }
        }
    }

    /// Run one hook and send the parser its `Done`; `false` when the parser is
    /// gone.
    // Cost: the token and the action.
    fn answer_hook(
        &mut self,
        rule: &str,
        source: &str,
        start: usize,
        node: Option<Value>,
        block: Option<Value>,
        span: Option<crate::slang_link::ActionSpan>,
    ) -> bool {
        let depth = HOOK_DEPTH.with(std::cell::Cell::get);
        let answer = if depth >= MAX_HOOK_DEPTH {
            Err(format!(
                "slang productions nest deeper than {MAX_HOOK_DEPTH} levels"
            ))
        } else {
            HOOK_DEPTH.with(|d| d.set(depth + 1));
            // Composed outside the class log: the host's own grammar and
            // actions are not classes the production installed, so they must
            // not be replayed into the unit (where the grammar would lack its
            // host base's `panic`).
            let composed = self.compose_slang_host();
            super::slang_class_replay::begin_class_log();
            let answer =
                composed.and_then(|()| self.run_slang_hook(rule, source, start, node, block, span));
            super::slang_class_replay::end_class_log();
            HOOK_DEPTH.with(|d| d.set(depth));
            answer
        };
        HOST_LINK.with(|l| {
            l.borrow()
                .as_ref()
                .is_some_and(|link| link.to_parser.send(FromHost::Done(answer)).is_ok())
        })
    }

    /// The host's entry sub, compiled on first use.
    // Cost: O(1) after the first call, which compiles the host program.
    fn slang_host_entry(&mut self) -> Result<Value, String> {
        if let Some(run) = HOST_ENTRY.with(|e| e.borrow().clone()) {
            return Ok(run);
        }
        let entry = self
            .eval_code_string(&format!("{HOST_PRELUDE}{HOST_STUBS}{HOST_ENTRY_SOURCE}"))
            .map_err(|e| e.message.to_string())?;
        HOST_ENTRY.with(|e| *e.borrow_mut() = Some(entry.clone()));
        Ok(entry)
    }

    /// Compose (or reuse) the grammar and actions classes for the slang's
    /// current roles.
    // Cost: O(1) when the roles are unchanged; the composition otherwise.
    fn compose_slang_host(&mut self) -> Result<(), String> {
        let run = self.slang_host_entry()?;
        let args = vec![
            Value::str(String::new()),
            Value::str(String::new()),
            Value::array(self.regex_state.slang_grammar_roles.clone()),
            Value::array(self.regex_state.slang_action_roles.clone()),
            Value::pair("compose-only".to_string(), Value::TRUE),
        ];
        self.call_sub_value(run, args)
            .map(|_| ())
            .map_err(|e| e.message.to_string())
    }

    /// Match `rule` at character offset `start` of `source` with the composed
    /// grammar and read the node its action attached.
    // Cost: the token and the action.
    fn run_slang_hook(
        &mut self,
        rule: &str,
        source: &str,
        start: usize,
        node: Option<Value>,
        block: Option<Value>,
        span: Option<crate::slang_link::ActionSpan>,
    ) -> HostAnswer {
        let run = self.slang_host_entry()?;
        let grammar_roles = Value::array(self.regex_state.slang_grammar_roles.clone());
        let action_roles = Value::array(self.regex_state.slang_action_roles.clone());
        let answer = self
            .call_sub_value(
                run,
                {
                    let mut args = vec![
                        Value::str(rule.to_string()),
                        Value::str(source.to_string()),
                        grammar_roles,
                        action_roles,
                        Value::pair(
                            "pos".to_string(),
                            Value::int(
                                crate::value::match_helpers::cp_to_grapheme_index(source, start)
                                    as i64,
                            ),
                        ),
                    ];
                    if let Some(node) = node {
                        args.push(Value::pair("node".to_string(), node));
                    }
                    if let Some(block) = block {
                        args.push(Value::pair("block".to_string(), block));
                    }
                    if let Some(span) = span {
                        args.push(Value::pair(
                            "to".to_string(),
                            Value::int(
                                crate::value::match_helpers::cp_to_grapheme_index(source, span.end)
                                    as i64,
                            ),
                        ));
                        if let Some(file) = span.file {
                            args.push(Value::pair("file".to_string(), Value::str(file)));
                        }
                    }
                    args
                },
            )
            .map_err(|e| e.message.to_string())?;
        let Some(items) = answer.as_list_items() else {
            return Ok(None);
        };
        let end = items
            .first()
            .and_then(Value::as_int)
            .filter(|n| *n >= 0)
            .ok_or_else(|| "slang token answered no end position".to_string())?;
        let node = items.get(1).cloned().unwrap_or(Value::NIL);
        // A node a class wrapped (an instance over a `RakuAst` base) stands for
        // that base.
        let node = match node.view() {
            ValueView::Instance { attributes, .. } => attributes.rakuast_base().unwrap_or(node),
            _ => node,
        };
        if !matches!(node.view(), ValueView::RakuAst(_)) {
            return Err(format!(
                "slang production '{rule}' produced no RakuAST node (its action must attach one)"
            ));
        }
        Ok(Some((end as usize, node)))
    }

    /// `$grammar.HOW.find_method($grammar, 'statementlist')` on a slang grammar
    /// handle: the core production's method, which a slang's override captures
    /// and delegates to (ADR-16888 §2.3). A `Method` (EEL tests `~~ Method`)
    /// invoked with the cursor as its invocant, which asks the parser to parse
    /// the core rule at it; extra arguments (`arglist(|args)`) are ignored.
    // Cost: O(1) (compiles a one-line method).
    pub(super) fn slang_core_method(&mut self, invocant: &Value, name: &str) -> Option<Value> {
        if !crate::parser::slang_host_wrapped_rule(name) {
            return None;
        }
        let ValueView::Instance { class_name, .. } = invocant.view() else {
            return None;
        };
        if class_name.as_str() != super::slang_activation::GRAMMAR_HANDLE_CLASS {
            return None;
        }
        self.eval_code_string(&format!(
            "method (*@) {{ __mutsu_slang_shim('{name}', self) }}"
        ))
        .ok()
    }

    /// `__mutsu_slang_shim($rule, $cursor)`: the grammar's shim for a core
    /// production. Asks the parser (a back-call) to parse `$rule` at the
    /// cursor's position and answers a cursor spanning it whose `.ast` is the
    /// node the parser built.
    // Cost: the parser's own parse of the production.
    pub(super) fn builtin_slang_shim(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let rule = args.first().map(Value::to_string_value).unwrap_or_default();
        let cursor = args.get(1).cloned().unwrap_or(Value::NIL);
        let ValueView::Instance {
            class_name,
            attributes,
            ..
        } = cursor.view()
        else {
            return Err(RuntimeError::new("slang shim called without a cursor"));
        };
        let pos = attributes
            .as_map()
            .get("pos")
            .and_then(Value::as_int)
            .unwrap_or(0)
            .max(0) as usize;
        let orig = attributes
            .as_map()
            .get("orig")
            .map(Value::to_string_value)
            .unwrap_or_default();
        let dynamics = crate::slang_link::BackCallDynamics {
            in_regex_assertion: self
                .get_dynamic_var("*IN_REGEX_ASSERTION")
                .is_ok_and(|v| v.truthy()),
        };
        let answer = self.slang_back_call(&rule, pos, dynamics)?;
        let class = class_name.resolve();
        match answer.map_err(RuntimeError::new)? {
            Some((end, node)) => {
                let cursor = Self::make_cursor_value(&class, &orig, pos as i64, end as i64, false);
                if let ValueView::Instance { attributes, .. } = cursor.view() {
                    // The match's own nodes: an action may edit them in place.
                    attributes.insert("ast", crate::rakuast::fresh_copy::fresh_copy(&node));
                }
                Ok(cursor)
            }
            None => Ok(Self::make_cursor_value(
                &class, &orig, pos as i64, -1, true,
            )),
        }
    }

    /// Ask the parser to parse the core production `rule` at `pos` and wait for
    /// its answer, serving a hook the parser starts meanwhile: a slang
    /// production inside the block it parses runs inside this call.
    // Cost: the parser's own parse of the production.
    fn slang_back_call(
        &mut self,
        rule: &str,
        pos: usize,
        dynamics: crate::slang_link::BackCallDynamics,
    ) -> Result<HostAnswer, RuntimeError> {
        let lost = || RuntimeError::new("the parser stopped answering the slang shim");
        HOST_LINK.with(|l| {
            let link = l.borrow();
            let link = link.as_ref().ok_or_else(lost)?;
            link.to_parser
                .send(FromHost::BackCall {
                    rule: rule.to_string(),
                    pos,
                    dynamics,
                })
                .map_err(|_| lost())
        })?;
        loop {
            let message = HOST_LINK.with(|l| {
                let link = l.borrow();
                let link = link.as_ref()?;
                crate::gc::block_quiescent(|| link.from_parser.recv()).ok()
            });
            match message.ok_or_else(lost)? {
                ToHost::BackReply(answer) => return Ok(answer),
                ToHost::Hook {
                    rule,
                    source,
                    start,
                    node,
                    block,
                    span,
                } => {
                    if !self.answer_hook(&rule, &source, start, node, block, span) {
                        return Err(lost());
                    }
                }
            }
        }
    }
}

/// Keep the host's end of the connection on this (the host) thread.
pub(super) fn install_host_end(end: HostEnd) {
    HOST_LINK.with(|l| *l.borrow_mut() = Some(end));
}
