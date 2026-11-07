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
//! Positions are character offsets into `source`, the text from the start of
//! the production to the end of the unit. The host owns the composed grammar
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

/// The bound on nested hooks: the source's nesting is untrusted input
/// (docs/security.md, resource bounds), so a deeper one is a catchable error.
const MAX_HOOK_DEPTH: u32 = 64;

/// The Raku side of the host: the base grammar and actions the slang's roles
/// are composed onto, and the entry the loop calls (ADR-15762 §2.3, §2.4).
/// `block` is a shim: the core production is the Rust parser's, reached by a
/// back-call. `end-keyword` is rakudo's own token.
const HOST_PRELUDE: &str = r#"
grammar Mutsu::Slang::Host::Base {
    token end-keyword { » <!before \h* '=>'> }
    method block() { __mutsu_slang_shim('block', self) }
}
class Mutsu::Slang::Host::Actions {
    method attach($/, $ast) { $/.make($ast); $ast }
    # The core actions a slang may override and `callsame` into: the core node
    # is the Match's `.ast` already, so they answer it.
    method INFIX-EXPR(Mu $/) { $/.ast }
    method PREFIX-EXPR(Mu $/) { $/.ast }
    method numish(Mu $/) { $/.ast }
}
our $Mutsu::Slang::Host::G;
our $Mutsu::Slang::Host::A;
sub mutsu-slang-host-run(Str $rule, Str $source, @groles, @aroles, :$node) {
    unless $Mutsu::Slang::Host::G.defined {
        my $g = Metamodel::GrammarHOW.new_type(:name<Mutsu::Slang::Host::Grammar>);
        $g.^add_parent(Mutsu::Slang::Host::Base);
        $g.^add_role($_) for @groles;
        $g.^compose;
        my $a = Metamodel::ClassHOW.new_type(:name<Mutsu::Slang::Host::ActionsClass>);
        $a.^add_parent(Mutsu::Slang::Host::Actions);
        $a.^add_role($_) for @aroles;
        $a.^compose;
        $Mutsu::Slang::Host::G = $g;
        $Mutsu::Slang::Host::A = $a.new;
    }
    if $node.defined {
        my $core = Match.new(:orig(''), :from(0), :to(0));
        $core.make($node);
        $Mutsu::Slang::Host::A."$rule"($core);
        return ($core.to, $core.ast);
    }
    my $m = $Mutsu::Slang::Host::G.subparse($source, :rule($rule), :actions($Mutsu::Slang::Host::A));
    $m ?? ($m.to, $m.ast) !! Nil
}
&mutsu-slang-host-run
"#;

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
            let Some(ToHost::Hook { rule, source, node }) = message else {
                return;
            };
            if !self.answer_hook(&rule, &source, node) {
                return;
            }
        }
    }

    /// Run one hook and send the parser its `Done`; `false` when the parser is
    /// gone.
    // Cost: the token and the action.
    fn answer_hook(&mut self, rule: &str, source: &str, node: Option<Value>) -> bool {
        let depth = HOOK_DEPTH.with(std::cell::Cell::get);
        let answer = if depth >= MAX_HOOK_DEPTH {
            Err(format!(
                "slang productions nest deeper than {MAX_HOOK_DEPTH} levels"
            ))
        } else {
            HOOK_DEPTH.with(|d| d.set(depth + 1));
            let answer = self.run_slang_hook(rule, source, node);
            HOOK_DEPTH.with(|d| d.set(depth));
            answer
        };
        HOST_LINK.with(|l| {
            l.borrow()
                .as_ref()
                .is_some_and(|link| link.to_parser.send(FromHost::Done(answer)).is_ok())
        })
    }

    /// Match `rule` at the start of `source` with the composed grammar and
    /// read the node its action attached.
    // Cost: the token and the action.
    fn run_slang_hook(&mut self, rule: &str, source: &str, node: Option<Value>) -> HostAnswer {
        let mut run = HOST_ENTRY.with(|e| e.borrow().clone());
        if run.is_none() {
            let entry = self
                .eval_code_string(HOST_PRELUDE)
                .map_err(|e| e.message.to_string())?;
            HOST_ENTRY.with(|e| *e.borrow_mut() = Some(entry.clone()));
            run = Some(entry);
        }
        let run = run.unwrap_or(Value::NIL);
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
                    ];
                    if let Some(node) = node {
                        args.push(Value::pair("node".to_string(), node));
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
        if !matches!(node.view(), ValueView::RakuAst(_)) {
            return Err(format!(
                "slang production '{rule}' produced no RakuAST node (its action must attach one)"
            ));
        }
        Ok(Some((end as usize, node)))
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
        let answer = self.slang_back_call(&rule, pos)?;
        let class = class_name.resolve();
        match answer.map_err(RuntimeError::new)? {
            Some((end, node)) => {
                let cursor = Self::make_cursor_value(&class, &orig, pos as i64, end as i64, false);
                if let ValueView::Instance { attributes, .. } = cursor.view() {
                    attributes.insert("ast", node);
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
    fn slang_back_call(&mut self, rule: &str, pos: usize) -> Result<HostAnswer, RuntimeError> {
        let lost = || RuntimeError::new("the parser stopped answering the slang shim");
        HOST_LINK.with(|l| {
            let link = l.borrow();
            let link = link.as_ref().ok_or_else(lost)?;
            link.to_parser
                .send(FromHost::BackCall {
                    rule: rule.to_string(),
                    pos,
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
                ToHost::Hook { rule, source, node } => {
                    if !self.answer_hook(&rule, &source, node) {
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
