//! The connection between the parser and a slang host (ADR-15762 §2.2): two
//! channels the threads alternate on. A leaf module -- the parser names it, the
//! runtime's `slang_host` serves the other end.

use crate::value::{Value, ValueView};
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
        /// For a `statement` action override: where the statement sits in
        /// `source` (the `Match` spans `start..end`) and the unit's file.
        span: Option<ActionSpan>,
    },
    /// The answer to a [`FromHost::BackCall`].
    BackReply(HostAnswer),
}

/// Where the statement an action override is offered sits: its end (a
/// character offset of the hook's source; `start` is the hook's own) and the
/// file of its unit (`$*ORIGIN-SOURCE.original-file`).
pub(crate) struct ActionSpan {
    pub(crate) end: usize,
    pub(crate) file: Option<String>,
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
    link: HostLink,
}

/// Where a slang host runs.
enum HostLink {
    /// The activation interpreter, on its own thread (ADR-15762 §2.2).
    Thread {
        to_host: Sender<ToHost>,
        from_host: Receiver<FromHost>,
    },
    /// The program interpreter, the parse's compile-time loader, through
    /// nested calls on this thread (ADR-17558 §2.8, slice 4b), composing the
    /// slang's own grammar and actions roles.
    InProcess {
        grammar_roles: Vec<Value>,
        action_roles: Vec<Value>,
    },
}

/// One production a slang host runs (a [`ToHost::Hook`] without a channel).
pub(crate) struct HookRequest {
    pub(crate) rule: String,
    pub(crate) source: String,
    pub(crate) start: usize,
    pub(crate) node: Option<Value>,
    pub(crate) block: Option<Value>,
    pub(crate) span: Option<ActionSpan>,
    /// The slang's grammar and actions roles, for a host that serves more than
    /// one slang.
    pub(crate) roles: (Vec<Value>, Vec<Value>),
}

/// The parser's answer to a host's back-call for a core production.
pub(crate) type BackCall<'a> = dyn FnMut(&str, usize, BackCallDynamics) -> HostAnswer + 'a;

/// The host on the program interpreter, which loaded the slang module at
/// parse time: its hooks run through the parse's compile-time loader.
// Cost: O(1).
pub(crate) fn in_process(grammar_roles: Vec<Value>, action_roles: Vec<Value>) -> SlangHost {
    SlangHost {
        link: HostLink::InProcess {
            grammar_roles,
            action_roles,
        },
    }
}

/// Both ends of a fresh host connection.
pub(crate) fn connect() -> (SlangHost, HostEnd) {
    let (to_host, from_parser) = std::sync::mpsc::channel();
    let (to_parser, from_host) = std::sync::mpsc::channel();
    (
        SlangHost {
            link: HostLink::Thread { to_host, from_host },
        },
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
        let shift = |answer: HostAnswer| {
            answer.map(|found| {
                found.map(|(end, node)| {
                    (grapheme_to_cp_index(text, end).saturating_sub(start), node)
                })
            })
        };
        let (to_host, from_host) = match &self.link {
            HostLink::Thread { to_host, from_host } => (to_host, from_host),
            HostLink::InProcess {
                grammar_roles,
                action_roles,
            } => {
                let request = HookRequest {
                    rule: rule.to_string(),
                    source: text.to_string(),
                    start,
                    node: None,
                    block,
                    span: None,
                    roles: (grammar_roles.clone(), action_roles.clone()),
                };
                let answer = crate::compile_time_loader::run_slang_hook(
                    request,
                    &mut |rule, pos, dynamics| {
                        back_call(rule, pos.saturating_sub(start), dynamics)
                            .map(|found| found.map(|(end, node)| (end + start, node)))
                    },
                );
                return shift(answer);
            }
        };
        to_host
            .send(ToHost::Hook {
                rule: rule.to_string(),
                source: text.to_string(),
                start,
                node: None,
                block,
                span: None,
            })
            .map_err(|_| "the slang host is gone".to_string())?;
        loop {
            let message = crate::gc::block_quiescent(|| from_host.recv())
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
                    to_host
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
        self.run_action_at(rule, node, source, 0, None)
    }

    /// [`Self::run_action`] for a production that occupies `start..` of
    /// `source` (character offsets), as `span` says: a `statement` override's
    /// `Match` is the statement within its unit's text.
    // Cost: the slang's own action, plus O(n) to send `source`.
    pub(crate) fn run_action_at(
        &self,
        rule: &str,
        node: Value,
        source: &str,
        start: usize,
        span: Option<ActionSpan>,
    ) -> Result<Value, String> {
        let (to_host, from_host) = match &self.link {
            HostLink::Thread { to_host, from_host } => (to_host, from_host),
            HostLink::InProcess {
                grammar_roles,
                action_roles,
            } => {
                let request = HookRequest {
                    rule: rule.to_string(),
                    source: source.to_string(),
                    start,
                    node: Some(node),
                    block: None,
                    span,
                    roles: (grammar_roles.clone(), action_roles.clone()),
                };
                let answer = crate::compile_time_loader::run_slang_hook(request, &mut |_, _, _| {
                    Err(format!(
                        "the action override {rule} asked the parser for a production"
                    ))
                });
                return match answer {
                    Ok(Some((_, node))) => Ok(node),
                    Ok(None) => Err(format!("the action override {rule} answered no node")),
                    Err(message) => Err(message),
                };
            }
        };
        to_host
            .send(ToHost::Hook {
                rule: rule.to_string(),
                source: source.to_string(),
                start,
                node: Some(node),
                block: None,
                span,
            })
            .map_err(|_| "the slang host is gone".to_string())?;
        let message = crate::gc::block_quiescent(|| from_host.recv())
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

// The facts a slang activation hands the parser (ADR-0026 §2.1, ADR-0091).
// Plain data, kept in this leaf so the parser and the compile-time loader
// (`crate::compile_time_loader`) can carry them without naming the runtime.

/// What one slang activation run learned: the grammar-rule overrides the
/// module's roles declare, and the package declarators they add (ADR-0091).
#[derive(Default)]
pub(crate) struct SlangActivation {
    pub(crate) rules: Vec<SlangRuleOverride>,
    pub(crate) declarators: Vec<SlangDeclarator>,
    /// Built-in declarators whose HOW an `EXPORT` hook replaced through
    /// `LEXICAL::<EXPORTHOW>` (`EXPORTHOW::SUPERSEDE`).
    pub(crate) supersede_keywords: Vec<String>,
    /// The parser's end of the slang host, when a rule is a production hook
    /// (ADR-15762): the interpreter that loaded the slang stays alive to run
    /// its tokens and actions.
    pub(crate) host: Option<SlangHost>,
    /// The routines the module exports to the activating `use` (under its
    /// import tags), with their operator precedence: an activation in the
    /// activation interpreter stands in for the parse-time load of a slang
    /// module, so the importer's parse learns them here (#18091). Empty when
    /// the program interpreter loaded the module, whose loaded unit supplies
    /// them as for any other `use`.
    pub(crate) exports: Vec<crate::compile_time_loader::LoadedExport>,
}

/// One grammar-rule override a slang role declares: the overridden rule/token
/// name, plus the token's raw regex source when it has one.
///
/// Slang::Tuxic-style slangs override *productions* and the body is Rakudo
/// internals mutsu never executes (ADR-0026 §4), so `body` goes unread. An
/// `L10N::XX` vocabulary role overrides *spellings*, and the body is the
/// localized spelling itself — data, which `parser::stmt::simple::l10n`
/// interprets.
#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct SlangRuleOverride {
    pub(crate) name: String,
    pub(crate) body: Option<String>,
    /// `localized => canonical` pairs read out of an L10N role's
    /// `<category>2ast` method (`method core2ast { my constant %mapping = ... }`),
    /// which is where the generated roles keep the identifier-position half of
    /// a vocabulary. Empty for every other kind of override.
    pub(crate) aliases: Vec<(String, String)>,
}

/// One package declarator a slang grammar role registered.
#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct SlangDeclarator {
    /// The keyword itself — the `:sym<...>` of the proto-regex candidate.
    pub(crate) keyword: String,
    /// The `$*PKGDECL` the candidate declares, i.e. which package kind the
    /// keyword builds. `role` makes the keyword a peer of `role`; anything
    /// else (including a kind named after the declarator itself) is built as
    /// a `class`, which is what Rakudo's `package_def` does for every
    /// class-like `$*PKGDECL`.
    pub(crate) kind: String,
    /// Name of the metaclass the declaration is built with, from
    /// `$*LANG.set_how($*PKGDECL, ...)`. Empty when the slang set none, in
    /// which case the keyword is a plain alias for its package kind.
    pub(crate) how_type: String,
}

impl SlangDeclarator {
    pub(crate) fn is_role(&self) -> bool {
        self.kind == "role"
    }
}

/// A literal `use` argument (`use L10N::BG 'no-slangification'`) carried to the
/// activation thread. Plain data rather than a `Value`: the value is built on
/// the activation thread, whose interpreter owns it.
#[derive(Clone, Debug, PartialEq)]
pub(crate) enum SlangUseArg {
    Str(String),
    Int(i64),
    Num(f64),
    Bool(bool),
    /// `&name` of a sub declared earlier in the importing unit, with the text of
    /// its declaration: rebuilt on the host, so the sub must not close over the
    /// unit's other lexicals (#17055).
    Routine {
        name: String,
        source: String,
    },
}

impl SlangUseArg {
    /// The argument for a literal `value`, or `None` when it is not a plain
    /// string, number or boolean.
    pub(crate) fn from_literal(value: &Value) -> Option<Self> {
        Some(match value.view() {
            ValueView::Str(s) => SlangUseArg::Str(s.to_string()),
            ValueView::Int(n) => SlangUseArg::Int(n),
            ValueView::Num(f) => SlangUseArg::Num(f),
            ValueView::Bool(b) => SlangUseArg::Bool(b),
            _ => return None,
        })
    }
}
