//! Production hooks (ADR-15762 §2.1): a slang's new `statement-control:sym<...>`
//! token is tried where the Rust parser starts a statement, ahead of the
//! built-in statement parsers, by running the token and its action on the slang
//! host. The node the action attached is lowered here and stands for the
//! statement.
//!
//! The registry is unit state like [`mod@super::slang_modes`]: reset at parse
//! start, snapshotted around a nested module scan, and filled only on the real
//! parse (the activation interpreter has no host to ask).

use super::*;
use crate::slang_link::SlangHost;
use crate::value::Value;
use std::cell::RefCell;
use std::rc::Rc;

/// One hookable production a slang added.
#[derive(Clone, Debug)]
pub(crate) struct HookRule {
    /// The token's name as the slang wrote it: `statement-control:sym<twice>`.
    pub(crate) rule: String,
    /// The literal the token must start with, read from its source, so that the
    /// host is asked only where the production can match.
    prefix: Option<String>,
}

#[derive(Clone, Default)]
pub(in crate::parser) struct SlangHooks {
    rules: Vec<HookRule>,
    host: Option<Rc<SlangHost>>,
    /// Core actions the slang's actions role overrides (`INFIX-EXPR`).
    actions: Vec<String>,
    /// The module whose `use` activated the slang: the overrides apply to what
    /// follows that statement.
    activator: Option<String>,
    /// Extra lexemes a slang's `pointy-block-starter` token accepts besides
    /// `->` / `<->` (Slang::Lambda's `λ`), with whether each is the `<->`
    /// (read-write) form.
    pointy_starters: Vec<(String, bool)>,
    /// Core productions a slang's grammar role wraps with a method of the same
    /// name (`statementlist`, ADR-16888).
    wrappers: Vec<String>,
}

/// What one hook attempt at a position answered: the length of the text left
/// after the production and its node, `None` when no candidate matched.
type HookMemoEntry = Option<(usize, Value)>;

thread_local! {
    static HOOKS: RefCell<SlangHooks> = RefCell::new(SlangHooks::default());
    /// The per-unit `position -> answer` table of ADR-15762 section 2.8: a hook
    /// answer is a pure function of the category list, the position and the
    /// grammar state, and backtracking asks the same position many times (a
    /// slang production nested in another's block is parsed again by every
    /// enclosing attempt, so without it the cost is exponential in the nesting).
    /// Keyed by the address and length of the input; cleared whenever the
    /// registry changes.
    static HOOK_MEMO: RefCell<std::collections::HashMap<(usize, usize, usize, u64), HookMemoEntry>> =
        RefCell::new(std::collections::HashMap::new());
    /// Set by the back-call that parses the core `<statementlist>` for a
    /// wrapper, and taken by the list parse it starts: that one list is not
    /// wrapped again (the wrapper's own delegation, as `callsame` skips the
    /// method it is in), the lists nested in it are.
    static WRAPPER_BACK_CALL: std::cell::Cell<bool> = const { std::cell::Cell::new(false) };
}

/// Whether `name` is a core production a slang may wrap with a grammar-role
/// method of that name (ADR-16888 §2.1).
// Cost: O(1).
pub(crate) fn slang_wrapper_rule(name: &str) -> bool {
    matches!(name, "statementlist")
}

/// Whether `name` is a core production a slang's grammar role may override
/// with a method that delegates back to the core one (`$grammar.HOW.find_method`
/// answers the shim): `statementlist`, which the parser also hands to the host
/// ([`slang_wrapper_rule`]), and the productions only a slang's own tokens
/// call, whose override runs when such a token does (`arglist`, `block`, the
/// `circumfix` forms). The Rust parser's own uses of those never see the
/// override, as the override only sets dynamics the slang's tokens read.
// Cost: O(1).
pub(crate) fn slang_host_wrapped_rule(name: &str) -> bool {
    slang_wrapper_rule(name)
        || matches!(
            name,
            "arglist"
                | "block"
                | "circumfix:sym<( )>"
                | "circumfix:sym<[ ]>"
                | "circumfix:sym<{ }>"
        )
}

/// Register the wrapper of the core production `name`.
// Cost: O(k), k = registered wrappers.
pub(in crate::parser) fn register_wrapper(name: &str) {
    clear_hook_memo();
    HOOKS.with(|h| {
        let mut hooks = h.borrow_mut();
        if !hooks.wrappers.iter().any(|w| w == name) {
            hooks.wrappers.push(name.to_string());
        }
    });
}

/// The category and symbol of a hookable token name (`statement-control:sym<x>`,
/// also spelled with an underscore), or `None` for any other rule.
// Cost: O(n), n = name length.
pub(crate) fn slang_hook_rule(name: &str) -> Option<&'static str> {
    let (category, rest) = name.split_once(":sym<")?;
    rest.strip_suffix('>')?;
    match category {
        "statement-control" | "statement_control" => Some("statement-control"),
        "routine-declarator" | "routine_declarator" => Some("routine-declarator"),
        "scope-declarator" | "scope_declarator" => Some("scope-declarator"),
        "statement-prefix" | "statement_prefix" => Some("statement-prefix"),
        "term" => Some("term"),
        "value" => Some("value"),
        "number" => Some("number"),
        _ => None,
    }
}

/// The alternatives of a `pointy-block-starter` token body written as a plain
/// alternation of quoted literals (`'->' | '→' | 'λ'`), or `None` when it is
/// anything else, which the caller reports as unsupported. Only `<->` is the
/// read-write form, as in rakudo; every other literal (`↔` included) is an
/// ordinary pointy starter.
// Cost: O(n), n = body length.
pub(crate) fn parse_pointy_starters(body: &str) -> Option<Vec<(String, bool)>> {
    // The regex source carries its adverbs (`:ratchet` for a token).
    let body = body.trim();
    let body = body.strip_prefix(":ratchet").unwrap_or(body);
    let mut starters = Vec::new();
    for alt in body.split('|') {
        let alt = alt.trim();
        let quote = alt.chars().next().filter(|c| matches!(c, '\'' | '"'))?;
        let inner = alt.strip_prefix(quote)?.strip_suffix(quote)?;
        if inner.is_empty() || inner.contains(quote) || inner.contains('\\') {
            return None;
        }
        starters.push((inner.to_string(), inner == "<->"));
    }
    (!starters.is_empty()).then_some(starters)
}

/// Add the lexemes a slang's `pointy-block-starter` accepts.
// Cost: O(k), k = registered starters.
pub(in crate::parser) fn register_pointy_starters(starters: Vec<(String, bool)>) {
    HOOKS.with(|h| {
        let mut hooks = h.borrow_mut();
        for starter in starters {
            if !hooks.pointy_starters.contains(&starter) {
                hooks.pointy_starters.push(starter);
            }
        }
    });
}

/// How `input` starts a pointy block: whether it is the read-write form and
/// the byte length of the starter. Beyond the built-in `<->` and `->`, only
/// the lexemes a slang registered count.
// Cost: O(1) for the built-ins; O(k) for k registered starters otherwise.
pub(crate) fn pointy_starter(input: &str) -> Option<(bool, usize)> {
    if input.starts_with("<->") {
        return Some((true, 3));
    }
    if input.starts_with("->") {
        return Some((false, 2));
    }
    HOOKS.with(|h| {
        h.borrow()
            .pointy_starters
            .iter()
            .find(|(lexeme, _)| input.starts_with(lexeme.as_str()))
            .map(|(lexeme, rw)| (*rw, lexeme.len()))
    })
}

/// Whether `name` is a core action a slang may override (ADR-15762 slice 3).
// Cost: O(1).
pub(crate) fn slang_action_rule(name: &str) -> bool {
    matches!(
        name,
        "INFIX-EXPR" | "PREFIX-EXPR" | "POSTFIX-EXPR" | "numish"
    )
}

/// Register the override of the core action `name`.
// Cost: O(k), k = registered overrides.
pub(in crate::parser) fn register_action(name: &str) {
    HOOKS.with(|h| {
        let mut hooks = h.borrow_mut();
        if !hooks.actions.iter().any(|a| a == name) {
            hooks.actions.push(name.to_string());
        }
    });
}

/// Note the module whose `use` activated the slang.
pub(in crate::parser) fn set_activator(module: &str) {
    HOOKS.with(|h| h.borrow_mut().activator = Some(module.to_string()));
}

/// The module after whose `use` the registered action overrides apply, and
/// whether any is registered with a host to run it.
// Cost: O(1).
pub(crate) fn slang_action_activator() -> Option<String> {
    HOOKS.with(|h| {
        let hooks = h.borrow();
        (hooks.host.is_some() && !hooks.actions.is_empty())
            .then(|| hooks.activator.clone())
            .flatten()
    })
}

/// Whether the slang overrides the core action `rule`.
// Cost: O(k), k = registered overrides.
pub(crate) fn slang_overrides_action(rule: &str) -> bool {
    HOOKS.with(|h| h.borrow().actions.iter().any(|a| a == rule))
}

/// Run the slang's override of the core action `rule` over `node`.
// Cost: the slang's own action.
pub(crate) fn run_slang_action(rule: &str, node: Value, source: &str) -> Result<Value, String> {
    let host = HOOKS
        .with(|h| h.borrow().host.clone())
        .ok_or_else(|| "no slang host is running".to_string())?;
    host.run_action(rule, node, source)
}

/// The literal a token body starts with: `'twice' ...` or `"twice" ...`.
// Cost: O(n), n = body length.
fn leading_literal(body: &str) -> Option<String> {
    let body = body.trim_start();
    let quote = body.chars().next().filter(|c| matches!(c, '\'' | '"'))?;
    let inner = &body[1..];
    let end = inner.find(quote)?;
    let literal = &inner[..end];
    (!literal.is_empty() && !literal.contains('\\')).then(|| literal.to_string())
}

/// Register the hook for the override `name` with token source `body`.
// Cost: O(n), n = body length.
pub(in crate::parser) fn register_hook(name: &str, body: Option<&str>) {
    clear_hook_memo();
    HOOKS.with(|h| {
        let mut hooks = h.borrow_mut();
        hooks.rules.retain(|r| r.rule != name);
        hooks.rules.push(HookRule {
            rule: name.to_string(),
            prefix: body.and_then(leading_literal),
        });
    });
}

/// The host that serves the registered hooks of this unit.
pub(in crate::parser) fn set_host(host: SlangHost) {
    clear_hook_memo();
    HOOKS.with(|h| h.borrow_mut().host = Some(Rc::new(host)));
}

pub(in crate::parser) fn reset_hooks() {
    clear_hook_memo();
    HOOKS.with(|h| *h.borrow_mut() = SlangHooks::default());
}

pub(in crate::parser) fn hooks_snapshot() -> SlangHooks {
    HOOKS.with(|h| h.borrow().clone())
}

pub(in crate::parser) fn restore_hooks(saved: SlangHooks) {
    clear_hook_memo();
    HOOKS.with(|h| *h.borrow_mut() = saved);
}

// Cost: O(1) when the table is empty, O(n) otherwise, n = remembered answers.
fn clear_hook_memo() {
    HOOK_MEMO.with(|m| {
        let mut memo = m.borrow_mut();
        if !memo.is_empty() {
            memo.clear();
        }
    });
}

/// The byte offset of character offset `chars` in `text`.
// Cost: O(n), n = the offset.
fn byte_offset(text: &str, chars: usize) -> usize {
    text.char_indices()
        .nth(chars)
        .map_or(text.len(), |(at, _)| at)
}

/// Parse the core production `rule` at character offset `pos` of `input` for
/// the slang host: the character offset it ends at and its node.
// Cost: the parse of the production.
fn serve_back_call(input: &str, rule: &str, pos: usize) -> crate::slang_link::HostAnswer {
    let start = byte_offset(input, pos);
    crate::rakuast::slang_bridge::parse_production(rule, &input[start..])
        .map(|found| found.map(|(len, node)| (input[..start + len].chars().count(), node)))
}

/// The statement list at `input` as the slang's `statementlist` wrapper parses it
/// (ADR-16888 §2.2): the text after it and its statements. `None` when no
/// wrapper applies here (none registered, this list is the wrapper's own
/// delegation, or the wrapper declined).
// Cost: O(1) with no wrapper; otherwise the slang's own method plus the parse
// of the list.
pub(in crate::parser) fn statementlist_wrapper(
    input: &str,
    suppressed: bool,
) -> Option<PResult<'_, Vec<Stmt>>> {
    if suppressed {
        return None;
    }
    let host = HOOKS.with(|h| {
        let hooks = h.borrow();
        hooks
            .wrappers
            .iter()
            .any(|w| w == "statementlist")
            .then(|| hooks.host.clone())
            .flatten()
    })?;
    let fail = |message: String| {
        Some(Err(PError::fatal_at(
            format!("slang method 'statementlist': {message}"),
            input,
        )))
    };
    let answer = host.run_hook("statementlist", input, &mut |rule, pos| {
        serve_back_call(input, rule, pos)
    });
    match answer {
        Ok(None) => None,
        Ok(Some((end, node))) => match crate::rakuast::slang_bridge::lower_statements(&node) {
            Ok(stmts) => Some(Ok((&input[byte_offset(input, end)..], stmts))),
            Err(e) => fail(e.message.to_string()),
        },
        Err(message) => fail(message),
    }
}

/// Whether the list parse being entered is a wrapper's own delegation; clears
/// the mark.
// Cost: O(1).
pub(in crate::parser) fn take_wrapper_back_call() -> bool {
    WRAPPER_BACK_CALL.with(|f| f.replace(false))
}

/// Run the slang's productions of `category` at the start of `input`: the text
/// after the first one that matches and the node its action attached. `None`: no
/// hook applies here (no slang, or no candidate matched).
// Cost: O(1) with no hook registered; otherwise the slang's own run.
fn run_category<'a>(category: &[&str], input: &'a str) -> Option<Result<(&'a str, Value), PError>> {
    // The category list is one of a few constant slices: its address names it.
    // The head of the text joins the key, so a freed temporary text another one
    // reuses the address of cannot answer for it.
    let head = input
        .as_bytes()
        .iter()
        .take(64)
        .fold(0xcbf29ce484222325u64, |h, b| {
            (h ^ u64::from(*b)).wrapping_mul(0x100000001b3)
        });
    let key = (
        input.as_ptr() as usize,
        input.len(),
        category.as_ptr() as usize,
        head,
    );
    if let Some(known) = HOOK_MEMO.with(|m| m.borrow().get(&key).cloned()) {
        return known.map(|(left, node)| Ok((&input[input.len() - left..], node)));
    }
    let found = run_category_uncached(category, input);
    match &found {
        Some(Ok((rest, node))) => {
            HOOK_MEMO.with(|m| m.borrow_mut().insert(key, Some((rest.len(), node.clone()))));
        }
        None => {
            HOOK_MEMO.with(|m| m.borrow_mut().insert(key, None));
        }
        // An error stands for a fatal parse failure; it is not remembered.
        Some(Err(_)) => {}
    }
    found
}

// Cost: O(1) with no hook registered; otherwise the slang's own run.
fn run_category_uncached<'a>(
    category: &[&str],
    input: &'a str,
) -> Option<Result<(&'a str, Value), PError>> {
    let (rules, host) = HOOKS.with(|h| {
        let hooks = h.borrow();
        if hooks.rules.is_empty() {
            return (Vec::new(), None);
        }
        let rules: Vec<HookRule> = hooks
            .rules
            .iter()
            .filter(|r| {
                slang_hook_rule(&r.rule).is_some_and(|c| category.contains(&c))
                    && r.prefix.as_deref().is_none_or(|p| input.starts_with(p))
            })
            .cloned()
            .collect();
        (rules, hooks.host.clone())
    });
    let host = host?;
    for candidate in rules {
        let answer = host.run_hook(&candidate.rule, input, &mut |rule, pos| {
            serve_back_call(input, rule, pos)
        });
        match answer {
            Ok(None) => continue,
            Ok(Some((end, node))) => return Some(Ok((&input[byte_offset(input, end)..], node))),
            Err(message) => {
                return Some(Err(PError::fatal_at(
                    format!("slang production '{}': {message}", candidate.rule),
                    input,
                )));
            }
        }
    }
    None
}

/// Try the slang's `statement-control`, `routine-declarator` and
/// `scope-declarator` productions at `input`, the start of a statement, ahead of
/// the built-in ones.
// Cost: as [`run_category`].
pub(in crate::parser) fn statement_control_hook(input: &str) -> Option<PResult<'_, Stmt>> {
    let found = run_category(
        &[
            "statement-control",
            "routine-declarator",
            "scope-declarator",
            "statement-prefix",
        ],
        input,
    )?;
    Some(found.and_then(|(rest, node)| {
        crate::rakuast::slang_bridge::lower_statement(&node)
            .map(|stmt| (rest, stmt))
            .map_err(|e| PError::fatal_at(e.message.to_string(), input))
    }))
}

/// Try the slang's `term`, `value`, `number`, `routine-declarator` and
/// `scope-declarator` productions at `input`, the start of a term, ahead of the
/// built-in ones.
// Cost: as [`run_category`].
pub(in crate::parser) fn term_hook(input: &str) -> Option<PResult<'_, Expr>> {
    let found = run_category(
        &[
            "term",
            "value",
            "number",
            "routine-declarator",
            "scope-declarator",
            "statement-prefix",
        ],
        input,
    )?;
    Some(found.and_then(|(rest, node)| {
        crate::rakuast::slang_bridge::lower_expression(&node)
            .map(|expr| (rest, expr))
            .map_err(|e| PError::fatal_at(e.message.to_string(), input))
    }))
}

/// The slang's `numish` override over the numeric literal `expr` the parser just
/// read from `source` (ADR-15762 slice 3): the lowered node its action attached,
/// or `expr` itself when no slang overrides `numish` here.
// Cost: O(1) with no override; otherwise the slang's own action.
pub(in crate::parser) fn numish_hook(expr: Expr, source: &str) -> Result<Expr, PError> {
    if !slang_overrides_action("numish") || slang_action_activator().is_none() {
        return Ok(expr);
    }
    crate::rakuast::slang_bridge::lower_numish(&expr, source)
        .map_err(|e| PError::fatal_at(e.message.to_string(), source))
}

/// Parse the core production `rule` at the start of `text` for a slang token
/// that calls it (`<block>`, ADR-15762 §2.3): the text after it and the
/// statement it stands for. `None` for a production with no shim yet.
// Cost: the parse of the production.
pub(crate) fn parse_slang_shim_production<'a>(
    rule: &str,
    text: &'a str,
) -> Option<Result<(&'a str, Stmt), String>> {
    match rule {
        "block" => Some(super::block_stmt(text).map_err(|e| e.to_string())),
        // `<routine-def=.key-origin('routine-def', 'sub')>`: the rest of a `sub`
        // declaration after its keyword, which a slang spells as it likes
        // (`lambda`, `λ`).
        "routine-def" => Some(
            ws(text)
                .and_then(|(text, _)| {
                    crate::parser::stmt::sub::sub_decl_body(text, false, false, false).or_else(
                        |named| {
                            // No name: the anonymous forms `λ ($n) { ... }`.
                            match crate::parser::primary::ident::sub_after_keyword(text) {
                                Ok(Some((rest, expr))) => Ok((rest, Stmt::Expr(expr))),
                                _ => Err(named),
                            }
                        },
                    )
                })
                .map_err(|e| e.to_string()),
        ),
        // The same for the rest of a `method` declaration (`def`, `method`).
        "method-def" => Some(
            ws(text)
                .and_then(|(text, _)| {
                    crate::parser::stmt::sub_param::method_decl_body(text, false, false)
                })
                .map_err(|e| e.to_string()),
        ),
        // `<statement>`: one statement, without its terminating `;`.
        "statement" => Some(crate::parser::stmt::statement_pub(text).map_err(|e| e.to_string())),
        _ => None,
    }
}

/// Parse the argument list at the start of `text` for a slang token that calls
/// the core `<arglist>`: the text after it and its arguments.
// Cost: the parse of the list.
pub(crate) fn parse_slang_arglist(text: &str) -> Option<(&str, Vec<crate::ast::CallArg>)> {
    crate::parser::stmt::parse_arglist_pub(text).ok()
}

/// Parse the statement list at the start of `text` for a slang token that calls
/// the core `<statementlist>` (ADR-16888): the text after it and its statements.
// Cost: the parse of the list.
pub(crate) fn parse_slang_statementlist(text: &str) -> Option<(&str, Vec<Stmt>)> {
    WRAPPER_BACK_CALL.with(|f| f.set(true));
    let parsed = crate::parser::stmt::stmtlist::stmt_list_with_mode(text, false, false);
    WRAPPER_BACK_CALL.with(|f| f.set(false));
    parsed.ok()
}

/// Parse the parameters of a slang token's `<signature>` at the start of `text`
/// (what stands between the parentheses of `sub f(...)`): the text after them,
/// the parameters and the return type.
// Cost: the parse of the parameter list.
pub(crate) fn parse_slang_signature(
    text: &str,
) -> Option<(&str, Vec<crate::ast::ParamDef>, Option<String>)> {
    let (rest, (params, returns)) =
        crate::parser::stmt::parse_param_list_with_return_pub(text).ok()?;
    Some((rest, params, returns))
}

/// Parse the one trait at the start of `text` for a slang token that calls the
/// core `<trait>`: the text after it and the trait as the source wrote it.
// Cost: the parse of the trait.
pub(crate) fn parse_slang_trait(
    text: &str,
) -> Option<(&str, crate::ast::routine_trait::RoutineTrait)> {
    let (rest, mut traits) = crate::parser::stmt::parse_one_trait_pub(text).ok()?;
    if traits.source_traits.len() != 1 {
        return None;
    }
    Some((rest, traits.source_traits.remove(0)))
}
