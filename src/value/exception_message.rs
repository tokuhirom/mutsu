use crate::value::AttrMap;
use crate::value::{Value, ValueView};

/// Construct the `.message` string for a structured exception from its class
/// name and attribute map.  Returns `None` when the class name is not
/// recognised (caller should fall back to the generic behaviour).
pub(crate) fn format_exception_message(class_name: &str, attrs: &AttrMap) -> Option<String> {
    match class_name {
        "X::Str::Numeric" => {
            let reason = attr_str(attrs, "reason");
            let source = attr_str(attrs, "source");
            let pos = attrs.get("pos").map(|v| v.to_f64() as usize).unwrap_or(0);
            // Same indicator the coercions build (`pos` counts characters).
            let indicator = crate::value::str_numeric::build_source_indicator(&source, pos);
            Some(format!(
                "Cannot convert string to number: {} {}",
                reason, indicator
            ))
        }
        // The `Promise` resolution-protocol errors. Their text lives here
        // rather than in a `message` attribute stored at each throw site: a
        // `message` attribute shadows this table for `.message`/`.Str`/
        // `.gist`, so a hand-built `X::Promise::Vowed.new` would otherwise
        // render differently from a thrown one (the defect PR #6980 fixed
        // across the whole `X::Proc::Async::*` family).
        "X::Promise::Vowed" => {
            Some("Access denied to keep/break this Promise; already vowed".to_string())
        }
        "X::Promise::Resolved" => Some(format!(
            "Cannot keep/break a Promise more than once (status: {})",
            promise_status(attrs)
        )),
        "X::Promise::CauseOnlyValidOnBroken" => Some(format!(
            "Can only call cause on a broken promise (status: {})",
            promise_status(attrs)
        )),
        // `temp 5` / `let &f`: `localizer` is the prefix's name.
        "X::Localizer::NoContainer" => Some(format!(
            "Can only use '{}' on a container",
            attr_str(attrs, "localizer")
        )),
        "X::Method::NotFound" => {
            let method = attr_str(attrs, "method");
            let typename = attr_str(attrs, "typename");
            Some(format!(
                "No such method '{}' for invocant of type '{}'",
                method, typename
            ))
        }
        "X::Undeclared" => {
            let what = attr_str_or(attrs, "what", "Variable");
            let symbol = attr_str(attrs, "symbol");
            let items = defined_attr(attrs, "suggestions")
                .map(suggestion_list)
                .unwrap_or_default();
            Some(undeclared_message(&what, &symbol, &items))
        }
        // Rakudo 2026.11's heredoc-body name checks; both texts go through
        // `naive-word-wrapper`.
        "X::Syntax::Heredoc::AmbiguousName" => Some(crate::word_wrap::naive_word_wrap(
            &format!(
                "Name '{}' in a heredoc body is ambiguous: it means one declaration where the \
                 heredoc starts and another where its body is written",
                attr_str(attrs, "symbol")
            ),
            72,
        )),
        "X::Syntax::Heredoc::HiddenName" => Some(crate::word_wrap::naive_word_wrap(
            &format!(
                "Name '{}' in a heredoc body is declared only inside a block that closes on the \
                 heredoc's line, before the body. Move the closing brace of the block after the \
                 body to use it",
                attr_str(attrs, "symbol")
            ),
            72,
        )),
        // The worry `* && 1` draws (rakudo 2026.11): `what` is `Whatever` or
        // `HyperWhatever`, `operator` the short-circuit operator.
        "X::Whatever::ShortCircuit" => {
            let what = attr_str(attrs, "what");
            let op = attr_str(attrs, "operator");
            Some(format!(
                "{what} tested first by {op} does not curry the {op} expression,\n\
                 and is always true and defined.\n\
                 Try a block using $_ instead if results are not as expected."
            ))
        }
        "X::Cannot::Lazy" => {
            let action = attr_str(attrs, "action");
            Some(format!("Cannot {} a lazy list", action))
        }
        // rakudo's `X::Cannot::Empty` formats its message from `:action`/`:what`
        // at read time, so user code that raises the documented
        // `X::Cannot::Empty.new(:action<pop>, :what(self.^name))` gets a real
        // message. mutsu's internal throw sites all pass a pre-built `message`
        // attribute (which shadows this table), so only the user-constructed
        // form reaches here -- it used to answer the empty string.
        // Neither attribute has a default in rakudo (an omitted `:what`
        // stringifies to the empty string, with an uninitialized-value
        // warning), so neither gets one here.
        "X::Cannot::Empty" => {
            let action = attr_str(attrs, "action");
            let what = attr_str(attrs, "what");
            Some(format!("Cannot {} from an empty {}", action, what))
        }
        // Rakudo's text, word-wrapped at 72 columns as its
        // `naive-word-wrapper` does. The thrower names the method that
        // needed a Str; `Stringy` is what `~` and the comparators call.
        "X::Buf::AsStr" => {
            let what = attrs
                .get("object")
                .map(crate::value::types::what_type_name)
                .unwrap_or_else(|| "Any".to_string());
            let method = attr_str(attrs, "method");
            let head = match method.as_str() {
                "Str" => format!("Stringification of a {what} is not done with 'Str'."),
                "Stringy" => format!(
                    "Stringification of a {what} is not done with 'Stringy', which the '~' operator uses."
                ),
                _ => format!("A {what} is not a Str, so using '{method}' will not work."),
            };
            Some(crate::word_wrap::naive_word_wrap(
                &format!("{head} The 'decode' method should be used to convert a {what} to a Str."),
                72,
            ))
        }
        // Rakudo's `X::TypeCheck.message`: a got value that already has the
        // expected type shows as its short gist alone, anything else as
        // `Type (short gist)`; a type object shows as its name.
        "X::TypeCheck" => {
            let operation = attr_str(attrs, "operation");
            Some(format!(
                "Type check failed in {operation}; {}",
                type_check_detail(attrs)
            ))
        }
        // Rakudo's subclasses fix the operation (`X::TypeCheck::Binding`'s is
        // `binding`) or word the message themselves (`X::TypeCheck::Return`),
        // so a hand-built one -- NativeCall's return-constraint check throws
        // `X::TypeCheck::Return.new(:got, :expected)` -- has a message too.
        "X::TypeCheck::Binding" => Some(format!(
            "Type check failed in binding; {}",
            type_check_detail(attrs)
        )),
        "X::TypeCheck::Return" => Some(format!(
            "Type check failed for return value; {}",
            type_check_detail(attrs)
        )),
        "X::ControlFlow::Return" => Some("Attempt to return outside of any Routine".to_string()),
        "X::OutOfRange" => {
            let what = attr_str_or(attrs, "what", "Argument");
            let got = attr_str(attrs, "got");
            let range = attr_str(attrs, "range");
            let mut msg = format!("{} out of range. Is: {}, should be in {}", what, got, range);
            if let Some(comment) = defined_attr(attrs, "comment") {
                let c = comment.to_string_value();
                if !c.is_empty() {
                    msg.push_str(&format!("; {}", c));
                }
            }
            Some(msg)
        }
        "X::Immutable" => {
            let typename = attr_str(attrs, "typename");
            let method = attr_str(attrs, "method");
            if method.is_empty() {
                Some(format!("Cannot modify an immutable {}", typename))
            } else {
                Some(format!(
                    "Cannot call '{}' on an immutable '{}'",
                    method, typename
                ))
            }
        }
        "X::Multi::NoMatch" => {
            let name = attr_str(attrs, "name");
            Some(format!(
                "Cannot resolve caller {}; none of these signatures match",
                name
            ))
        }
        "X::Multi::Ambiguous" => {
            let name = attr_str(attrs, "name");
            Some(format!(
                "Ambiguous call to '{}'; these signatures all match",
                name
            ))
        }
        "X::Redeclaration" => {
            let symbol = attr_str(attrs, "symbol");
            let what = attr_str_or(attrs, "what", "symbol");
            if what == "routine" {
                Some(format!(
                    "Redeclaration of {} '{}'. Did you mean to declare a multi-sub?",
                    what, symbol
                ))
            } else {
                Some(format!("Redeclaration of {} '{}'.", what, symbol))
            }
        }
        "X::StubCode" => Some("Stub code executed".to_string()),
        // rakudo: `($.payload ~ " (use the MONKEY-SEE-NO-EVAL pragma ...).")
        // .naive-word-wrapper`; the payload names what was refused.
        "X::SecurityPolicy::Eval" => {
            let payload = attr_str_or(attrs, "payload", "EVAL is a very dangerous function!!!");
            Some(crate::word_wrap::naive_word_wrap(
                &format!(
                    "{payload} (use the MONKEY-SEE-NO-EVAL pragma to override this error but only \
                     if you're VERY sure your data contains no injection attacks)."
                ),
                72,
            ))
        }
        "X::Bind" => {
            let target = attr_str(attrs, "target");
            if target.is_empty() {
                Some("Cannot use bind operator with this left-hand side".to_string())
            } else {
                Some(format!("Cannot bind to {}", target))
            }
        }
        "X::Match::Bool" => {
            let type_name = attr_str_or(attrs, "type", "");
            Some(format!(
                "Cannot use Bool as Matcher with '{}'.  Did you mean to use $_ inside a block?",
                type_name
            ))
        }
        "X::Assignment::RO" => {
            if let Some(val) = defined_attr(attrs, "value") {
                let held = val.clone().into_deref();
                let type_name = if matches!(
                    held.view(),
                    ValueView::Instance { .. } | ValueView::Mixin(..)
                ) {
                    crate::value::types::what_type_name(&held)
                } else {
                    crate::value::type_name::value_type_name(&held).to_string()
                };
                Some(format!(
                    "Cannot modify an immutable {} ({})",
                    type_name,
                    assignment_ro_value_gist(val)
                ))
            } else {
                Some("Cannot modify an immutable value".to_string())
            }
        }
        "X::NYI" => {
            let mut message = if let Some(feature) = attrs.get("feature").filter(|v| v.truthy()) {
                format!("{} not yet implemented. Sorry.", feature.to_string_value())
            } else {
                "Not yet implemented. Sorry.".to_string()
            };
            if let Some(suggestion) = attrs.get("did-you-mean").filter(|v| v.truthy()) {
                message.push_str(&format!(
                    "\nDid you mean: {}?",
                    suggestion.to_string_value()
                ));
            }
            if let Some(workaround) = attrs.get("workaround").filter(|v| v.truthy()) {
                message.push_str(&format!("\nWorkaround: {}", workaround.to_string_value()));
            }
            Some(message)
        }
        "X::Signature::Placeholder" => {
            Some("Placeholder variable cannot override an existing signature".to_string())
        }
        "X::IO::Closed" => {
            let op = attr_str(attrs, "operation");
            Some(format!("Cannot do '{}' on a closed handle", op))
        }
        "X::IO::Resolve" => Some(format!(
            "Failed to completely resolve \"{}\"",
            attr_str(attrs, "path")
        )),
        "X::IO::NotAChild" => Some(format!(
            "Path \"{}\" is not a child of path \"{}\"",
            attr_str(attrs, "child"),
            attr_str(attrs, "path")
        )),
        // The `X::Proc::Async::*` family. Rakudo gives each of these a real
        // `.message` built from its attributes; mutsu used to stuff the bare
        // class name into the `message` attribute at throw time, so `.Str` /
        // `.message` / `.gist` all rendered as the type name. Formatting them
        // here means both the thrown exception and a user-constructed
        // `X::Proc::Async::MustBeStarted.new(:method<say>)` render correctly.
        "X::Proc::Async::AlreadyStarted" => Some("Process has already been started".to_string()),
        "X::Proc::Async::MustBeStarted" => Some(format!(
            "Process must be started first before calling '{}'",
            attr_str(attrs, "method")
        )),
        "X::Proc::Async::OpenForWriting" => Some(format!(
            "Process must be opened for writing with :w to call '{}'",
            attr_str(attrs, "method")
        )),
        "X::Proc::Async::CharsOrBytes" => Some(format!(
            "Can only tap one of chars or bytes supply for {}",
            attr_str(attrs, "handle")
        )),
        "X::Proc::Async::TapBeforeSpawn" => Some(format!(
            "To avoid data races, you must tap {} before running the process",
            attr_str(attrs, "handle")
        )),
        "X::Proc::Async::SupplyOrStd" => Some(
            "Using .Supply on a Proc::Async implies merging stdout and stderr; \
             .stdout and .stderr cannot therefore be used in combination with it"
                .to_string(),
        ),
        "X::Proc::Async::BindOrUse" => Some(format!(
            "Cannot both bind {} to a handle and also {}",
            attr_str(attrs, "handle"),
            attr_str(attrs, "use")
        )),
        "X::Proc::Async::MissingColsRows" => {
            Some(":pty must be set as follows: :pty(:cols(12), :rows(34))".to_string())
        }
        _ => None,
    }
}

/// The promise status an `X::Promise::*` message reports. Rakudo derives it
/// from the exception's `promise` attribute (`$.promise.status`), so read
/// that live status when present and fall back to an explicit `status`
/// attribute (which a hand-built `X::Promise::Resolved.new(:status<Kept>)`
/// or a `throws-like ... status => 'Kept'` check supplies instead).
/// The `expected T but got U (gist)` tail of rakudo's `X::TypeCheck` messages
/// (its `expectedn` / `gotn`): a got value that already has the expected type
/// shows as its short gist alone, anything else as `Type (short gist)`; a type
/// object shows as its name.
// Cost: O(g), g = chars of the got value's `.raku` (cut to 23).
fn type_check_detail(attrs: &AttrMap) -> String {
    let type_name = |v: &Value| match v.view() {
        ValueView::Package(name) => name.resolve().to_string(),
        _ => crate::value::type_name::value_type_name(v).to_string(),
    };
    let expected = attrs.get("expected").map(type_name).unwrap_or_default();
    let got = attrs.get("got").cloned().unwrap_or(Value::NIL);
    let got_name = if got.is_nil() {
        "Nil".to_string()
    } else {
        type_name(&got)
    };
    let mut gist = crate::value::raku_repr::raku_value(&got);
    if gist.chars().count() > 23 {
        gist = format!("{}...", gist.chars().take(20).collect::<String>());
    }
    let got_text = if got_name == expected {
        gist
    } else {
        format!("{got_name} ({gist})")
    };
    format!("expected {expected} but got {got_text}")
}

fn promise_status(attrs: &AttrMap) -> String {
    if let Some(p) = attrs.get("promise")
        && let ValueView::Promise(shared) = p.view()
    {
        return shared.status();
    }
    attr_str_or(attrs, "status", "Planned")
}

/// An attribute's value when it is defined. A built-in exception declares its
/// attributes, so construction seeds an unset one with its type object
/// (`Any`, `Mu`) or `Nil`; the formatters read that as absent, as Rakudo's
/// test `.defined` (#18654).
// Cost: O(1).
pub(crate) fn defined_attr<'a>(attrs: &'a AttrMap, key: &str) -> Option<&'a Value> {
    attrs
        .get(key)
        .filter(|v| !matches!(v.view(), ValueView::Nil | ValueView::Package(_)))
}

/// Extract a string attribute, defaulting to "" if absent or undefined (a
/// type object stringifies to "" in Rakudo).
// Cost: O(s), s = the attribute's string length.
fn attr_str(attrs: &AttrMap, key: &str) -> String {
    defined_attr(attrs, key)
        .map(|v| v.to_string_value())
        .unwrap_or_default()
}

/// Extract a string attribute with a custom default for an absent or
/// undefined one.
// Cost: O(s), s = the attribute's string length.
fn attr_str_or(attrs: &AttrMap, key: &str, default: &str) -> String {
    defined_attr(attrs, key)
        .map(|v| v.to_string_value())
        .unwrap_or_else(|| default.to_string())
}

/// Extract a list of suggestion strings from a Value.
/// Rakudo's `X::Undeclared.message`: the suggestions, or with none the
/// forgotten-`sub` hint, word-wrapped at 72 columns as rakudo's is (#18731).
// Cost: O(m), m = chars of the message.
pub(crate) fn undeclared_message(what: &str, symbol: &str, suggestions: &[String]) -> String {
    let msg = format!("{} '{}' is not declared", what, symbol);
    match suggestions {
        [] => crate::word_wrap::naive_word_wrap(
            &format!(
                "{msg}. Perhaps you forgot a 'sub' if this was intended to be part of a signature?"
            ),
            72,
        ),
        [one] => format!("{msg}. Did you mean '{one}'?"),
        many => {
            let quoted: Vec<String> = many.iter().map(|s| format!("'{}'", s)).collect();
            format!("{msg}. Did you mean any of these: {}?", quoted.join(", "))
        }
    }
}

fn suggestion_list(v: &Value) -> Vec<String> {
    match v.view() {
        ValueView::Array(items, _) => items.iter().map(|item| item.to_string_value()).collect(),
        ValueView::Nil => Vec::new(),
        _ => {
            let s = v.to_string_value();
            if s.is_empty() { Vec::new() } else { vec![s] }
        }
    }
}

/// Get a human-readable type label for a value (used by X::Assignment::RO).
/// The value part of rakudo's `X::Assignment::RO.message`: the value's
/// `.gist`, cut to its first 20 graphemes plus `...` when longer than 23.
// Cost: O(g), g = the gist's length.
fn assignment_ro_value_gist(v: &Value) -> String {
    shorten_assignment_ro_gist(&crate::value::gist::gist_value(v))
}

/// [`assignment_ro_value_gist`]'s cut, for a caller that already holds the
/// gist text.
// Cost: O(g), g = the gist's length.
pub(crate) fn shorten_assignment_ro_gist(gist: &str) -> String {
    use crate::grapheme_break::UnicodeSegmentation;
    if gist.graphemes(true).nth(23).is_none() {
        return gist.to_string();
    }
    let mut short: String = gist.graphemes(true).take(20).collect();
    short.push_str("...");
    short
}
