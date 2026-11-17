//! Structured error constructors for method dispatch failures
//! (X::Method::Private::*, X::Method::NotFound, X::Immutable, X::Multi::*).
use super::*;
use crate::symbol::Symbol;

/// Create a structured X::Method::Private::Permission error: a fully-qualified
/// private call `$o!Owner::meth` where `Owner` does not trust the calling
/// package. Carries `method`, `source-package` (the owner) and `calling-package`.
pub(super) fn make_private_permission_error(
    method_name: &str,
    class_name: &str,
    calling_package: &str,
) -> RuntimeError {
    // Either package name may be a lexical class's mangled storage name
    // (ADR-0047 P1: `Foo\u{0}<decl-id>`) — show the user-facing bare name in
    // the message and the `source-package`/`calling-package` attributes.
    let class_name = crate::value::user_facing_type_name(class_name);
    let calling_package = crate::value::user_facing_type_name(calling_package);
    let msg = format!(
        "Cannot call private method '{}' on package {} because it does not trust {}",
        method_name, class_name, calling_package
    );
    let mut attrs = std::collections::HashMap::new();
    attrs.insert("method".to_string(), Value::str(method_name.to_string()));
    attrs.insert(
        "source-package".to_string(),
        Value::str(class_name.to_string()),
    );
    attrs.insert(
        "calling-package".to_string(),
        Value::str(calling_package.to_string()),
    );
    attrs.insert("message".to_string(), Value::str(msg.clone()));
    let ex = Value::make_instance(Symbol::intern("X::Method::Private::Permission"), attrs);
    let mut err = RuntimeError::new(msg);
    err.exception = Some(Box::new(ex));
    err
}

/// Create a structured X::Method::Private::Unqualified error: an unqualified
/// private call `$o!meth` on something other than `self`. Raku requires such
/// calls to name the package that defines the private method.
pub(super) fn make_private_unqualified_error(method_name: &str) -> RuntimeError {
    let msg = format!(
        "Calling private method '{}' must be fully qualified with the package containing that private method.",
        method_name
    );
    let mut attrs = std::collections::HashMap::new();
    attrs.insert("method".to_string(), Value::str(method_name.to_string()));
    attrs.insert("message".to_string(), Value::str(msg.clone()));
    let ex = Value::make_instance(Symbol::intern("X::Method::Private::Unqualified"), attrs);
    let mut err = RuntimeError::new(msg);
    err.exception = Some(Box::new(ex));
    err
}

/// `X::Method::NotFound` for `method` called on `target`, naming a type
/// object by its own name (`Int.name` is "for invocant of type 'Int'"). A
/// `Str` invocant reads `for string 'abc'`, as in Rakudo.
pub(super) fn method_not_found_for_value(method: &str, target: &Value) -> RuntimeError {
    match target.view() {
        crate::value::ValueView::Package(name) => {
            let name = name.resolve();
            let text = (name == "Str").then_some("");
            make_method_not_found_error_for(method, &name, false, text)
        }
        crate::value::ValueView::Str(s) => {
            make_method_not_found_error_for(method, "Str", false, Some(&s))
        }
        _ => make_method_not_found_error(method, crate::runtime::value_type_name(target), false),
    }
}

/// Create a structured X::Method::NotFound error.
pub(crate) fn make_method_not_found_error(
    method_name: &str,
    type_name: &str,
    private: bool,
) -> RuntimeError {
    make_method_not_found_error_for(method_name, type_name, private, None)
}

/// [`make_method_not_found_error`] for an invocant whose stringification is
/// known to be a `Str` (`string_invocant`): Rakudo words that case
/// `for string 'abc'`.
pub(super) fn make_method_not_found_error_for(
    method_name: &str,
    type_name: &str,
    private: bool,
    string_invocant: Option<&str>,
) -> RuntimeError {
    make_method_not_found_error_with(method_name, type_name, private, string_invocant, &[])
}

/// [`make_method_not_found_error_for`] with the method names a user class
/// declares (`extra`), which the "Did you mean" tip offers beside the
/// built-in ones.
pub(super) fn make_method_not_found_error_with(
    method_name: &str,
    type_name: &str,
    private: bool,
    string_invocant: Option<&str>,
    extra: &[String],
) -> RuntimeError {
    // Display the user-facing type name (`<anon|N>` for anonymous
    // class/grammar/role); registry lookups below keep the raw name.
    let display_type = crate::value::user_facing_type_name(type_name);
    let msg = if private {
        format!(
            "No such private method '{}' for invocant of type '{}'",
            method_name, display_type
        )
    } else {
        super::did_you_mean::method_not_found_message_with(
            method_name,
            type_name,
            &display_type,
            string_invocant,
            extra,
        )
    };

    let mut attrs = std::collections::HashMap::new();
    attrs.insert("method".to_string(), Value::str(method_name.to_string()));
    attrs.insert("typename".to_string(), Value::str(display_type.to_string()));
    attrs.insert("private".to_string(), Value::truth(private));
    attrs.insert("message".to_string(), Value::str(msg.clone()));
    let ex = Value::make_instance(Symbol::intern("X::Method::NotFound"), attrs);
    let mut err = RuntimeError::new(msg.to_string());
    err.exception = Some(Box::new(ex));
    err
}

/// Create a structured X::Immutable error.
pub(crate) fn make_x_immutable_error(method_name: &str, typename: &str) -> RuntimeError {
    let msg = format!(
        "Cannot call '{}' on an immutable '{}'",
        method_name, typename
    );
    let mut attrs = std::collections::HashMap::new();
    attrs.insert("method".to_string(), Value::str(method_name.to_string()));
    attrs.insert("typename".to_string(), Value::str(typename.to_string()));
    attrs.insert("message".to_string(), Value::str(msg.clone()));
    let ex = Value::make_instance(Symbol::intern("X::Immutable"), attrs);
    let mut err = RuntimeError::new(msg);
    err.exception = Some(Box::new(ex));
    err
}

/// `X::Multi::NoMatch` in rakudo's "there are no candidates at all" spelling:
///
/// ```text
/// Cannot resolve caller splice(List:D, Int:D, Int:D); Routine does not have
/// any candidates.  Is only the proto defined?
/// ```
///
/// (Two spaces before "Is", as rakudo prints it.) This is what a method the
/// invocant's type simply does not have produces, as distinct from
/// [`builtin_method_no_match`]'s "none of these signatures matches", which is
/// for a routine that HAS candidates and none of them bound.
///
/// The signature lists the invocant first and then each argument, each rendered
/// as its type name plus a `:D`/`:U` smiley.
pub(crate) fn make_no_candidates_error(
    method_name: &str,
    invocant: &Value,
    args: &[Value],
) -> RuntimeError {
    fn typed(v: &Value) -> String {
        let smiley = if crate::runtime::types::value_is_defined(v) {
            ":D"
        } else {
            ":U"
        };
        let name = match v.view() {
            // Only the user-facing spelling names a type object or an instance.
            crate::value::ValueView::Package(_)
            | crate::value::ValueView::Instance { .. }
            | crate::value::ValueView::Mixin(..) => {
                crate::value::types::what_type_name(v)
            }
            _ => crate::runtime::utils::value_type_name(v).to_string(),
        };
        format!("{name}{smiley}")
    }
    let mut sig = String::from(typed(invocant).as_str());
    for arg in args {
        sig.push_str(", ");
        sig.push_str(&typed(arg));
    }
    let msg = format!(
        "Cannot resolve caller {}({}); Routine does not have any candidates.  Is only the proto defined?",
        method_name, sig
    );
    let mut attrs = std::collections::HashMap::new();
    attrs.insert("message".to_string(), Value::str(msg.clone()));
    let ex = Value::make_instance(Symbol::intern("X::Multi::NoMatch"), attrs);
    let mut err = RuntimeError::new(msg);
    err.exception = Some(Box::new(ex));
    err
}

/// An `X::Multi::NoMatch` carrying `msg` verbatim, for a dispatch failure whose
/// full text the caller has already composed.
pub(crate) fn make_no_match_error_with_message(msg: String) -> RuntimeError {
    let mut attrs = std::collections::HashMap::new();
    attrs.insert("message".to_string(), Value::str(msg.clone()));
    let ex = Value::make_instance(Symbol::intern("X::Multi::NoMatch"), attrs);
    let mut err = RuntimeError::new(msg);
    err.exception = Some(Box::new(ex));
    err
}

/// An `X::Multi::NoMatch` used only as an internal signal between dispatch
/// layers, which catch it and never show it (an `is rw` accessor assignment
/// handing over to the Proxy path; a user `new` falling back to the default
/// constructor, where formatting the candidates per call cost #9494). Any
/// error a user can see is built with [`builtin_method_no_match`] or
/// `Interpreter::user_method_no_match` instead.
// Cost: O(1).
pub(crate) fn multi_no_match_signal(method_name: &str) -> RuntimeError {
    make_no_match_error_with_message(format!(
        "Cannot resolve caller {method_name}; none of these signatures matches"
    ))
}

/// The `X::Multi::NoMatch` of a built-in method whose candidates bind the
/// invocant `is rw`, called on a value that is not a container
/// (`"a".subst-mutate(...)`): rakudo's dispatcher says the candidates "match
/// the type but require mutable arguments".
// Cost: as [`builtin_method_no_match`].
pub(crate) fn builtin_method_needs_mutable(
    target: &Value,
    method_name: &str,
    args: &[Value],
) -> RuntimeError {
    let call = super::Interpreter::multi_call_profile(method_name, args);
    let profile = call
        .strip_prefix(method_name)
        .and_then(|rest| rest.strip_prefix('('))
        .and_then(|rest| rest.strip_suffix(')'))
        .unwrap_or("");
    let type_name = builtin_receiver_type_name(target);
    let type_name = declared_class_name(&type_name);
    let lines = super::builtin_method_sigs::candidate_lines(type_name, method_name);
    make_no_match_error_with_message(format!(
        "Cannot resolve caller {method_name}({type_name}:D: {profile}); the following candidates\nmatch the type but require mutable arguments:{lines}"
    ))
}

/// [`builtin_method_no_match`] for a receiver known only by its type name
/// and definedness (a native handle's attribute map, a scheduler).
// Cost: as [`builtin_method_no_match`].
pub(crate) fn builtin_type_no_match(
    type_name: &str,
    concrete: bool,
    method_name: &str,
    args: &[Value],
) -> RuntimeError {
    let call = super::Interpreter::multi_call_profile(method_name, args);
    let profile = call
        .strip_prefix(method_name)
        .and_then(|rest| rest.strip_prefix('('))
        .and_then(|rest| rest.strip_suffix(')'))
        .unwrap_or("");
    make_multi_no_match_error_detailed(method_name, type_name, concrete, profile, &[])
}

/// The `X::Multi::NoMatch` a built-in multi method raises for a call of
/// `method_name` with `args` on `target`: rakudo's `Cannot resolve caller`
/// header with the call profile, and its candidates from the generated table.
// Cost: O(n + a + c), n = arguments, a = the ancestry length of the
// invocant's type, c = candidate signatures of the method.
pub(crate) fn builtin_method_no_match(
    target: &Value,
    method_name: &str,
    args: &[Value],
) -> RuntimeError {
    let call = super::Interpreter::multi_call_profile(method_name, args);
    let profile = call
        .strip_prefix(method_name)
        .and_then(|rest| rest.strip_prefix('('))
        .and_then(|rest| rest.strip_suffix(')'))
        .unwrap_or("");
    make_builtin_method_no_match_error(target, method_name, profile)
}

/// Build an `X::Multi::Ambiguous` error naming the dispatch target and the
/// candidate signatures that all matched equally well.
pub(crate) fn make_multi_ambiguous_error(
    method_name: &str,
    invocant_type: &str,
    candidate_sigs: &[String],
) -> RuntimeError {
    let sig_lines = if candidate_sigs.is_empty() {
        String::new()
    } else {
        let mut s = String::new();
        for sig in candidate_sigs {
            s.push_str("\n  ");
            s.push_str(sig);
        }
        s
    };
    let msg = format!(
        "Ambiguous call to '{}({}: )'; these signatures all match:{}",
        method_name,
        declared_class_name(invocant_type),
        sig_lines
    );
    let mut attrs = std::collections::HashMap::new();
    attrs.insert("message".to_string(), Value::str(msg.clone()));
    let ex = Value::make_instance(Symbol::intern("X::Multi::Ambiguous"), attrs);
    let mut err = RuntimeError::new(msg);
    err.exception = Some(Box::new(ex));
    err
}

/// Build an `X::Multi::NoMatch` error with a Raku-style message that names the
/// dispatch target (invocant type + method name) and lists the candidate
/// signatures, e.g.:
///
/// ```text
/// Cannot resolve caller has_tie(WorkingTie:D: Array:D); none of these
/// signatures matches: ...
/// ```
/// A type minted under a name another type already held is registered as
/// `Name\0<id>` (ADR-0047); a message names the declared one.
// Cost: O(n), n = chars of the name.
fn declared_class_name(name: &str) -> &str {
    name.split('\u{0}').next().unwrap_or(name)
}

/// The type a built-in method call's invocant reports in a NoMatch message
/// and looks its candidates up under: `what_type_name`, except that a list
/// value names `List`/`Seq` as `.^name` does rather than `Array`.
// Cost: O(1).
pub(crate) fn builtin_receiver_type_name(target: &Value) -> String {
    match target.view() {
        crate::value::ValueView::Array(..) | crate::value::ValueView::LazyList(_) => {
            crate::runtime::value_type_name(target).to_string()
        }
        _ => crate::value::what_type_name(target),
    }
}

/// The error a built-in method call on `target` raises when no candidate of
/// `method` takes its number of positionals, as rakudo reports it: a multi
/// whose proto binds the call raises `X::Multi::NoMatch` listing its
/// candidates, while a bounded proto (`proto method chars(*%)`) fails its own
/// bind ("Too many positionals passed"). `None` when rakudo's method is an
/// only method or unknown: the caller words that from its own rows. `args`
/// are the call's arguments; named ones arrive as pairs and bind no
/// positional.
// Cost: O(n + a + c), n = arguments, a = the ancestry length of the
// invocant's type, c = candidate signatures of the method.
pub(crate) fn builtin_multi_arity_mismatch(
    target: &Value,
    method_name: &str,
    args: &[Value],
) -> Option<RuntimeError> {
    use crate::runtime::types::arity_message::{too_few_positionals, too_many_positionals};
    let type_name = builtin_receiver_type_name(target);
    let type_name = declared_class_name(&type_name);
    super::builtin_method_sigs::candidate_signatures(type_name, method_name)?;
    // The invocant counts as a positional in rakudo's wording.
    let got = args.iter().filter(|arg| !arg.is_string_pair_value()).count() + 1;
    if let Some((required, max)) =
        super::builtin_method_sigs::proto_positional_bounds(type_name, method_name)
    {
        if got > max {
            return Some(RuntimeError::new(too_many_positionals(required, max, got)));
        }
        if got < required {
            return Some(RuntimeError::new(too_few_positionals(required, Some(max), got)));
        }
    }
    Some(builtin_method_no_match(target, method_name, args))
}

/// Whether rakudo's type of `target` declares `method_name` at all: a call
/// that misses every one of mutsu's rows only reports an arity mismatch then
/// (a `%?RESOURCES` entry has an `IO::Path` shape but no `sibling`).
// Cost: O(a), a = the ancestry length of the invocant's type.
pub(crate) fn builtin_type_declares(target: &Value, method_name: &str) -> bool {
    let type_name = builtin_receiver_type_name(target);
    super::builtin_method_sigs::declares(declared_class_name(&type_name), method_name)
}

/// The `X::Multi::NoMatch` a built-in multi method raises for a call with
/// `args` on `target`, listing rakudo's candidates from the generated table
/// (`builtin_method_sigs.tsv`). `profile` is the argument types as the message
/// spells them (`Int:D, Int:D`).
// Cost: O(c), c = candidate signatures of the method.
pub(crate) fn make_builtin_method_no_match_error(
    target: &Value,
    method_name: &str,
    profile: &str,
) -> RuntimeError {
    let concrete = !matches!(
        target.view(),
        crate::value::ValueView::Package(_) | crate::value::ValueView::ParametricRole { .. }
    );
    make_multi_no_match_error_detailed(
        method_name,
        &builtin_receiver_type_name(target),
        concrete,
        profile,
        &[],
    )
}

pub(crate) fn make_multi_no_match_error_detailed(
    method_name: &str,
    invocant_type: &str,
    invocant_concrete: bool,
    arg_profile: &str,
    candidate_sigs: &[String],
) -> RuntimeError {
    // A built-in method has no user-visible candidates to list: rakudo's own
    // come from the generated table.
    let from_table: Vec<String>;
    let candidate_sigs = if candidate_sigs.is_empty()
        && let Some(sigs) = super::builtin_method_sigs::candidate_signatures(
            declared_class_name(invocant_type),
            method_name,
        ) {
        from_table = sigs.iter().map(|s| (*s).to_string()).collect();
        &from_table[..]
    } else {
        candidate_sigs
    };
    let sig_lines = if candidate_sigs.is_empty() {
        String::new()
    } else {
        let mut s = String::new();
        for sig in candidate_sigs {
            s.push_str("\n    ");
            s.push_str(sig);
        }
        s
    };
    let smiley = if invocant_concrete { ":D" } else { ":U" };
    let msg = format!(
        "Cannot resolve caller {}({}{}: {}); none of these signatures matches:{}",
        method_name,
        declared_class_name(invocant_type),
        smiley,
        arg_profile,
        sig_lines
    );
    let mut attrs = std::collections::HashMap::new();
    attrs.insert("message".to_string(), Value::str(msg.clone()));
    let ex = Value::make_instance(Symbol::intern("X::Multi::NoMatch"), attrs);
    let mut err = RuntimeError::new(msg);
    err.exception = Some(Box::new(ex));
    err
}

impl super::Interpreter {
    /// The `X::Multi::NoMatch` a call of a user class's multi method raises
    /// when no candidate binds `args`: rakudo's header with the call profile
    /// and the class's candidate signatures (the built-in table's when the
    /// class declares none).
    // Cost: O(n + m), n = arguments, m = candidates across the class's MRO.
    pub(crate) fn user_method_no_match(
        &mut self,
        target: &Value,
        class_name: &str,
        method_name: &str,
        args: &[Value],
    ) -> RuntimeError {
        let sigs = self.format_method_candidate_signatures(class_name, method_name, None);
        let profile = self.format_call_arg_profile(args);
        let concrete = !matches!(
            target.view(),
            crate::value::ValueView::Package(_) | crate::value::ValueView::ParametricRole { .. }
        );
        if sigs.is_empty() {
            // A proto with no candidates at all (`proto method new(|) {*}`).
            let smiley = if concrete { ":D" } else { ":U" };
            let sep = if profile.is_empty() { "" } else { ", " };
            return make_no_match_error_with_message(format!(
                "Cannot resolve caller {method_name}({}{smiley}{sep}{profile}); Routine does not have any candidates.  Is only the proto defined?",
                declared_class_name(class_name),
            ));
        }
        make_multi_no_match_error_detailed(method_name, class_name, concrete, &profile, &sigs)
    }
}
