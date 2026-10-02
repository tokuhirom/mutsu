//! The Name property in regex property tests (`<:name(/.../)>`), and the
//! regex engine's single entry point for property tests.
//!
//! Rakudo smartmatches a character's name against a `<:name(...)>` argument,
//! so a regex argument has to run on a regex engine. It runs on mutsu's own
//! (#10439); the free predicates in [`super::unicode`] decide every other
//! form, and the scan prefilter treats a Name regex as opaque.

use super::Interpreter;
use super::unicode::{check_unicode_property, check_unicode_property_with_args};

/// The body of a `<:name(/.../)>` / `<:na(/.../)>` regex argument, if `prop`
/// is the Name property and `args` is a regex literal.
///
/// Rakudo smartmatches the character's name against the argument, so a regex
/// is matched by the regex engine itself
/// ([`Interpreter::unicode_property_holds`]); the free predicates only see
/// the other argument forms.
fn name_property_regex<'a>(prop: &str, args: &'a str) -> Option<&'a str> {
    if !prop.eq_ignore_ascii_case("name") && !prop.eq_ignore_ascii_case("na") {
        return None;
    }
    let inner = args.trim().strip_prefix('/')?;
    Some(inner.strip_suffix('/').unwrap_or(inner))
}

/// Split the `<+:Prop(...)>` embedded-argument form (`name(/X/)`) into the
/// property and its argument text.
fn split_embedded_args(name: &str) -> (&str, Option<&str>) {
    match name.find('(') {
        Some(open) if open > 0 && name.ends_with(')') => {
            (&name[..open], Some(&name[open + 1..name.len() - 1]))
        }
        _ => (name, None),
    }
}

/// Is this property test a `<:name(/.../)>` regex — given its separate
/// argument, or embedded in `name`? Only the regex engine can decide those
/// (see [`name_property_regex`]), so the prefilter treats them as opaque.
pub(super) fn is_name_regex_test(name: &str, args: Option<&str>) -> bool {
    let (prop, args) = match args {
        Some(args) => (name, Some(args)),
        None => split_embedded_args(name),
    };
    args.is_some_and(|args| name_property_regex(prop, args).is_some())
}

impl Interpreter {
    /// Does `c` have the Unicode property `name` — given its argument
    /// separately (`<:Line_Break("ID")>`) or embedded as `name(args)` (the
    /// `<+:Prop(...)>` class-combining form)?
    ///
    /// This is the regex engine's entry point, so it is where a
    /// `<:name(/.../)>` argument is matched: Rakudo smartmatches the
    /// character's name against the regex, and mutsu runs it on its own Raku
    /// regex engine rather than reading it as some other regex dialect.
    // Cost: O(log R) for a class property, R = ranges in the class; a Name
    // regex costs one regex match over the character's name.
    pub(super) fn unicode_property_holds(
        &mut self,
        name: &str,
        args: Option<&str>,
        c: char,
    ) -> bool {
        let (prop, args) = match args {
            Some(args) => (name, Some(args)),
            None => split_embedded_args(name),
        };
        if let Some(body) = args.and_then(|args| name_property_regex(prop, args)) {
            return crate::builtins::unicode_name::char_name(c)
                .is_some_and(|char_name| self.regex_find_first(body, &char_name).is_some());
        }
        match args {
            Some(args) => check_unicode_property_with_args(prop, args, c),
            None => check_unicode_property(prop, c),
        }
    }
}
