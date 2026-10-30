//! What one leaf atom matches at a position: an atom that answers at most one
//! end and needs no backtracking engine around it.
//!
//! Both engines call these: the compiled engine's `Leaf` op and `Call` op (for a
//! builtin `<name>`), and the walk's single-candidate matcher. They are the
//! atoms' one definition (ADR-0135 D4), kept out of the walk's modules so the
//! walk can be deleted without them (D7).

use super::super::unicode::check_unicode_property;
use super::super::*;
use super::regex_helpers::{
    LTM_DECLARATIVE_MODE, NamedRegexLookupSpec, is_word_char,
};

/// What a `$name` interpolation ([`RegexAtom::VarInterp`]) matches at a
/// position: a literal's end (`None`: no match), or a pattern to run there.
pub(in crate::runtime) enum VarInterpTarget {
    End(Option<usize>),
    Pattern(std::sync::Arc<RegexPattern>),
}

impl Interpreter {
    /// Match the leaf atom `atom` at `pos`: its end and the capture delta it
    /// files, or `None` for no match. `caps` is the matching level's captures
    /// (a backreference and a `$x` read them); `ignore_case` is the atom's
    /// `:i`. The leaf atoms are a backreference, a `<(` / `)>` marker, a `$x`
    /// lexical, a `"…"` atom, a `<{ … }>` closure and `<~~>`.
    // Cost: O(n) for a backreference, n = the characters it compares; O(1) for
    // a marker; O(r) for a `$x` or a `"…"`, r = the value's length; a closure's
    // and `<~~>`'s cost is the pattern they match.
    pub(super) fn regex_leaf_atom(
        &mut self,
        atom: &RegexAtom,
        chars: &[char],
        pos: usize,
        caps: &RegexCaptures,
        pkg: Symbol,
        ignore_case: bool,
    ) -> Option<(usize, RegexCaptures)> {
        match atom {
            RegexAtom::CaptureStartMarker => Some((
                pos,
                RegexCaptures {
                    capture_start: Some(pos),
                    ..Default::default()
                },
            )),
            RegexAtom::CaptureEndMarker => Some((
                pos,
                RegexCaptures {
                    capture_end: Some(pos),
                    ..Default::default()
                },
            )),
            RegexAtom::Backref(idx) => {
                backref_end(*idx, caps, chars, pos).map(|end| (end, RegexCaptures::default()))
            }
            RegexAtom::NamedBackref(name) => {
                named_backref_end(name, caps, chars, pos).map(|end| (end, RegexCaptures::default()))
            }
            RegexAtom::VarInterp(name) | RegexAtom::VarAssertion(name) => {
                let assertion = matches!(atom, RegexAtom::VarAssertion(_));
                match self.var_interp_target(name, assertion, caps, chars, pos, (ignore_case, false)) {
                    VarInterpTarget::End(end) => end,
                    // The walk takes the interpolated pattern's
                    // highest-priority end, as for a `$( … )` atom.
                    VarInterpTarget::Pattern(parsed) => self
                        .regex_code_interp_pattern_ends(&parsed, chars, pos, pkg)
                        .pop()
                        .map(|(end, _)| end),
                }
                .map(|end| (end, RegexCaptures::default()))
            }
            RegexAtom::QqInterp { key, fallback } => {
                match self.match_qq_interp_result(*key, chars, pos, ignore_case) {
                    Some(end) => end.map(|end| (end, RegexCaptures::default())),
                    None => self
                        .regex_match_end_from_caps_in_pkg(fallback, chars, pos, pkg)
                        .map(|(next, _)| (next, RegexCaptures::default())),
                }
            }
            RegexAtom::ClosureInterpolation { code, body } => {
                self.regex_closure_interp_atom(code, body.as_ref(), chars, pos, caps)
            }
            RegexAtom::RecurseSelf(source) => self
                .regex_match_recurse_self(source, chars, pos, pkg)
                .map(|end| (end, RegexCaptures::default())),
            _ => {
                debug_assert!(false, "not a leaf atom");
                None
            }
        }
    }

    /// What `$name` matches at `pos`. The value is the in-regex `:my $name …`
    /// lexical from the level's `regex_vars` (set by an earlier `VarDecl` or
    /// code block), else the env's — an outer variable, or a rule's runtime
    /// parameter bound in the call's window (`regex_rule_params`). A `Regex`
    /// value is matched as a pattern, as rakudo's interpolation node calls
    /// it; anything else as a literal, and under `:i` through a pattern too
    /// (the case-folding literal match lives in the engine). An undefined
    /// value (Nil or a bare type object) interpolates as the empty string, a
    /// zero-width match, rather than as its `.gist`.
    ///
    /// `assertion` is the `<$name>` form ([`RegexAtom::VarAssertion`]): a
    /// defined value is always a pattern — a `Str` is parsed as regex source
    /// under the same interpolation policy as the parse-time `<$var>` arm —
    /// entered as a capture-isolated group, and an undefined one matches
    /// nothing.
    // Cost: O(r), r = the value's length; a pattern's parse is cached by its
    // source.
    pub(in crate::runtime) fn var_interp_target(
        &self,
        name: &str,
        assertion: bool,
        caps: &RegexCaptures,
        chars: &[char],
        pos: usize,
        (ignore_case, markless): (bool, bool),
    ) -> VarInterpTarget {
        let val = caps
            .regex_vars()
            .get(name)
            .or_else(|| caps.regex_vars().get(&format!("${name}")))
            .cloned()
            .or_else(|| self.env.get(name).cloned())
            .or_else(|| self.env.get(&format!("${name}")).cloned());
        let val = val.map(Value::into_deref);
        if assertion {
            return self.var_assertion_target(val, ignore_case);
        }
        let is_regex = val.as_ref().is_some_and(|v| {
            matches!(v.view(), ValueView::Regex(_) | ValueView::RegexWithAdverbs(_))
        });
        let ref_text = match &val {
            Some(v) if is_regex => Self::regex_alternation_sources(v, false).swap_remove(0),
            Some(v) => match v.view() {
                ValueView::Nil | ValueView::Package(_) => String::new(),
                _ => v.to_string_value(),
            },
            None => String::new(),
        };
        // A markless run compares bases, so a plain value becomes a pattern
        // too: its markless program does the comparison.
        if is_regex || ((ignore_case || markless) && !ref_text.is_empty()) {
            let mut source = String::from(if ignore_case { ":i " } else { "" });
            if is_regex {
                source.push_str(&ref_text);
            } else {
                source.push_str(&Self::escape_regex_scalar_literal(&ref_text));
            }
            return match self.parse_regex(&source) {
                Some(parsed) => VarInterpTarget::Pattern(parsed),
                None => VarInterpTarget::End(None),
            };
        }
        let mut end = pos;
        for want in ref_text.chars() {
            if chars.get(end) != Some(&want) {
                return VarInterpTarget::End(None);
            }
            end += 1;
        }
        VarInterpTarget::End(Some(end))
    }

    /// A call `<name>` of no rule: a builtin (`<wb>`, `<ww>`, `<ws>`, a
    /// character class such as `<alpha>`, a Unicode property `<:Letter>`), or
    /// an unknown name, which raises `X::Method::NotFound` through
    /// `PENDING_REGEX_ERROR`. The end and the capture delta it files, or `None`
    /// for no match.
    // Cost: O(w) for `<ws>`, w = the whitespace it consumes; O(1) for a
    // boundary, a class or a property; O(len) for the literal fallback,
    // len = the name; plus one static rule lookup for an unknown name.
    pub(super) fn regex_builtin_named(
        &mut self,
        spec: &NamedRegexLookupSpec,
        chars: &[char],
        pos: usize,
        pkg: Symbol,
    ) -> Option<(usize, RegexCaptures)> {
        if spec.lookup_name == "wb" && !spec.token_lookup {
            let before_is_word = pos > 0 && is_word_char(chars[pos - 1]);
            let after_is_word = pos < chars.len() && is_word_char(chars[pos]);
            let at_boundary = before_is_word != after_is_word
                || (pos == 0 && after_is_word)
                || (pos == chars.len() && before_is_word);
            if !at_boundary {
                return None;
            }
            return Some((pos, RegexCaptures::default()));
        }
        if spec.lookup_name == "ww" && !spec.token_lookup {
            let before_is_word = pos > 0 && is_word_char(chars[pos - 1]);
            let after_is_word = pos < chars.len() && is_word_char(chars[pos]);
            if !(before_is_word && after_is_word) {
                return None;
            }
            return Some((pos, RegexCaptures::default()));
        }
        if spec.lookup_name == "ws" && !spec.token_lookup {
            let mut end = pos;
            while end < chars.len() && crate::builtins::cclass::is_space(chars[end]) {
                end += 1;
            }
            let before_is_word = pos > 0 && is_word_char(chars[pos - 1]);
            let after_is_word = end < chars.len() && is_word_char(chars[end]);
            if before_is_word && after_is_word && end == pos {
                return None;
            }
            let mut new_caps = RegexCaptures::default();
            if !spec.silent {
                new_caps
                    .named
                    .slot_mut(Symbol::intern(&spec.lookup_name))
                    .nodes
                    .push(std::sync::Arc::new(CapNode {
                        from: pos,
                        to: end,
                        ..Default::default()
                    }));
            }
            return Some((end, new_caps));
        }
        // A builtin character class or `ident` (`alpha`, `digit`, …), also under an alias
        // (`<foo=alpha>`).
        let is_builtin_class = matches!(
            spec.lookup_name.as_str(),
            "alpha"
                | "upper"
                | "lower"
                | "digit"
                | "xdigit"
                | "space"
                | "alnum"
                | "blank"
                | "cntrl"
                | "punct"
                | "graph"
                | "print"
                | "ident"
        );
        if is_builtin_class {
            // `ident` spans `<alpha> <alnum>*`; the rest are one char.
            let end = super::regex_builtin_rule::builtin_rule_end(&spec.lookup_name, chars, pos)
                .flatten()?;
            let mut new_caps = RegexCaptures::default();
            let capture_name = spec
                .capture_name
                .as_deref()
                .or_else(|| (!spec.silent).then_some(spec.lookup_name.as_str()));
            if let Some(capture_name) = capture_name {
                new_caps
                    .named
                    .slot_mut(Symbol::intern(capture_name))
                    .merge(NamedSlot::leaf(pos, end));
                // For <foo=alpha>, also capture under the original name.
                // For <foo=.alpha>, the dot suppresses the original name.
                if spec.capture_name.is_some()
                    && capture_name != spec.lookup_name
                    && !spec.alias_replaces_original
                {
                    new_caps
                        .capture_alias_map_mut()
                        .insert(Symbol::intern(capture_name), spec.lookup_sym);
                    new_caps
                        .named
                        .slot_mut(Symbol::intern(&spec.lookup_name))
                        .merge(NamedSlot::leaf(pos, end));
                }
            }
            return Some((end, new_caps));
        }
        // A Unicode property assertion (`:Letter`, `:!Letter`, `-:Letter`),
        // also under an alias (`<foo=:Letter>`).
        let (uni_prop, uni_negated) = if let Some(prop) = spec.lookup_name.strip_prefix(":!") {
            (Some(prop), true)
        } else if let Some(prop) = spec.lookup_name.strip_prefix("-:") {
            (Some(prop), true)
        } else if let Some(prop) = spec.lookup_name.strip_prefix(':') {
            (Some(prop), false)
        } else {
            (None, false)
        };
        if let Some(prop_name) = uni_prop {
            if pos >= chars.len() {
                return None;
            }
            let matches = check_unicode_property(prop_name, chars[pos]);
            if matches == uni_negated {
                return None;
            }
            let end = pos + 1;
            let mut new_caps = RegexCaptures::default();
            let capture_name = spec
                .capture_name
                .as_deref()
                .or_else(|| (!spec.silent).then_some(spec.lookup_name.as_str()));
            if let Some(capture_name) = capture_name {
                new_caps
                    .named
                    .slot_mut(Symbol::intern(capture_name))
                    .merge(NamedSlot::leaf(pos, end));
            }
            return Some((end, new_caps));
        }
        // Named rule not found — report error for valid identifier names.
        // Skip error for names containing special chars (likely parser
        // artifacts from character class syntax like `<[...]>`).
        //
        // When the first character after the identifier is whitespace, the
        // remainder is a regex argument (`<test hat>`), so the method name is
        // just the leading identifier. Validate and report against that name.
        let method_name = spec
            .lookup_name
            .split_whitespace()
            .next()
            .unwrap_or("")
            .to_string();
        let is_plain_ident = !method_name.is_empty()
            && method_name
                .chars()
                .all(|c| c.is_alphanumeric() || c == '-' || c == '_' || c == ':' || c == '.');
        // If the leading identifier resolves to a known rule/token (e.g. the
        // space form `<lit 'a'>` passes `'a'` as an argument to the defined
        // `lit` token), it is not an unknown-method error — just a subrule
        // call form we do not fully support yet, so fall through.
        let leading_resolves = is_plain_ident
            && !self
                .resolve_token_patterns_static_in_pkg(&method_name, pkg)
                .is_empty();
        if !spec.silent
            && is_plain_ident
            && !leading_resolves
            && !self.has_proto_token_in_pkg(&method_name, pkg)
        {
            // Measuring an LTM prefix (ADR-0125): Rakudo's NFA finds no
            // method by that name and puts a fate there, so the branch
            // ranks with what precedes it. Only a real match reports it.
            if LTM_DECLARATIVE_MODE.with(std::cell::Cell::get) {
                super::regex_ltm_fate::ltm_record_fate(pos);
                return None;
            }
            super::super::regex_parse::PENDING_REGEX_ERROR.with(|e| {
                let msg = format!(
                    "No such method '{}' for invocant of type 'Match'",
                    method_name
                );
                let mut err = RuntimeError::new(msg.clone());
                let mut attrs = std::collections::HashMap::new();
                attrs.insert("message".to_string(), Value::str(msg));
                attrs.insert("method".to_string(), Value::str(method_name.clone()));
                attrs.insert("typename".to_string(), Value::str("Match".to_string()));
                let ex = Value::make_instance(Symbol::intern("X::Method::NotFound"), attrs);
                err.exception = Some(Box::new(ex));
                *e.borrow_mut() = Some(err);
            });
            return None;
        }
        // Anything else matches the name as literal text.
        if pos >= chars.len() || spec.token_lookup {
            return None;
        }
        let literal = &spec.lookup_name;
        let len = literal.chars().count();
        if pos + len > chars.len() || !chars[pos..pos + len].iter().copied().eq(literal.chars()) {
            return None;
        }
        let mut new_caps = RegexCaptures::default();
        let capture_name = spec.capture_name.as_deref().unwrap_or(literal);
        new_caps
            .named
            .slot_mut(Symbol::intern(capture_name))
            .merge(NamedSlot::leaf(pos, pos + len));
        Some((pos + len, new_caps))
    }
}

/// The end of the positional backreference `$idx` matched at `pos`. A
/// quantified slot's text is every iteration's span in turn; a plain slot's is
/// its own span. Both compare against the `chars` the spans were recorded in,
/// so no text is materialized (ADR-0016 P4). A backreference inside an inline
/// sub-pattern resolves against the enclosing level's captures too
/// (`/ (\w) [ $0 ] /`).
// Cost: O(n), n = the characters compared.
fn backref_end(idx: usize, caps: &RegexCaptures, chars: &[char], pos: usize) -> Option<usize> {
    let slot = caps.backref_positional(idx)?;
    let origin = StrippedOrigin::of(chars);
    let mut cursor = pos;
    let mut compare_span = |a: usize, b: usize| -> bool {
        match span_repeats_at(chars, a, b, cursor, origin.as_ref()) {
            Some(next) => {
                cursor = next;
                true
            }
            None => false,
        }
    };
    let ok = match &slot.quantified {
        Some(qlist) => qlist.iter().all(|(a, b, _)| compare_span(*a, *b)),
        None => compare_span(slot.from, slot.to),
    };
    ok.then_some(cursor)
}

/// The end of the named backreference `$<name>` matched at `pos`: its last
/// node's span, read through to the enclosing level as [`backref_end`] does
/// (`/ $<x>=(\w) [ $<x> ] /`).
// Cost: O(n), n = the characters compared.
fn named_backref_end(
    name: &str,
    caps: &RegexCaptures,
    chars: &[char],
    pos: usize,
) -> Option<usize> {
    let sym = Symbol::intern(name);
    let node = caps
        .named
        .get(&sym)
        .and_then(|slot| slot.nodes.last())
        .or_else(|| {
            caps.outer_backref()
                .as_ref()
                .and_then(|outer| outer.lookup_named(&sym))
        })?;
    span_repeats_at(chars, node.from, node.to, pos, StrippedOrigin::of(chars).as_ref())
}

/// Where `chars` sits in the current match target when it is (a suffix of)
/// that target's mark-stripped view: the subject a `:m` pattern runs over
/// (`ignoremark_on_target`, the whole-pattern `:m` entries).
struct StrippedOrigin {
    target: MatchTarget,
    /// The stripped position `chars[0]` stands at.
    offset: usize,
}

impl StrippedOrigin {
    // Cost: O(1).
    fn of(chars: &[char]) -> Option<Self> {
        let target = super::regex_helpers::current_match_target()?;
        let stripped = target.stripped_if_built()?;
        if stripped.is_unchanged() {
            return None;
        }
        // A suffix of the stripped view: the same allocation, ending where it
        // ends. Only the addresses are compared; nothing is dereferenced.
        let whole = stripped.chars();
        let bytes = (chars.as_ptr() as usize).checked_sub(whole.as_ptr() as usize)?;
        let offset = bytes / std::mem::size_of::<char>();
        (offset + chars.len() == whole.len()).then_some(Self { target, offset })
    }
}

/// The end of the text `chars[a..b]` repeated at `cursor`, or `None`. Over a
/// mark-stripped view (`origin`) the two spans are compared in the original
/// subject, marks included: rakudo compares a backreference exactly even
/// under `:m`, as it does under `:i`.
// Cost: O(n), n = the characters compared.
fn span_repeats_at(
    chars: &[char],
    a: usize,
    b: usize,
    cursor: usize,
    origin: Option<&StrippedOrigin>,
) -> Option<usize> {
    let (a, b) = (a.min(chars.len()), b.min(chars.len()));
    let (a, b) = (a, b.max(a));
    let len = b - a;
    if cursor + len > chars.len() || chars[cursor..cursor + len] != chars[a..b] {
        return None;
    }
    if let Some(origin) = origin
        && let Some(stripped) = origin.target.stripped_if_built()
    {
        let orig = origin.target.chars();
        let to_orig = |p: usize| stripped.stripped_to_original(p + origin.offset).min(orig.len());
        let (oa, ob) = (to_orig(a), to_orig(b));
        let (oc, od) = (to_orig(cursor), to_orig(cursor + len));
        if orig[oa..ob.max(oa)] != orig[oc..od.max(oc)] {
            return None;
        }
    }
    Some(cursor + len)
}
