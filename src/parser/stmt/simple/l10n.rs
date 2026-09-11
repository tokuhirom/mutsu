//! L10N slang vocabularies — localized spellings of Raku's keywords and core
//! routine names (ADR-0026's vocabulary axis).
//!
//! An `L10N::XX` distribution (`L10N::JA`, `L10N::DE`, ...) is a role whose
//! members are `token <category>-<name> { <literal> }` declarations, one per
//! translatable piece of Raku's surface syntax. Rakudo mixes that role into the
//! MAIN slang grammar, so each token *overrides* the grammar production that
//! recognizes the corresponding piece. mutsu's parser is a hand-written
//! recursive descent with no grammar to mix into, so — exactly as ADR-0026 does
//! for Slang::Tuxic's rule overrides — the role's token names are *interpreted*
//! rather than executed: the category tells us which parser production the
//! localized spelling belongs to, and the token body supplies the spelling.
//!
//! Two kinds of entry, distinguished by category, matching what rakudo does
//! (measured against `raku -I lib -e '"...".AST("JA")'` for `L10N::JA` 0.0.3):
//!
//! - **Replacement** (`block-if` → `もしも`): the localized spelling *takes the
//!   place of* the ASCII keyword. Under an active JA vocabulary `if 1 { }` no
//!   longer parses, and `もしも 1 { }` does.
//! - **Alias** (`core-say` → `言う`): the localized spelling is an *additional*
//!   name for a core routine. Both `say 42` and `言う 42` parse.
//!
//! Only the categories whose parser production mutsu consults here are wired
//! up; the rest (`infix-`, `named-`, `adverb-`, `meta-`, `quote-lang-`) are
//! accepted and recorded as inert. An inert entry means source written with
//! that localized spelling fails to *parse*, loudly — it can never make
//! existing syntax silently mean something else, which is the failure mode
//! ADR-0026's hard-error rule exists to prevent. The residue is tracked in
//! <https://github.com/tokuhirom/mutsu/issues/7990>.

use std::cell::{Cell, RefCell};
use std::collections::HashMap;
use std::rc::Rc;

/// Token-key categories whose localized spelling *replaces* the ASCII keyword.
/// Every one of these names a production the parser reaches through
/// [`crate::parser::stmt::keyword`].
const REPLACEMENT_CATEGORIES: &[&str] = &[
    "block-",
    "modifier-",
    "scope-",
    "routine-",
    "package-",
    "multi-",
    "typer-",
    "use-",
    "stmt-prefix-",
    "phaser-",
    "constraint-",
    "traitmod-",
    "prefix-",
];

/// Token-key categories whose localized spelling is an *additional* name for a
/// routine/term/enum value, reached through the bareword term production
/// (`primary::ident::identifier_or_call`). The ASCII name keeps working:
/// measured, under `L10N::JA` both `True` and `正` parse.
///
/// `term-` sits here rather than with the replacements because mutsu reaches
/// `now` / `self` / `time` as barewords, not through [`crate::parser::stmt::keyword`].
/// The one divergence that costs: rakudo drops the ASCII `now` once `term-now`
/// is translated, and mutsu keeps accepting it. Accepting more than rakudo
/// cannot mis-parse a program rakudo accepts.
const ALIAS_CATEGORIES: &[&str] = &["core-", "enum-", "term-", "system-", "pragma-", "trait-is-"];

/// Categories that belong to the L10N schema but whose grammatical position
/// mutsu's parser does not consult yet: word infix operators, metaoperators,
/// quote-language names, named arguments and adverbs. Recognized so a role
/// declaring them is still a valid vocabulary — see the module docs on why
/// inert is safe where an unknown *production* override is not.
const INERT_CATEGORIES: &[&str] = &["infix-", "meta-", "quote-lang-", "named-", "adverb-"];

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum Category {
    Replacement,
    Alias,
    Inert,
}

/// The spellings accepted for one canonical keyword. A canonical keyword can
/// collect more than one spelling: `block-if` and `modifier-if` translate the
/// same `if` differently (`もしも` vs `の場合`), and mutsu's `keyword()` does
/// not know which grammatical position it was called from, so it accepts
/// either. That over-accepts relative to rakudo (a statement-modifier spelling
/// in block position parses here, and would not there) but never mis-parses
/// anything rakudo accepts.
#[derive(Clone, Default, PartialEq, Eq, Debug)]
struct KeywordSpellings {
    spellings: Vec<String>,
    /// Set once a [`Category::Replacement`] entry contributed: the canonical
    /// ASCII spelling stops being accepted.
    replaces: bool,
}

/// One language's localized surface syntax.
#[derive(Clone, Default, PartialEq, Eq, Debug)]
pub(crate) struct L10nVocabulary {
    /// Canonical keyword → the spellings that stand in for it.
    keywords: HashMap<String, KeywordSpellings>,
    /// Localized bareword → the canonical routine/term/enum name.
    aliases: HashMap<String, String>,
    /// Every localized spelling → the canonical keyword it stands for, the
    /// reverse of `keywords`. Answers "is this word reserved?".
    canonical: HashMap<String, String>,
    /// Token keys whose category mutsu does not consult yet, kept so the
    /// residue is reportable rather than invisible.
    inert: Vec<String>,
}

impl L10nVocabulary {
    /// Add one `token <key> { <body> }` from an `L10N::XX` role.
    ///
    /// `body` is the token's raw regex source as the parser recorded it
    /// (`":ratchet もしも"`); [`literal_of`] reduces it to the spelling.
    ///
    /// Returns `false` when `key` is not an L10N vocabulary token at all — it
    /// names no category of the L10N schema, or its body is not a plain
    /// literal. The caller must then treat the override as a *production*
    /// override (Slang::Tuxic's kind) and hard-error if it does not recognize
    /// it, per ADR-0026 §2.2.
    pub(crate) fn try_insert_token(&mut self, key: &str, body: Option<&str>) -> bool {
        let Some((category, canonical)) = categorize(key) else {
            return false;
        };
        if category == Category::Inert {
            self.inert.push(key.to_string());
            return true;
        }
        let Some(literal) = body.and_then(literal_of) else {
            return false;
        };
        match category {
            Category::Replacement => {
                let entry = self.keywords.entry(canonical.to_string()).or_default();
                entry.replaces = true;
                push_spelling(entry, literal);
                // A language that leaves a keyword untranslated spells it as
                // itself; recording that identity would make the reserved-word
                // lookup in `is_reserved_infix_word` recurse forever.
                if literal != canonical {
                    self.canonical
                        .insert(literal.to_string(), canonical.to_string());
                }
            }
            Category::Alias => {
                self.insert_alias(literal, canonical);
            }
            Category::Inert => unreachable!("handled above"),
        }
        true
    }

    /// Add the `localized => canonical` pairs an L10N role's `<category>2ast`
    /// method carries in its `%mapping` constant.
    ///
    /// This is where `core-say` actually lives: the generated role translates
    /// core routine names through `method core2ast`, not through a `token`, so
    /// the identifier-position half of a vocabulary comes from here.
    pub(crate) fn insert_aliases<'a>(&mut self, pairs: impl Iterator<Item = (&'a str, &'a str)>) {
        for (localized, canonical) in pairs {
            if localized.is_empty() || canonical.is_empty() || localized == canonical {
                continue;
            }
            self.insert_alias(localized, canonical);
        }
    }

    fn insert_alias(&mut self, localized: &str, canonical: &str) {
        self.aliases
            .insert(localized.to_string(), canonical.to_string());
        // A core routine mutsu's parser reaches as a keyword rather than as a
        // bareword term (`return` → `リターン`) still has to match, so an alias
        // is offered to `keyword()` too — without disabling the ASCII spelling.
        let entry = self.keywords.entry(canonical.to_string()).or_default();
        push_spelling(entry, localized);
        self.canonical
            .insert(localized.to_string(), canonical.to_string());
    }

    pub(crate) fn is_empty(&self) -> bool {
        self.keywords.is_empty() && self.aliases.is_empty()
    }

    /// Token keys this implementation recorded but does not consult.
    #[cfg(test)]
    pub(crate) fn inert(&self) -> &[String] {
        &self.inert
    }
}

fn push_spelling(entry: &mut KeywordSpellings, spelling: &str) {
    if !entry.spellings.iter().any(|s| s == spelling) {
        entry.spellings.push(spelling.to_string());
    }
}

/// Split an L10N token key into the parser production it belongs to and the
/// canonical Raku name it translates. The longest matching category prefix
/// wins, so `trait-is-copy` is the `copy` trait argument rather than anything
/// under a shorter prefix.
fn categorize(key: &str) -> Option<(Category, &str)> {
    let mut best: Option<(Category, &str, usize)> = None;
    let candidates = REPLACEMENT_CATEGORIES
        .iter()
        .map(|c| (Category::Replacement, *c))
        .chain(ALIAS_CATEGORIES.iter().map(|c| (Category::Alias, *c)))
        .chain(INERT_CATEGORIES.iter().map(|c| (Category::Inert, *c)));
    for (category, prefix) in candidates {
        let Some(rest) = key.strip_prefix(prefix) else {
            continue;
        };
        if rest.is_empty() {
            continue;
        }
        if best.is_none_or(|(_, _, len)| prefix.len() > len) {
            best = Some((category, rest, prefix.len()));
        }
    }
    best.map(|(category, rest, _)| (category, rest))
}

/// Reduce a token body to the literal spelling it matches, or `None` when the
/// body is not a plain literal.
///
/// L10N token bodies are always one literal — a bare run of text
/// (`:ratchet もしも`) or a quoted string when it contains characters the regex
/// grammar would otherwise read as syntax (`:ratchet "(続き)"`). Anything else
/// would need the regex engine to decide what it matches, which is precisely
/// the Rakudo-internal-token execution ADR-0026 §4 rejects.
fn literal_of(body: &str) -> Option<&str> {
    let body = body.trim();
    // The parser records a `token`'s implicit `:ratchet` in the regex source.
    let body = body.strip_prefix(":ratchet").unwrap_or(body).trim();
    if body.len() >= 2 {
        for quote in ['"', '\''] {
            if let Some(inner) = body.strip_prefix(quote).and_then(|b| b.strip_suffix(quote))
                && !inner.is_empty()
                && !inner.contains(quote)
                && !inner.contains('\\')
            {
                return Some(inner);
            }
        }
    }
    if body.is_empty() || body.chars().any(is_regex_metachar) {
        return None;
    }
    Some(body)
}

/// Characters that make an *unquoted* token body regex syntax rather than a
/// literal spelling.
fn is_regex_metachar(c: char) -> bool {
    matches!(
        c,
        '\\' | '|'
            | '*'
            | '+'
            | '?'
            | '['
            | ']'
            | '('
            | ')'
            | '{'
            | '}'
            | '<'
            | '>'
            | '$'
            | '.'
            | '^'
            | '"'
            | '\''
            | ':'
    ) || c.is_whitespace()
}

thread_local! {
    /// Fast bail for the `keyword()` hot path: true only while a vocabulary is
    /// installed, so the ordinary (non-localized) parse pays one `Cell` read.
    static L10N_ACTIVE: Cell<bool> = const { Cell::new(false) };
    static L10N_VOCABULARY: RefCell<Option<Rc<L10nVocabulary>>> = const { RefCell::new(None) };
    /// The vocabulary the *next* unit parse starts under. `reset_user_subs`
    /// clears the live vocabulary and then installs this, the same way
    /// `EVAL_LANGUAGE_VERSION_PRESEED` re-establishes an EVAL's language
    /// revision after the reset.
    ///
    /// Consumed by that first reset, deliberately: the `Str.AST($slang)` unit
    /// is localized, but a module it `use`s is a unit of its own and parses in
    /// stock Raku. Were the seed to survive, a module scanned from inside a
    /// localized parse would be read under the caller's vocabulary and quietly
    /// lose every statement whose keywords it no longer spells.
    static L10N_PRESEED: RefCell<Option<Rc<L10nVocabulary>>> = const { RefCell::new(None) };
}

pub(crate) fn set_l10n_vocabulary(vocabulary: Option<Rc<L10nVocabulary>>) {
    L10N_ACTIVE.with(|a| a.set(vocabulary.is_some()));
    L10N_VOCABULARY.with(|v| *v.borrow_mut() = vocabulary);
}

/// Seed the vocabulary a nested parse (`Str.AST($slang)`) starts under. `None`
/// restores the plain, unlocalized default of a fresh compilation unit.
pub(crate) fn set_l10n_preseed(vocabulary: Option<Rc<L10nVocabulary>>) {
    L10N_PRESEED.with(|p| *p.borrow_mut() = vocabulary);
}

/// Re-establish the preseeded vocabulary after `reset_user_subs` cleared the
/// unit's parser state, consuming the seed (see `L10N_PRESEED`).
pub(in crate::parser) fn apply_l10n_preseed() {
    let preseed = L10N_PRESEED.with(|p| p.borrow_mut().take());
    set_l10n_vocabulary(preseed);
}

pub(in crate::parser) fn l10n_vocabulary_snapshot() -> Option<Rc<L10nVocabulary>> {
    L10N_VOCABULARY.with(|v| v.borrow().clone())
}

pub(in crate::parser) fn restore_l10n_vocabulary(saved: Option<Rc<L10nVocabulary>>) {
    set_l10n_vocabulary(saved);
}

/// Match canonical keyword `kw` at the start of `input` under the active
/// vocabulary.
///
/// Returns `None` when the vocabulary does not govern this keyword — no
/// vocabulary installed, the keyword is untranslated, or it has only *alias*
/// spellings and none of them matched — and the caller must then perform its
/// ordinary ASCII match. `Some(result)` is the decision: a localized spelling
/// matched, or the keyword is replaced and so its ASCII spelling must not.
pub(crate) fn l10n_match_keyword<'a>(kw: &str, input: &'a str) -> Option<Option<&'a str>> {
    if !L10N_ACTIVE.with(Cell::get) {
        return None;
    }
    L10N_VOCABULARY.with(|v| {
        let borrowed = v.borrow();
        let entry = borrowed.as_ref()?.keywords.get(kw)?;
        for spelling in &entry.spellings {
            if let Some(rest) = match_spelling(spelling, input) {
                return Some(Some(rest));
            }
        }
        // A replaced keyword's ASCII spelling is gone: `if` does not parse
        // while a vocabulary that translates it is in force.
        entry.replaces.then_some(None)
    })
}

/// The canonical routine/term name a localized bareword stands for.
pub(crate) fn l10n_alias(name: &str) -> Option<String> {
    if !L10N_ACTIVE.with(Cell::get) {
        return None;
    }
    L10N_VOCABULARY.with(|v| v.borrow().as_ref()?.aliases.get(name).cloned())
}

/// The canonical Raku keyword a localized spelling stands for, for the checks
/// that ask "is this word reserved?" rather than "does this word appear here?".
///
/// Without it a localized keyword is not reserved anywhere the parser tests a
/// *word* against a fixed list — and `1 の場合 1` was read as a user-defined
/// word infix operator named `の場合` instead of as `1 if 1`.
pub(crate) fn l10n_canonical_keyword(name: &str) -> Option<String> {
    if !L10N_ACTIVE.with(Cell::get) {
        return None;
    }
    L10N_VOCABULARY.with(|v| v.borrow().as_ref()?.canonical.get(name).cloned())
}

/// A localized spelling matches only at an identifier boundary, so `もしも`
/// does not match inside `もしもし`. The ASCII-byte boundary test
/// `crate::parser::helpers::is_ident_char` uses cannot answer that — every
/// continuation byte of a multi-byte character fails it — so decode the next
/// character instead.
fn match_spelling<'a>(spelling: &str, input: &'a str) -> Option<&'a str> {
    let rest = input.strip_prefix(spelling)?;
    match rest.chars().next() {
        Some(c)
            if c == '-' || c == '\'' || crate::parser::helpers::is_raku_identifier_continue(c) =>
        {
            None
        }
        _ => Some(rest),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn ja() -> L10nVocabulary {
        let mut vocabulary = L10nVocabulary::default();
        assert!(vocabulary.try_insert_token("block-if", Some(":ratchet もしも")));
        assert!(vocabulary.try_insert_token("modifier-if", Some(":ratchet の場合")));
        assert!(vocabulary.try_insert_token("scope-my", Some(":ratchet 私の")));
        assert!(vocabulary.try_insert_token("enum-True", Some(":ratchet 正")));
        assert!(vocabulary.try_insert_token("infix-pcontp", Some(":ratchet \"(続き)\"")));
        vocabulary.insert_aliases([("言う", "say"), ("リターン", "return")].into_iter());
        vocabulary
    }

    #[test]
    fn replacement_category_takes_the_place_of_the_ascii_keyword() {
        set_l10n_vocabulary(Some(Rc::new(ja())));
        assert_eq!(
            l10n_match_keyword("if", "もしも 1 { }"),
            Some(Some(" 1 { }"))
        );
        // `if` itself no longer parses: rakudo's `.AST("JA")` rejects it too.
        assert_eq!(l10n_match_keyword("if", "if 1 { }"), Some(None));
        set_l10n_vocabulary(None);
    }

    #[test]
    fn both_spellings_of_one_canonical_keyword_are_accepted() {
        set_l10n_vocabulary(Some(Rc::new(ja())));
        assert_eq!(l10n_match_keyword("if", "の場合 $x"), Some(Some(" $x")));
        set_l10n_vocabulary(None);
    }

    #[test]
    fn alias_category_leaves_the_ascii_name_working() {
        set_l10n_vocabulary(Some(Rc::new(ja())));
        // No decision for the ASCII spelling — the caller matches `say` itself.
        assert_eq!(l10n_match_keyword("say", "say 42"), None);
        assert_eq!(l10n_match_keyword("say", "言う 42"), Some(Some(" 42")));
        assert_eq!(l10n_alias("言う").as_deref(), Some("say"));
        // An `enum-` token is an alias too: `True` keeps working.
        assert_eq!(l10n_match_keyword("True", "True"), None);
        assert_eq!(l10n_alias("正").as_deref(), Some("True"));
        set_l10n_vocabulary(None);
    }

    #[test]
    fn an_untranslated_keyword_is_not_governed() {
        set_l10n_vocabulary(Some(Rc::new(ja())));
        assert_eq!(l10n_match_keyword("sub", "sub foo { }"), None);
        set_l10n_vocabulary(None);
    }

    #[test]
    fn nothing_is_governed_without_a_vocabulary() {
        set_l10n_vocabulary(None);
        assert_eq!(l10n_match_keyword("if", "もしも 1"), None);
        assert_eq!(l10n_alias("言う"), None);
    }

    #[test]
    fn a_spelling_matches_only_at_an_identifier_boundary() {
        set_l10n_vocabulary(Some(Rc::new(ja())));
        // `もしもし` is one identifier, not the `if` keyword plus `し`.
        assert_eq!(l10n_match_keyword("if", "もしもし"), Some(None));
        set_l10n_vocabulary(None);
    }

    #[test]
    fn quoted_token_bodies_lose_their_quotes() {
        assert_eq!(literal_of(":ratchet \"(続き)\""), Some("(続き)"));
        assert_eq!(literal_of(":ratchet もしも"), Some("もしも"));
        assert_eq!(literal_of(":ratchet \"^ff\""), Some("^ff"));
    }

    #[test]
    fn a_non_literal_token_body_is_not_a_vocabulary_entry() {
        assert_eq!(literal_of(":ratchet <alpha>+"), None);
        assert_eq!(literal_of(":ratchet"), None);
        let mut vocabulary = L10nVocabulary::default();
        // Not a literal → not an L10N token, so the caller falls through to the
        // production-override map and hard-errors there rather than guessing.
        assert!(!vocabulary.try_insert_token("block-if", Some(":ratchet <alpha>+")));
        assert!(vocabulary.is_empty());
    }

    #[test]
    fn a_production_override_is_not_mistaken_for_a_vocabulary_token() {
        let mut vocabulary = L10nVocabulary::default();
        assert!(!vocabulary.try_insert_token("methodop", None));
        assert!(!vocabulary.try_insert_token("term:sym<identifier>", None));
        assert!(vocabulary.is_empty());
    }

    #[test]
    fn unwired_categories_are_recorded_inert_not_rejected() {
        let vocabulary = ja();
        assert_eq!(vocabulary.inert(), ["infix-pcontp"]);
    }

    #[test]
    fn the_longest_category_prefix_wins() {
        assert_eq!(categorize("trait-is-copy"), Some((Category::Alias, "copy")));
        assert_eq!(
            categorize("stmt-prefix-do"),
            Some((Category::Replacement, "do"))
        );
        assert_eq!(
            categorize("traitmod-is"),
            Some((Category::Replacement, "is"))
        );
        assert_eq!(categorize("infix-and"), Some((Category::Inert, "and")));
        assert_eq!(categorize("methodop"), None);
    }
}
