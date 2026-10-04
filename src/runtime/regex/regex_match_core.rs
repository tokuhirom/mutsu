//! The backtracking regex engine: a depth-first walk over pattern tokens with
//! a single mutable capture store + undo trail (ADR-0007).
//!
//! Atom candidate producers (`regex_match_atom_all_with_capture_in_pkg`,
//! `regex_match_atom_with_capture_in_pkg`, `match_separated_quantifier`)
//! return `(end, delta)` pairs where the delta is a `RegexCaptures` relative
//! to an EMPTY baseline. The walk applies a candidate with
//! `CapStore::merge_delta`, descends to the next token, and rewinds the trail
//! on backtrack — per-step capture cost is O(delta), never O(accumulated).

use super::super::*;
use super::regex_trail::CapStore;
use std::collections::HashSet;

impl Interpreter {
    /// Collect all named capture names inside a regex atom (recursively).
    fn collect_named_captures_in_atom(atom: &RegexAtom, out: &mut HashSet<String>) {
        match atom {
            RegexAtom::Named(name) => {
                let spec = name.spec();
                if !spec.silent {
                    // A non-suppressing alias captures under BOTH names (see
                    // `also_under_original` in `regex_match_atom.rs`), so both are
                    // quantified here — `[ <tags=tag-directive> ]+` must leave
                    // `$/<tag-directive>` a LIST, not a bare Match (YAMLish reads
                    // it back as `@<tag-directive>».ast.list`).
                    let alias = spec.capture_name.clone();
                    if let Some(alias) = alias {
                        if !alias.is_empty() {
                            out.insert(alias.clone());
                        }
                        if !spec.alias_replaces_original
                            && alias != spec.lookup_name
                            && !spec.lookup_name.is_empty()
                        {
                            out.insert(spec.lookup_name.clone());
                        }
                    } else if !spec.lookup_name.is_empty() {
                        out.insert(spec.lookup_name.clone());
                    }
                }
            }
            // A CAPTURING group is a capture boundary: a named capture inside
            // `( <element> ',' )*` belongs to each group's own Match (reached
            // via `$0[n]<element>`), NOT to the enclosing Match — raku leaves
            // `$/<element>` entirely absent there. Only a non-capturing group
            // (`[ <e> ]*`) exposes its inner names to the quantified parent
            // (as lists). So do not descend into CaptureGroup.
            RegexAtom::Group(pat) => {
                for tok in &pat.tokens {
                    if let Some(name) = tok.named_capture.as_ref() {
                        out.insert(name.clone());
                    }
                    Self::collect_named_captures_in_atom(&tok.atom, out);
                }
            }
            RegexAtom::Alternation(alts) | RegexAtom::SequentialAlternation(alts) => {
                for alt in alts {
                    for tok in &alt.tokens {
                        if let Some(name) = tok.named_capture.as_ref() {
                            out.insert(name.clone());
                        }
                        Self::collect_named_captures_in_atom(&tok.atom, out);
                    }
                }
            }
            _ => {}
        }
    }

    /// Collect names that sit under a nested LIST quantifier (`*`, `+`, `**`,
    /// `%`-separated) inside `atom`. When an enclosing `?` group matches zero
    /// times, raku still renders those names as EMPTY LISTS (`'/' [ <seg-nz>
    /// [ '/' <seg> ]* ]?` matching "/" leaves `$<seg>` = []), while names
    /// under only `?`/unquantified positions stay absent (Nil).
    pub(super) fn collect_nested_list_quantified_names(
        atom: &RegexAtom,
        out: &mut HashSet<String>,
    ) {
        let visit_tok = |tok: &RegexToken, out: &mut HashSet<String>| {
            let is_list = matches!(
                tok.quant,
                RegexQuant::ZeroOrMore
                    | RegexQuant::OneOrMore
                    | RegexQuant::Repeat(..)
                    | RegexQuant::RepeatCode(_)
            ) || tok.separator.is_some();
            if is_list {
                out.extend(Self::collect_quantified_names_for_token(tok));
            } else {
                Self::collect_nested_list_quantified_names(&tok.atom, out);
            }
        };
        match atom {
            RegexAtom::Group(pat) => {
                for tok in &pat.tokens {
                    visit_tok(tok, out);
                }
            }
            RegexAtom::Alternation(alts) | RegexAtom::SequentialAlternation(alts) => {
                for alt in alts {
                    for tok in &alt.tokens {
                        visit_tok(tok, out);
                    }
                }
            }
            _ => {}
        }
    }

    /// Collect all named capture names from a regex token.
    pub(super) fn collect_quantified_names_for_token(token: &RegexToken) -> HashSet<String> {
        let mut names = HashSet::new();
        if let Some(name) = token.named_capture.as_ref() {
            names.insert(name.clone());
        }
        Self::collect_named_captures_in_atom(&token.atom, &mut names);
        // A `%` separator's own captures are quantified too: `<a> *% <sep>`
        // matching a single atom (zero separators) must leave `$<sep>` an empty
        // list, not an absent capture, exactly as a zero-iteration `[ <sep> ]*`
        // does. Every caller is a quantified context, and a token with no
        // separator adds nothing here.
        if let Some(sep) = token.separator.as_ref() {
            for tok in &sep.pattern.tokens {
                if let Some(name) = tok.named_capture.as_ref() {
                    names.insert(name.clone());
                }
                Self::collect_named_captures_in_atom(&tok.atom, &mut names);
            }
        }
        names
    }

    /// The first (highest-priority) match of `pattern` at `start`.
    // Cost: the match itself (`rx_match_first`).
    pub(super) fn regex_match_end_from_caps_in_pkg(
        &mut self,
        pattern: &RegexPattern,
        chars: &[char],
        start: usize,
        pkg: Symbol,
    ) -> Option<(usize, RegexCaptures)> {
        self.rx_match_first(pattern, chars, start, pkg)
    }

    /// Every end of `pattern` at `start`, highest priority first.
    // Cost: the whole backtracking search (`rx_match_ends`).
    pub(in crate::runtime) fn regex_match_ends_from_caps_in_pkg(
        &mut self,
        pattern: &RegexPattern,
        chars: &[char],
        start: usize,
        pkg: Symbol,
    ) -> Vec<(usize, RegexCaptures)> {
        self.rx_match_ends(pattern, chars, start, pkg, false)
    }

    /// Like `regex_match_ends_from_caps_in_pkg`, but stops as soon as a match
    /// covers the whole subject. `Grammar.parse` wants exactly one such match,
    /// and an ordered alternation must not keep entering later branches
    /// (running their `{ ... }` blocks) after the parse already succeeded
    /// through an earlier one.
    // Cost: the backtracking search up to the first full match.
    pub(in crate::runtime) fn regex_match_ends_stop_at_full(
        &mut self,
        pattern: &RegexPattern,
        chars: &[char],
        start: usize,
        pkg: Symbol,
    ) -> Vec<(usize, RegexCaptures)> {
        self.rx_match_ends(pattern, chars, start, pkg, true)
    }

    /// Apply a `$<name>=` / `$N=` capture alias for `token` to the store.
    /// `pos_base` is the store's positional length at token start.
    pub(super) fn store_apply_named_capture(
        store: &mut CapStore,
        token: &RegexToken,
        from: usize,
        to: usize,
        pos_base: usize,
    ) {
        let Some(name) = token.named_capture.as_ref() else {
            return;
        };
        // A named capture group `$<x>=(...)` aliases the group to the name and
        // does NOT consume a positional number (Raku: `/$<x>=(\w)(\d)/` makes
        // `$<x>` the \w and `$0` the \d). When this named token's atom is itself
        // a capturing group, it pushed a parent positional during matching;
        // drop those entries so the following `(...)` keeps the next number.
        // A named NON-capturing group `$<x>=[...]` leaves its inner captures'
        // positional numbers intact (its atom is not a CaptureGroup), so this
        // truncation correctly does not fire for it.
        // When aliasing a capturing group, preserve the group's own inner
        // captures (named subrules etc.) so e.g. `$<family>=(<ident>)` keeps
        // `$<family><ident>` accessible — otherwise truncating the group's
        // positional entry would discard its nested subcapture.
        let mut group_subcap: Option<std::sync::Arc<CapNode>> = None;
        if matches!(token.atom, RegexAtom::CaptureGroup(_))
            && store.caps().positional.len() > pos_base
        {
            group_subcap = store
                .caps()
                .positional
                .get(pos_base)
                .and_then(|slot| slot.subcap.clone());
            store.truncate_positional(pos_base);
        }
        // The alias entry is one span-bearing node (ADR-0016 P4): the pinned
        // group subcap when the atom is a capturing group; for an aliased
        // subrule call (`$<pl> = <G::list>`) the called rule's full sub-match
        // tree, which the rule already merged into the store under its own
        // capture key (Raku keeps BOTH captures — `$/.keys` is `(G::list pl)`);
        // else a minimal span carrier, which keeps `.from`/`.to` exact even
        // for a zero-width match (`$<delim>=<[a..z]>*` matching empty).
        let (subrule_subcap, reused_silent_marker) = if group_subcap.is_none() {
            Self::aliased_subrule_subcap(store, token, from, to)
        } else {
            (None, false)
        };
        // An alias on anything but a subrule call (`$<x>=(…)`, `$<x>=[…]`,
        // `$<x>=<:!Cc>*`, `$<x>=\S+`) names a capture, not a rule: no cursor
        // reduces there, so the grammar action walk must not dispatch an
        // action method named after the alias (rakudo calls `method x` only
        // for `<x>`). An empty rule name marks that — the same name the walk
        // already gives a positional `( )` group — while the walk still
        // descends into the node's own captures.
        let no_rule = || Some(Symbol::intern(""));
        let mut sub = if let Some(mut gs) = group_subcap.take() {
            // Keep the group's nested captures and its own span: the group's
            // extent, already narrowed by a `<(` / `)>` inside it
            // (`$<x>=(a )> b)` is `a`, #11570; see `capture_group_span`).
            let gsm = std::sync::Arc::make_mut(&mut gs);
            if gsm.action_name.is_none() {
                gsm.action_name = no_rule();
            }
            gs
        } else if let Some(sc) = subrule_subcap {
            sc
        } else {
            std::sync::Arc::new(CapNode {
                from,
                to,
                action_name: if matches!(token.atom, RegexAtom::Named(_)) {
                    None
                } else {
                    no_rule()
                },
                ..Default::default()
            })
        };
        if reused_silent_marker {
            Self::drop_reused_silent_marker(store, token);
        }
        // `$<alias>=<.subrule>` is a visible alias around a silent subrule.
        // The silent call itself does not create a named capture, so the alias
        // would otherwise be only a span carrier and the subrule's action would
        // never be dispatched. Preserve the original rule name on the alias
        // node, just as the `<alias=.subrule>` spelling does in the matcher.
        if let RegexAtom::Named(atom_name) = &token.atom {
            let spec = atom_name.spec();
            if spec.silent && !spec.lookup_name.is_empty() && sub.action_name.is_none() {
                std::sync::Arc::make_mut(&mut sub).action_name = Some(spec.lookup_sym);
            }
        }
        // A sigil-prefixed alias (`$<alias> = <rule>`) shares the subrule's
        // capture node with the original rule-name entry, just like the
        // angle-bracket form (`<alias=rule>`). Tag that shared node with the
        // original rule name so the grammar action walk can dispatch through
        // the alias even when the alias entry is visited first. This must be
        // per node: one alias name can select different rules in different
        // alternatives (`$<part> = <text> || $<part> = <code>`).
        if let RegexAtom::Named(atom_name) = &token.atom {
            let spec = atom_name.spec();
            if !spec.silent
                && !spec.lookup_name.is_empty()
                && name != &spec.lookup_name
                && let Some(original) = store
                    .caps_mut()
                    .named
                    .get_mut(&Symbol::intern(&spec.lookup_name))
                    .and_then(|slot| slot.nodes.last_mut())
                && original.from == from
                && original.to == to
                && std::sync::Arc::ptr_eq(original, &sub)
            {
                let node = std::sync::Arc::make_mut(original);
                node.action_name = Some(spec.lookup_sym);
                sub = std::sync::Arc::clone(original);
            }
        }
        store.push_named_node(name, sub);
        // `@<name>=` array-sigil alias forces list context: mark the name as
        // quantified so the Match builder always presents it as a List, even
        // for a single non-quantified capture (`@<foo>=(.(.))` → `[«bc»]`).
        if token.force_list_capture {
            store.insert_named_quantified(name.clone());
        }
        // Also capture under the secondary name (e.g., original builtin class name
        // when using `$<alias>=<builtin_class>` syntax).
        if let Some(secondary) = token.secondary_named_capture.as_ref() {
            store.push_named_node(
                secondary,
                std::sync::Arc::new(CapNode {
                    from,
                    to,
                    ..Default::default()
                }),
            );
        }
    }

}
