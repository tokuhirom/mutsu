//! "Can this rule re-enter itself *without consuming input*?", answered over
//! the rule call graph restricted to calls made at a rule's start position
//! (#9579).
//!
//! ADR-0073 Slice 2 lets a `<subrule>` call under a ratcheted caller walk the
//! callee with `first_only`: the caller cannot backtrack into it, so only its
//! highest-priority end can ever be used. The eager arm gates that on
//! [`super::regex_subrule_lazy::pattern_is_rule_call_free`], because a
//! `first_only` walk can stop before reaching the branch that would re-enter
//! the rule and so hide a left recursion from the growing-seed loop (see that
//! module). A rule that calls any rule at all — every interior rule of a real
//! grammar — therefore still enumerated its whole end set. For a recursive,
//! ratcheted rule like
//!
//! ```raku
//! token A { '{' [ <A> | . ]*? '}' }
//! ```
//!
//! that is exponential: every `<A>` enumerates every end of the nested `<A>`s
//! it contains, although the caller will only ever use the first.
//!
//! The hazard is re-entering the SAME left-recursion key, i.e. the same rule at
//! the same position. A rule call is only made at the rule's start position
//! when everything before it in the body can match the empty string, so
//! following only those *left* call edges and asking whether the rule's own
//! name comes back decides it. When it cannot, no rule call can consult the
//! seed, and `first_only` is exactly as sound as it is for a rule-call-free
//! body. User code is treated the way `pattern_is_rule_call_free` treats it
//! (it has the same runtime escape), and anything whose target cannot be named
//! answers "may re-enter".
//!
//! Nullability is over-approximated in the safe direction: an atom is taken to
//! consume only when it provably does (a literal, a character class, `.`), so
//! a construct this walk does not model makes more calls "left", never fewer.

use std::cell::RefCell;
use std::collections::VecDeque;

use rustc_hash::{FxHashMap as HashMap, FxHashSet as HashSet};

use super::super::*;
use crate::runtime::regex_types::{RegexAtom, RegexPattern, RegexQuant, RegexToken};

/// A `(package, rule name)` node of the call graph, interned.
type RuleNode = (Symbol, Symbol);

/// Same ceiling as the full call-graph walk.
const MAX_REACHABLE_RULES: usize = 512;

thread_local! {
    /// `(pkg, name) -> the rules its candidates can call at their start
    /// position`, `None` when some construct's target cannot be named. Keyed
    /// by the token generation like every other call-graph memo.
    #[allow(clippy::type_complexity)]
    static LEFT_CALLS: RefCell<(u64, HashMap<RuleNode, Option<std::sync::Arc<Vec<RuleNode>>>>)> =
        RefCell::new((0, HashMap::default()));

    /// `(pkg, name) -> proven unable to re-enter itself at the same position`.
    static NO_LEFT_REENTRY: RefCell<(u64, HashMap<RuleNode, bool>)> =
        RefCell::new((0, HashMap::default()));
}

fn token_defs_gen() -> u64 {
    crate::runtime::regex_parse::TOKEN_DEFS_GEN.load(std::sync::atomic::Ordering::Relaxed)
}

impl Interpreter {
    /// `true` when no chain of rule calls made at their callers' start
    /// positions leads from `<name>` in `pkg` back to a rule named `name`, so a
    /// call of it can never re-enter its own left-recursion key through a rule
    /// call. `false` means only "not proven".
    // Cost: O(1) expected once memoized for the token generation; the first
    // call per (pkg, name) walks the left cone, O(r * t), r = rules reached,
    // t = tokens in their bodies.
    pub(super) fn subrule_cannot_left_reenter(&mut self, name: Symbol, pkg: Symbol) -> bool {
        let generation = token_defs_gen();
        let key: RuleNode = (pkg, name);
        if let Some(hit) = NO_LEFT_REENTRY.with(|c| {
            let c = c.borrow();
            (c.0 == generation)
                .then(|| c.1.get(&key).copied())
                .flatten()
        }) {
            return hit;
        }
        let verdict = self.left_cone_is_acyclic(key);
        NO_LEFT_REENTRY.with(|c| {
            let mut c = c.borrow_mut();
            if c.0 != generation {
                c.0 = generation;
                c.1.clear();
            }
            c.1.insert(key, verdict);
        });
        verdict
    }

    fn left_cone_is_acyclic(&mut self, start: RuleNode) -> bool {
        let mut seen: HashSet<RuleNode> = HashSet::from_iter([start]);
        let mut queue: VecDeque<RuleNode> = VecDeque::from([start]);
        while let Some(node) = queue.pop_front() {
            let Some(callees) = self.direct_left_calls(node) else {
                return false;
            };
            for callee in callees.iter() {
                if callee.1 == start.1 {
                    return false;
                }
                if seen.len() >= MAX_REACHABLE_RULES {
                    return false;
                }
                if seen.insert(*callee) {
                    queue.push_back(*callee);
                }
            }
        }
        true
    }

    /// The rules the candidates of `node` can call at their start position.
    fn direct_left_calls(&mut self, node: RuleNode) -> Option<std::sync::Arc<Vec<RuleNode>>> {
        let generation = token_defs_gen();
        if let Some(hit) = LEFT_CALLS.with(|c| {
            let c = c.borrow();
            (c.0 == generation)
                .then(|| c.1.get(&node).cloned())
                .flatten()
        }) {
            return hit;
        }
        let computed = self.compute_direct_left_calls(node);
        LEFT_CALLS.with(|c| {
            let mut c = c.borrow_mut();
            if c.0 != generation {
                c.0 = generation;
                c.1.clear();
            }
            c.1.insert(node, computed.clone());
        });
        computed
    }

    fn compute_direct_left_calls(
        &mut self,
        (pkg, name_sym): RuleNode,
    ) -> Option<std::sync::Arc<Vec<RuleNode>>> {
        let name = name_sym.as_str();
        // Resolved exactly as the full call-graph walk resolves a rule.
        let candidates = match self.resolve_parsed_token_candidates_in_pkg(name, name_sym, pkg) {
            Some(candidates) => candidates,
            None if self.rule_body_edges_are_generation_stable(name, pkg) => {
                let spec = Self::parse_named_regex_lookup_spec(name);
                self.parsed_subrule_candidates(&spec, pkg, &[]).0
            }
            None => return None,
        };
        if candidates.is_empty() {
            // A builtin assertion calls no user rule; a plain grammar METHOD
            // is arbitrary code.
            return match self.registry().user_method_overloads(pkg.as_str(), name) {
                None => Some(std::sync::Arc::new(Vec::new())),
                Some(_) => None,
            };
        }
        let mut out = Vec::new();
        for (parsed, sub_pkg, _) in candidates.iter() {
            left_calls_of_pattern(parsed, *sub_pkg, &mut out)?;
        }
        out.sort_by_key(|(pkg, name)| (pkg.id(), name.id()));
        out.dedup();
        Some(std::sync::Arc::new(out))
    }
}

/// Append the rules `pattern` can call before it has consumed anything.
/// Returns whether the whole pattern may match the empty string, or `None`
/// when a construct's target cannot be named.
fn left_calls_of_pattern(
    pattern: &RegexPattern,
    pkg: Symbol,
    out: &mut Vec<RuleNode>,
) -> Option<bool> {
    for token in &pattern.tokens {
        if !left_calls_of_token(token, pkg, out)? {
            return Some(false);
        }
    }
    Some(true)
}

/// [`left_calls_of_pattern`] for one quantified token: returns whether the
/// token may match the empty string.
fn left_calls_of_token(token: &RegexToken, pkg: Symbol, out: &mut Vec<RuleNode>) -> Option<bool> {
    let atom_nullable = left_calls_of_atom(&token.atom, pkg, out)?;
    // A separator is matched after one atom, so it is at the start position
    // only when the atom can match empty.
    if atom_nullable && let Some(sep) = &token.separator {
        left_calls_of_pattern(&sep.pattern, pkg, out)?;
    }
    let quant_nullable = match &token.quant {
        RegexQuant::One | RegexQuant::OneOrMore => false,
        RegexQuant::Repeat(min, _) => *min == 0,
        RegexQuant::ZeroOrMore | RegexQuant::ZeroOrOne | RegexQuant::RepeatCode(_) => true,
    };
    Some(atom_nullable || quant_nullable)
}

/// [`left_calls_of_pattern`] for one atom: returns whether the atom may match
/// the empty string.
fn left_calls_of_atom(atom: &RegexAtom, pkg: Symbol, out: &mut Vec<RuleNode>) -> Option<bool> {
    match atom {
        // Always consume one character (or grapheme) and call nothing.
        RegexAtom::Literal(_)
        | RegexAtom::LiteralGrapheme(_)
        | RegexAtom::Any
        | RegexAtom::CharClass(_)
        | RegexAtom::NotNewline => Some(false),
        // Consume, but a predicate argument is user code — harmless here, as
        // for `pattern_is_rule_call_free`.
        RegexAtom::UnicodeProp { .. } => Some(false),
        // Zero-width, or of a width this walk does not model: treated as
        // possibly empty so the walk goes on past them.
        RegexAtom::Newline
        | RegexAtom::ZeroWidth
        | RegexAtom::UnicodePropAssert { .. }
        | RegexAtom::CaptureStartMarker
        | RegexAtom::CaptureEndMarker
        | RegexAtom::CompositeClass { .. }
        | RegexAtom::LeftWordBoundary
        | RegexAtom::RightWordBoundary
        | RegexAtom::WordBoundary { .. }
        | RegexAtom::WithinWord { .. }
        | RegexAtom::StartOfLine
        | RegexAtom::EndOfLine
        | RegexAtom::EndOfString
        | RegexAtom::Backref(_)
        | RegexAtom::NamedBackref(_)
        | RegexAtom::VarInterp(_)
        | RegexAtom::SameAssertion { .. }
        | RegexAtom::AtPosition(_)
        | RegexAtom::TildeMarker
        | RegexAtom::CodeAssertion { .. }
        | RegexAtom::VarDecl { .. } => Some(true),
        RegexAtom::Group(p)
        | RegexAtom::CaptureGroup(p)
        | RegexAtom::CaptureIsolatedGroup(p)
        | RegexAtom::CaptureIsolatedGroupScoped(p, _) => left_calls_of_pattern(p, pkg, out),
        RegexAtom::Alternation(alts)
        | RegexAtom::SequentialAlternation(alts)
        | RegexAtom::Conjunction(alts) => {
            let mut nullable = false;
            for alt in alts {
                nullable |= left_calls_of_pattern(alt, pkg, out)?;
            }
            Some(nullable)
        }
        RegexAtom::Lookaround {
            pattern, is_behind, ..
        } => {
            if *is_behind {
                // A look-behind matches its pattern at EARLIER positions, where
                // an enclosing activation of any rule may be live: every call
                // in it counts.
                all_calls_of_pattern(pattern, pkg, out)?;
            } else {
                left_calls_of_pattern(pattern, pkg, out)?;
            }
            Some(true)
        }
        RegexAtom::GoalMatch { goal, inner, .. } => {
            let inner_nullable = left_calls_of_pattern(inner, pkg, out)?;
            if inner_nullable {
                left_calls_of_pattern(goal, pkg, out)
            } else {
                Some(false)
            }
        }
        RegexAtom::WsRule => {
            // `<.ws>` dispatches to whatever `ws` the grammar resolves to.
            out.push((pkg, Symbol::intern("ws")));
            Some(true)
        }
        RegexAtom::Named(name) => {
            let spec = name.spec();
            if spec.lookup_name == "::" || !spec.arg_exprs.is_empty() {
                return None;
            }
            if spec.lookup_name.is_empty()
                || !spec
                    .lookup_name
                    .chars()
                    .all(|c| c.is_alphanumeric() || matches!(c, '_' | '-' | ':'))
            {
                return None;
            }
            out.push((pkg, spec.lookup_sym));
            Some(true)
        }
        RegexAtom::ClosureInterpolation { .. } | RegexAtom::RecurseSelf(_) => None,
    }
}

/// Every rule `pattern` can call, wherever it calls it.
fn all_calls_of_pattern(
    pattern: &RegexPattern,
    pkg: Symbol,
    out: &mut Vec<RuleNode>,
) -> Option<()> {
    for token in &pattern.tokens {
        left_calls_of_atom_everywhere(&token.atom, pkg, out)?;
        if let Some(sep) = &token.separator {
            all_calls_of_pattern(&sep.pattern, pkg, out)?;
        }
    }
    Some(())
}

/// Every call inside `atom`: its left calls, plus the calls of every token
/// after the first in each sub-pattern.
fn left_calls_of_atom_everywhere(
    atom: &RegexAtom,
    pkg: Symbol,
    out: &mut Vec<RuleNode>,
) -> Option<()> {
    match atom {
        RegexAtom::Group(p)
        | RegexAtom::CaptureGroup(p)
        | RegexAtom::CaptureIsolatedGroup(p)
        | RegexAtom::CaptureIsolatedGroupScoped(p, _)
        | RegexAtom::Lookaround { pattern: p, .. } => all_calls_of_pattern(p, pkg, out),
        RegexAtom::Alternation(alts)
        | RegexAtom::SequentialAlternation(alts)
        | RegexAtom::Conjunction(alts) => {
            for alt in alts {
                all_calls_of_pattern(alt, pkg, out)?;
            }
            Some(())
        }
        RegexAtom::GoalMatch { goal, inner, .. } => {
            all_calls_of_pattern(inner, pkg, out)?;
            all_calls_of_pattern(goal, pkg, out)
        }
        other => left_calls_of_atom(other, pkg, out).map(|_| ()),
    }
}
