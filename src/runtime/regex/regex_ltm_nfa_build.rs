//! Building the declarative-prefix NFA of ADR-0125 from a parsed pattern.
//!
//! The builder works backwards, in continuation-passing style: every
//! `build_*` function is handed the node its sub-graph must continue to and
//! returns the sub-graph's entry node. A quantified atom is compiled once per
//! unrolled copy, so no node is ever shared between two places in the
//! pattern — except a rule's body, which is compiled once per NFA as a
//! procedure ending in [`NfaNode::Return`] and entered by a
//! [`NfaNode::Call`] from every place that calls it.
//!
//! Every atom first goes through `ltm_atom_mode`, the classifier Rakudo's NFA
//! construction is mirrored in, so what is a fate is decided in one place.
//! What is left is either structure (groups, `|`, `||`, quantifiers, subrule
//! calls) or a leaf that the existing single-atom matchers answer at
//! simulation time.
//!
//! The builder never declines: a construct it cannot follow statically is a
//! fate (as in Rakudo's NFA), a call resolved at run time
//! ([`NfaNode::DynCall`]), or a region measured by an NFA of its own
//! ([`NfaNode::Sub`]).

use super::super::*;
use super::regex_helpers::{bounded_declarative_max, named_lookup_is_ws};
use super::regex_ltm_nfa::{LeafKind, LtmNfa, NfaNode, SubKind};
use super::regex_ltm_rank::{LtmAtomMode, ltm_atom_mode};
use super::regex_token_resolve::ParsedTokenCandidate;
use rustc_hash::FxHashMap as HashMap;
use std::sync::Arc;

/// The most copies of an atom a counted quantifier (`** 2..5`, `+ % ','`) is
/// unrolled into. Past it, a minimum ends in a fate after this many copies
/// and a maximum is treated as unbounded.
const MAX_UNROLL: usize = 32;

/// Past this many nodes a counted quantifier is no longer unrolled at all:
/// `** m..n` compiles like `+` (or `*` when `m` is 0). Nested counted
/// quantifiers multiply their copies, and a measurement only needs the
/// furthest end, which the looser loop can only lengthen.
const UNROLL_BUDGET: usize = 1 << 16;

/// How deeply [`NfaNode::Sub`] regions may nest while building. A rule whose
/// `:m` body calls itself would otherwise build its stripped copy forever;
/// past the bound the region is a fate.
const MAX_SUB_DEPTH: usize = 8;

/// What one iteration of a counted quantifier consists of.
#[derive(Clone, Copy)]
enum Unit {
    /// The atom alone: every iteration of an unseparated quantifier, and the
    /// first iteration of a separated one.
    Atom,
    /// Separator, then atom: a separated quantifier's later iterations.
    SepAtom,
}

pub(super) struct NfaBuilder<'a> {
    interp: &'a mut Interpreter,
    nodes: Vec<NfaNode>,
    /// `(rule name, package it is looked up in, inherited :i)` -> the entry
    /// of that rule's procedure.
    procs: HashMap<(Symbol, Symbol, bool), u32>,
    /// The single [`NfaNode::Return`] every procedure ends in.
    ret: u32,
    /// How many [`NfaNode::Sub`] builds enclose this one.
    sub_depth: usize,
}

impl<'a> NfaBuilder<'a> {
    // Cost: O(1).
    pub(super) fn new(interp: &'a mut Interpreter, sub_depth: usize) -> Self {
        NfaBuilder {
            interp,
            nodes: vec![NfaNode::Return],
            procs: HashMap::default(),
            ret: 0,
            sub_depth,
        }
    }

    /// Compile `pattern`, walked in `pkg`, with `inherited_ic` from a `:i`
    /// caller (applied at the pattern's top level only, as
    /// `subrule_candidate_ends` does).
    // Cost: O(s), s = nodes of the result, plus one candidate resolution per
    // distinct rule it can call.
    pub(super) fn build(
        mut self,
        pattern: &RegexPattern,
        pkg: Symbol,
        inherited_ic: bool,
    ) -> LtmNfa {
        let accept = self.push(NfaNode::Accept);
        let start = self.build_pattern(pattern, pkg, inherited_ic, accept);
        LtmNfa {
            nodes: self.nodes,
            start,
        }
    }

    fn push(&mut self, node: NfaNode) -> u32 {
        self.nodes.push(node);
        (self.nodes.len() - 1) as u32
    }

    fn fate(&mut self) -> u32 {
        self.push(NfaNode::Fate)
    }

    /// A pattern walked at one level: its own `:i` flag, or one inherited from
    /// a `:i` subrule call.
    fn build_pattern(
        &mut self,
        pattern: &RegexPattern,
        pkg: Symbol,
        inherited_ic: bool,
        next: u32,
    ) -> u32 {
        if pattern.ignore_mark {
            let stripped = super::regex_helpers::strip_marks_pattern(pattern);
            return self.build_sub(&stripped, pkg, inherited_ic, SubKind::StripMarks, next);
        }
        let ic = pattern.ignore_case || inherited_ic;
        let mut cont = next;
        if pattern.anchor_end {
            cont = self.push(NfaNode::AtEnd(cont));
        }
        for token in pattern.tokens.iter().rev() {
            cont = self.build_token(token, pkg, ic, cont);
        }
        if pattern.anchor_start {
            cont = self.push(NfaNode::AtStart(cont));
        }
        cont
    }

    /// A region measured by an NFA of its own (see [`SubKind`]).
    fn build_sub(
        &mut self,
        pattern: &RegexPattern,
        pkg: Symbol,
        ic: bool,
        kind: SubKind,
        next: u32,
    ) -> u32 {
        if self.sub_depth >= MAX_SUB_DEPTH {
            return self.fate();
        }
        let nfa = NfaBuilder::new(self.interp, self.sub_depth + 1).build(pattern, pkg, ic);
        self.push(NfaNode::Sub {
            nfa: Arc::new(nfa),
            kind,
            next,
        })
    }

    fn build_token(&mut self, token: &RegexToken, pkg: Symbol, ic: bool, next: u32) -> u32 {
        // A literal from a runtime interpolation and a `** {code}` count are
        // fates (ADR-0022 Slice 5; `'a' ** {3} % ','` measures 0 in `raku`).
        if token.from_runtime_interpolation || matches!(token.quant, RegexQuant::RepeatCode(_)) {
            return self.fate();
        }
        let (min, max) = match token.quant {
            RegexQuant::One if token.separator.is_none() => {
                return self.build_atom(&token.atom, pkg, ic, next);
            }
            RegexQuant::One => (1, Some(1)),
            RegexQuant::ZeroOrOne => (0, Some(1)),
            RegexQuant::ZeroOrMore => (0, None),
            RegexQuant::OneOrMore => (1, None),
            RegexQuant::Repeat(min, max) => (min, max.map(|max| bounded_declarative_max(min, max))),
            RegexQuant::RepeatCode(_) => unreachable!("handled above"),
        };
        // `** 2..1` throws when matched for real; it has no prefix.
        if max.is_some_and(|max| min > max) {
            return self.fate();
        }
        self.build_counted(token, pkg, ic, min, max, next)
    }

    /// `min..max` iterations of `token`'s atom, with its separator between
    /// iterations and, for `%%`, optionally after the last one.
    fn build_counted(
        &mut self,
        token: &RegexToken,
        pkg: Symbol,
        ic: bool,
        min: usize,
        max: Option<usize>,
        next: u32,
    ) -> u32 {
        let (min, max, cut_short) =
            if self.nodes.len() > UNROLL_BUDGET && (min > 1 || max.is_some_and(|m| m > 1)) {
                (min.min(1), None, false)
            } else {
                (
                    min.min(MAX_UNROLL),
                    max.filter(|&max| max <= MAX_UNROLL),
                    min > MAX_UNROLL,
                )
            };
        let later = if token.separator.is_some() {
            Unit::SepAtom
        } else {
            Unit::Atom
        };
        // Where a path goes once it has done at least one iteration and stops.
        let done = match token.separator.as_ref() {
            Some(sep) if sep.allow_trailing => {
                let trailing = self.build_pattern(&sep.pattern, pkg, false, next);
                self.push(NfaNode::Split(vec![trailing, next]))
            }
            _ => next,
        };
        // A minimum past `MAX_UNROLL`: the prefix ends after the copies built.
        if cut_short {
            let fate = self.fate();
            let mut after = fate;
            for i in (0..min).rev() {
                let unit = if i == 0 { Unit::Atom } else { later };
                after = self.build_unit(unit, token, pkg, ic, after);
            }
            return after;
        }
        let exit = |iterations: usize| if iterations == 0 { next } else { done };
        match max {
            Some(max) => {
                // `after[i]`: the node reached after `i` iterations.
                let mut after = exit(max);
                for i in (0..max).rev() {
                    let unit = if i == 0 { Unit::Atom } else { later };
                    let more = self.build_unit(unit, token, pkg, ic, after);
                    after = if i >= min {
                        self.push(NfaNode::Split(vec![more, exit(i)]))
                    } else {
                        more
                    };
                }
                after
            }
            None => {
                // The loop after at least one iteration: another one, or stop.
                let lp = self.push(NfaNode::Split(Vec::new()));
                let again = self.build_unit(later, token, pkg, ic, lp);
                self.nodes[lp as usize] = NfaNode::Split(vec![again, done]);
                if min == 0 {
                    let first = self.build_unit(Unit::Atom, token, pkg, ic, lp);
                    return self.push(NfaNode::Split(vec![first, next]));
                }
                let mut after = lp;
                for i in (0..min).rev() {
                    let unit = if i == 0 { Unit::Atom } else { later };
                    after = self.build_unit(unit, token, pkg, ic, after);
                }
                after
            }
        }
    }

    fn build_unit(
        &mut self,
        unit: Unit,
        token: &RegexToken,
        pkg: Symbol,
        ic: bool,
        next: u32,
    ) -> u32 {
        let atom = self.build_atom(&token.atom, pkg, ic, next);
        match (unit, token.separator.as_ref()) {
            (Unit::SepAtom, Some(sep)) => self.build_pattern(&sep.pattern, pkg, false, atom),
            _ => atom,
        }
    }

    fn build_atom(&mut self, atom: &RegexAtom, pkg: Symbol, ic: bool, next: u32) -> u32 {
        match ltm_atom_mode(atom) {
            // A rule's leading `<.ws>` is transparent at the start of the
            // subject (`ltm_leading_ws_is_transparent`); that is decided per
            // position.
            LtmAtomMode::Terminate if is_ws_atom(atom) => {
                return self.push(NfaNode::WsLead {
                    atom: Box::new(atom.clone()),
                    pkg,
                    ic,
                    next,
                });
            }
            LtmAtomMode::Terminate => return self.fate(),
            // `<?before X>` measures `X`, then ends the path.
            LtmAtomMode::TerminateAfter(inner) => {
                let fate = self.fate();
                return self.build_pattern(inner, pkg, false, fate);
            }
            LtmAtomMode::SkipZeroWidth => return next,
            LtmAtomMode::Normal => {}
        }
        match atom {
            RegexAtom::Group(pattern)
            | RegexAtom::CaptureGroup(pattern)
            | RegexAtom::CaptureIsolatedGroup(pattern) => {
                self.build_pattern(pattern, pkg, false, next)
            }
            RegexAtom::CaptureIsolatedGroupScoped(pattern, scope) => self.build_sub(
                pattern,
                pkg,
                false,
                SubKind::Scoped(Arc::new(scope.as_ref().clone())),
                next,
            ),
            RegexAtom::Alternation(alternatives) => {
                let branches = alternatives
                    .iter()
                    .map(|alt| self.build_pattern(alt, pkg, false, next))
                    .collect();
                self.push(NfaNode::Split(branches))
            }
            // ADR-0022 §4.2: the first branch, plus an ε bypass.
            RegexAtom::SequentialAlternation(alternatives) => match alternatives.first() {
                Some(first) => {
                    let first = self.build_pattern(first, pkg, false, next);
                    self.push(NfaNode::SeqAlt(vec![first, next]))
                }
                None => next,
            },
            // `<?{ }>` is a zero-width pass; a plain block is a fate (ADR-0009).
            RegexAtom::CodeAssertion { is_assertion, .. } => {
                if *is_assertion {
                    next
                } else {
                    self.fate()
                }
            }
            RegexAtom::CaptureStartMarker | RegexAtom::CaptureEndMarker => next,
            RegexAtom::GoalMatch { goal, inner, .. } => {
                let goal = self.build_pattern(goal, pkg, false, next);
                self.build_pattern(inner, pkg, false, goal)
            }
            RegexAtom::Named(name) => self.build_subrule(atom, name, pkg, ic, next),
            // Everything else consumes one grapheme-sized unit or is a
            // zero-width test: one end at most.
            _ => {
                let kind = match atom {
                    RegexAtom::Literal(_)
                    | RegexAtom::LiteralGrapheme(_)
                    | RegexAtom::Any
                    | RegexAtom::CharClass(_)
                    | RegexAtom::Newline
                    | RegexAtom::NotNewline
                    | RegexAtom::UnicodeProp { .. }
                    | RegexAtom::CompositeClass { .. } => LeafKind::Consume,
                    _ => LeafKind::Probe,
                };
                self.push(NfaNode::Leaf {
                    atom: Box::new(atom.clone()),
                    pkg,
                    ic,
                    kind,
                    next,
                })
            }
        }
    }

    /// A `<name>` call: a call into the rule's procedure (a proto's every
    /// candidate), a call resolved at run time, a builtin left to the
    /// matcher, or a fate (ADR-0125 §3).
    fn build_subrule(
        &mut self,
        atom: &RegexAtom,
        name: &NamedAtom,
        pkg: Symbol,
        ic: bool,
        next: u32,
    ) -> u32 {
        let spec = name.spec().clone();
        // `<::(…)>`: the name is computed by user code. Rakudo's NFA has no
        // name to inline there and puts a fate.
        if spec.lookup_name == "::" {
            return self.fate();
        }
        // `<&re>` and `<$re>` call a code object at run time, not a rule of the
        // grammar by name: Rakudo's NFA puts a fate there (verified against
        // `raku`: `/ [ 'ab' | <&three> ] /` with `my regex three { 'abc' }`
        // matches "ab" of "abcd").
        if Interpreter::may_name_lexical_regex(&spec) {
            return self.fate();
        }
        // A body whose parse depends on runtime values is re-parsed per call,
        // so it is resolved when the measurement reaches it.
        if !self
            .interp
            .rule_body_edges_are_generation_stable(&spec.lookup_name, pkg)
        {
            return self.push(NfaNode::DynCall {
                atom: Box::new(atom.clone()),
                pkg,
                ic,
                next,
            });
        }
        let Some(candidates) = self.interp.resolve_parsed_token_candidates_in_pkg(
            &spec.lookup_name,
            spec.lookup_sym,
            pkg,
        ) else {
            return self.push(NfaNode::DynCall {
                atom: Box::new(atom.clone()),
                pkg,
                ic,
                next,
            });
        };
        if candidates.is_empty() {
            // A plain method of the grammar is user code with no NFA: a fate,
            // as in Rakudo. Anything else is a builtin assertion (`<ident>`,
            // `<alpha>`, ...) the plural matcher answers at simulation time.
            if self
                .interp
                .registry()
                .user_method_overloads(pkg.as_str(), &spec.lookup_name)
                .is_some()
            {
                return self.fate();
            }
            return self.push(NfaNode::Leaf {
                atom: Box::new(atom.clone()),
                pkg,
                ic,
                kind: LeafKind::Plural,
                next,
            });
        }
        // Arguments are ignored, as in Rakudo's NFA: a parameter the body
        // interpolates is a runtime value there, which is a fate.
        let body = self.procedure(spec.lookup_sym, pkg, ic, &candidates);
        self.push(NfaNode::Call {
            name: spec.lookup_sym,
            body,
            ret: next,
        })
    }

    /// The entry of the procedure for rule `name` looked up in `pkg`, with
    /// its body compiled on first use. A proto is the union of its
    /// candidates, as in Rakudo's NFA (#9643); a candidate's `<sym>` was
    /// already rewritten to its own sym text when the body was resolved
    /// (`replace_sym_assertions`).
    fn procedure(
        &mut self,
        name: Symbol,
        pkg: Symbol,
        ic: bool,
        candidates: &[ParsedTokenCandidate],
    ) -> u32 {
        if let Some(&entry) = self.procs.get(&(name, pkg, ic)) {
            return entry;
        }
        // Registered before the body is built: a recursive call inside it
        // calls this same entry.
        let entry = self.push(NfaNode::Split(Vec::new()));
        self.procs.insert((name, pkg, ic), entry);
        let ret = self.ret;
        let bodies = candidates
            .iter()
            .map(|(parsed, sub_pkg, _)| self.build_pattern(parsed, *sub_pkg, ic, ret))
            .collect();
        self.nodes[entry as usize] = NfaNode::Split(bodies);
        entry
    }
}

/// `<.ws>` in any of its spellings.
fn is_ws_atom(atom: &RegexAtom) -> bool {
    match atom {
        RegexAtom::WsRule => true,
        RegexAtom::Named(name) => named_lookup_is_ws(name),
        _ => false,
    }
}
