//! Building the declarative-prefix NFA of ADR-0125 from a parsed pattern.
//!
//! The builder works backwards, in continuation-passing style: every
//! `build_*` function is handed the node its sub-graph must continue to and
//! returns the sub-graph's entry node. A quantified atom is compiled once per
//! unrolled copy, so no node is ever shared between two places in the
//! pattern.
//!
//! Every atom first goes through `ltm_atom_mode`, the classifier the walker
//! uses, so the two agree on what is a fate. What is left is either structure
//! (groups, `|`, `||`, quantifiers, inlined subrules) or a leaf that the
//! existing single-atom matchers answer at simulation time.

use super::super::*;
use super::regex_helpers::named_lookup_is_ws;
use super::regex_ltm_nfa::{LtmNfa, NfaNode};
use super::regex_ltm_rank::{LtmAtomMode, ltm_atom_mode};

/// More nodes than this and the NFA is declined: inlining copies every
/// subrule body at every call site, which a large grammar can blow up.
const MAX_NODES: usize = 8192;

/// The most copies of an atom a counted quantifier (`** 2..5`, `+ % ','`) is
/// unrolled into.
const MAX_UNROLL: usize = 32;

/// Why the builder gave up; the caller falls back to the walker.
pub(super) struct Declined;

type Built = Result<u32, Declined>;

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
    /// The rules being inlined on the current path, for the recursion cut.
    stack: Vec<Symbol>,
}

impl<'a> NfaBuilder<'a> {
    // Cost: O(1).
    pub(super) fn new(interp: &'a mut Interpreter) -> Self {
        NfaBuilder {
            interp,
            nodes: Vec::new(),
            stack: Vec::new(),
        }
    }

    /// Compile `pattern`, walked in `pkg` from a real match (no inherited
    /// `:i`, nothing on the subrule stack).
    // Cost: O(s), s = nodes of the result (bounded by `MAX_NODES`), plus one
    // candidate resolution per inlined subrule call.
    pub(super) fn build(mut self, pattern: &RegexPattern, pkg: Symbol) -> Option<LtmNfa> {
        let accept = self.push(NfaNode::Accept).ok()?;
        let start = self.build_pattern(pattern, pkg, false, accept).ok()?;
        Some(LtmNfa {
            nodes: self.nodes,
            start,
        })
    }

    fn push(&mut self, node: NfaNode) -> Built {
        if self.nodes.len() >= MAX_NODES {
            return Err(Declined);
        }
        self.nodes.push(node);
        Ok((self.nodes.len() - 1) as u32)
    }

    fn fate(&mut self) -> Built {
        self.push(NfaNode::Fate)
    }

    /// A pattern walked at one level: its own `:i` flag, or one inherited from
    /// a `:i` subrule call (applied at the body's top level only, as
    /// `subrule_candidate_ends` does).
    fn build_pattern(
        &mut self,
        pattern: &RegexPattern,
        pkg: Symbol,
        inherited_ic: bool,
        next: u32,
    ) -> Built {
        if pattern.ignore_mark {
            return Err(Declined);
        }
        let ic = pattern.ignore_case || inherited_ic;
        let mut cont = next;
        if pattern.anchor_end {
            cont = self.push(NfaNode::AtEnd(cont))?;
        }
        for token in pattern.tokens.iter().rev() {
            cont = self.build_token(token, pkg, ic, cont)?;
        }
        if pattern.anchor_start {
            cont = self.push(NfaNode::AtStart(cont))?;
        }
        Ok(cont)
    }

    fn build_token(&mut self, token: &RegexToken, pkg: Symbol, ic: bool, next: u32) -> Built {
        // A literal from a runtime interpolation and a `** {code}` count are
        // fates (`walk_tokens`).
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
            RegexQuant::Repeat(min, max) => (min, max),
            RegexQuant::RepeatCode(_) => unreachable!("handled above"),
        };
        if max.is_some_and(|max| min > max) {
            return Err(Declined);
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
    ) -> Built {
        if min > MAX_UNROLL || max.is_some_and(|max| max > MAX_UNROLL) {
            return Err(Declined);
        }
        let later = if token.separator.is_some() {
            Unit::SepAtom
        } else {
            Unit::Atom
        };
        // Where a path goes once it has done at least one iteration and stops.
        let done = match token.separator.as_ref() {
            Some(sep) if sep.allow_trailing => {
                let trailing = self.build_pattern(&sep.pattern, pkg, false, next)?;
                self.push(NfaNode::Split(vec![trailing, next]))?
            }
            _ => next,
        };
        let exit = |iterations: usize| if iterations == 0 { next } else { done };
        match max {
            Some(max) => {
                // `after[i]`: the node reached after `i` iterations.
                let mut after = exit(max);
                for i in (0..max).rev() {
                    let unit = if i == 0 { Unit::Atom } else { later };
                    let more = self.build_unit(unit, token, pkg, ic, after)?;
                    after = if i >= min {
                        self.push(NfaNode::Split(vec![more, exit(i)]))?
                    } else {
                        more
                    };
                }
                Ok(after)
            }
            None => {
                // The loop after at least one iteration: another one, or stop.
                let lp = self.push(NfaNode::Split(Vec::new()))?;
                let again = self.build_unit(later, token, pkg, ic, lp)?;
                self.nodes[lp as usize] = NfaNode::Split(vec![again, done]);
                if min == 0 {
                    let first = self.build_unit(Unit::Atom, token, pkg, ic, lp)?;
                    return self.push(NfaNode::Split(vec![first, next]));
                }
                let mut after = lp;
                for i in (0..min).rev() {
                    let unit = if i == 0 { Unit::Atom } else { later };
                    after = self.build_unit(unit, token, pkg, ic, after)?;
                }
                Ok(after)
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
    ) -> Built {
        let atom = self.build_atom(&token.atom, pkg, ic, next)?;
        match (unit, token.separator.as_ref()) {
            (Unit::SepAtom, Some(sep)) => self.build_pattern(&sep.pattern, pkg, false, atom),
            _ => Ok(atom),
        }
    }

    fn build_atom(&mut self, atom: &RegexAtom, pkg: Symbol, ic: bool, next: u32) -> Built {
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
                let fate = self.fate()?;
                return self.build_pattern(inner, pkg, false, fate);
            }
            LtmAtomMode::SkipZeroWidth => return Ok(next),
            LtmAtomMode::Normal => {}
        }
        match atom {
            RegexAtom::Group(pattern)
            | RegexAtom::CaptureGroup(pattern)
            | RegexAtom::CaptureIsolatedGroup(pattern) => {
                self.build_pattern(pattern, pkg, false, next)
            }
            RegexAtom::Alternation(alternatives) => {
                let mut branches = Vec::with_capacity(alternatives.len());
                for alt in alternatives {
                    branches.push(self.build_pattern(alt, pkg, false, next)?);
                }
                self.push(NfaNode::Split(branches))
            }
            // ADR-0022 §4.2: the first branch, plus an ε bypass.
            RegexAtom::SequentialAlternation(alternatives) => match alternatives.first() {
                Some(first) => {
                    let first = self.build_pattern(first, pkg, false, next)?;
                    self.push(NfaNode::Split(vec![first, next]))
                }
                None => Ok(next),
            },
            // `<?{ }>` is a zero-width pass; a plain block is a fate (ADR-0009).
            RegexAtom::CodeAssertion { is_assertion, .. } => {
                if *is_assertion {
                    Ok(next)
                } else {
                    self.fate()
                }
            }
            RegexAtom::CaptureStartMarker | RegexAtom::CaptureEndMarker => Ok(next),
            RegexAtom::GoalMatch { goal, inner, .. } => {
                let goal = self.build_pattern(goal, pkg, false, next)?;
                self.build_pattern(inner, pkg, false, goal)
            }
            RegexAtom::Named(name) => self.build_subrule(atom, name, pkg, ic, next),
            // Its scope has to be installed around the match.
            RegexAtom::CaptureIsolatedGroupScoped(..) => Err(Declined),
            // Everything else consumes one grapheme-sized unit or is a
            // zero-width test: one end at most.
            _ => self.push(NfaNode::Leaf {
                atom: Box::new(atom.clone()),
                pkg,
                ic,
                plural: false,
                next,
            }),
        }
    }

    /// A `<name>` call: inline the rule's body, cut a recursive call into a
    /// fate, leave a builtin to the matcher, or decline (ADR-0125 §3).
    fn build_subrule(
        &mut self,
        atom: &RegexAtom,
        name: &NamedAtom,
        pkg: Symbol,
        ic: bool,
        next: u32,
    ) -> Built {
        let spec = name.spec().clone();
        if self.stack.contains(&spec.lookup_sym) {
            return self.fate();
        }
        if !spec.arg_exprs.is_empty()
            || spec.lookup_name == "::"
            || Interpreter::may_name_lexical_regex(&spec)
            || !self
                .interp
                .rule_body_edges_are_generation_stable(&spec.lookup_name, pkg)
        {
            return Err(Declined);
        }
        let candidates = self
            .interp
            .resolve_parsed_token_candidates_in_pkg(&spec.lookup_name, spec.lookup_sym, pkg)
            .ok_or(Declined)?;
        // A method answering to the name is user code (and wins over a rule
        // of the same name in some dispatch paths): not ours to model.
        if self
            .interp
            .registry()
            .user_method_overloads(pkg.as_str(), &spec.lookup_name)
            .is_some()
        {
            return Err(Declined);
        }
        if candidates.is_empty() {
            // A builtin assertion (`<ident>`, `<alpha>`, ...): the plural
            // matcher answers it at simulation time.
            return self.push(NfaNode::Leaf {
                atom: Box::new(atom.clone()),
                pkg,
                ic,
                plural: true,
                next,
            });
        }
        // A proto's candidates are ranked and only the winner's first end is
        // kept by the walker (ADR-0125 §3).
        if candidates.iter().any(|(_, _, sym)| sym.is_some()) {
            return Err(Declined);
        }
        self.stack.push(spec.lookup_sym);
        let mut bodies = Vec::with_capacity(candidates.len());
        let mut result = Ok(());
        for (parsed, sub_pkg, _) in candidates.iter() {
            match self.build_pattern(parsed, *sub_pkg, ic, next) {
                Ok(body) => bodies.push(body),
                Err(declined) => {
                    result = Err(declined);
                    break;
                }
            }
        }
        self.stack.pop();
        result?;
        let body = if bodies.len() == 1 {
            bodies[0]
        } else {
            self.push(NfaNode::Split(bodies))?
        };
        self.push(NfaNode::Enter {
            name: spec.lookup_sym,
            next: body,
        })
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
