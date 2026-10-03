//! The compiled engine's capture-writing ops. Each one applies the walk's own
//! capture transform (ADR-0135 D4) to the innermost capture level, through
//! `Levels::edit` so a backtrack undoes it.

use super::super::regex_helpers::{
    count_capture_groups, merge_goal_captures, merge_regex_captures,
};
use super::super::regex_match_delta::{
    alternation_tail_delta, capture_group_delta, group_merge_delta,
};
use super::super::regex_match_sep::{separated_capture_delta_syms, separator_stride};
use super::rx_levels::Levels;
use super::{RxOp, RxProgram};
use crate::runtime::Interpreter;
use crate::runtime::regex_types::{RegexAtom, RegexCaptures};
use crate::symbol::Symbol;

impl Interpreter {
    /// Run one capture op at `pos`: the new position, or `None` on failure.
    /// Only `CapAtom` can fail or move the cursor.
    #[allow(clippy::too_many_arguments)]
    pub(super) fn rx_capture_op(
        &mut self,
        program: &RxProgram,
        op: RxOp,
        regs: &[usize],
        levels: &mut Levels,
        chars: &[char],
        pos: usize,
        pkg: Symbol,
    ) -> Option<usize> {
        match op {
            // Cost: O(1).
            RxOp::OpenCapture => levels.open(pos, true),
            // Cost: O(c), c = the captures the enclosing level sees (one
            // flattened copy).
            RxOp::OpenInline => levels.open_inline(None, None),
            // Cost: O(n + c), n = the names under the token, c = the captures
            // the enclosing level sees plus those of the iterations collected
            // so far (one fold, one flattened copy), as the walk pays per atom.
            RxOp::OpenSepIter {
                tok,
                base,
                sep,
                names,
            } => {
                let token = &program.toks[tok as usize];
                let entries = levels.collected_since(regs[base as usize]).to_vec();
                let folded =
                    Self::rx_sep_fold(token, &program.name_sets[names as usize], entries, false);
                let atom_stride = count_capture_groups(token);
                let fold = if sep {
                    let sep = &token.separator.as_ref().expect("a separated token").pattern;
                    (atom_stride, separator_stride(sep))
                } else {
                    (0, atom_stride)
                };
                levels.open_inline(Some(folded), Some(fold));
            }
            // Cost: O(c), c = the captures the enclosing level sees (one fold,
            // one flattened copy), as the walk pays per iteration.
            RxOp::OpenPlainIter { tok, pos_base } => {
                let stride = count_capture_groups(&program.toks[tok as usize]);
                levels.open_plain_iter(regs[pos_base as usize], stride);
            }
            // Cost: O(c), c = the iteration's captures (one snapshot).
            RxOp::ClosePlainIter => {
                let inner = levels.close();
                levels.edit(|s| s.merge_delta(group_merge_delta(inner)));
            }
            // Cost: O(1).
            RxOp::OpenIsolated => levels.open(pos, false),
            // Cost: O(1).
            RxOp::DropCapture => levels.discard(),
            // Cost: O(c) amortized, c = the closed level's captures (one
            // snapshot); O(1) for a group whose body captures nothing.
            RxOp::CloseCapture { start, nested } => {
                let from = regs[start as usize];
                if nested {
                    let inner = levels.close();
                    levels.edit(|s| s.merge_delta(capture_group_delta(from, pos, inner)));
                } else {
                    // A body that captured nothing is a leaf sub-Match: the slot
                    // `capture_group_delta` builds over an empty level, pushed
                    // without the delta around it.
                    let leaf = crate::runtime::CapNode {
                        from,
                        to: pos,
                        ..Default::default()
                    };
                    levels.edit(|s| {
                        s.push_positional(crate::runtime::PosSlot {
                            from,
                            to: pos,
                            subcap: Some(std::sync::Arc::new(leaf)),
                            ..Default::default()
                        })
                    });
                }
            }
            // Cost: O(n), n = the characters a backreference compares; O(1)
            // for a marker; one run of the body for a lookahead, and one per
            // candidate start (at most the body's longest match back) for a
            // lookbehind.
            RxOp::CapAtom(i) => {
                walk_leaf_use(&program.atoms[i as usize]);
                let (next, delta) = self.regex_match_atom_with_capture_in_pkg(
                    &program.atoms[i as usize],
                    chars,
                    pos,
                    levels.top().caps(),
                    pkg,
                    program.atom_ic[i as usize],
                )?;
                levels.edit(|s| s.merge_delta(delta));
                return Some(next);
            }
            // Cost: O(1) amortized to merge the delta, plus `regex_code_atom`'s
            // own cost (one run of the user's code).
            RxOp::Code(i) => {
                let (next, delta) = self.regex_code_atom(
                    &program.atoms[i as usize],
                    chars,
                    pos,
                    levels.top().caps(),
                    pkg,
                )?;
                levels.edit(|s| s.merge_delta(delta));
                return Some(next);
            }
            // Cost: O(v) amortized to merge the delta, v = the lexicals the
            // declaration introduces, plus `regex_var_decl_atom`'s own cost
            // (one run of each initializer).
            RxOp::VarDecl(i) => {
                let RegexAtom::VarDecl { code } = &program.atoms[i as usize] else {
                    debug_assert!(false, "a VarDecl op names a declaration atom");
                    return None;
                };
                let (next, delta) =
                    self.regex_var_decl_atom(code, chars, pos, levels.top().caps())?;
                levels.edit(|s| s.merge_delta(delta));
                return Some(next);
            }
            // Cost: O(1) amortized for the aliases the compiler accepts.
            RxOp::Named {
                tok,
                start,
                pos_base,
            } => levels.edit(|s| {
                Self::store_apply_named_capture(
                    s,
                    &program.toks[tok as usize],
                    regs[start as usize],
                    pos,
                    regs[pos_base as usize],
                )
            }),
            // Cost: O(a + n·m), a = the atom's capture groups, n = the names
            // under it (the walk's zero arm, same order), m = the names the
            // level has filed.
            RxOp::ZeroArm {
                tok,
                pos_base,
                plan,
            } => {
                let token = &program.toks[tok as usize];
                let plan = &program.zero_arms[plan as usize];
                levels.edit(|s| {
                    s.reserve_nil(&plan.flags);
                    if plan.named_zero_capture {
                        Self::store_apply_named_capture(
                            s,
                            token,
                            pos,
                            pos,
                            regs[pos_base as usize],
                        );
                    }
                    for &n in plan.list_names.iter() {
                        s.insert_named_quantified_sym(n);
                    }
                });
            }
            // Cost: O(n·m), n = the names under the token, m = the names the
            // level has filed.
            RxOp::QuantNames { names } => {
                levels.edit(|s| {
                    for &n in program.name_sets[names as usize].iter() {
                        s.insert_named_quantified_sym(n);
                    }
                });
            }
            // Cost: O(t + (k + n)·m), t = the capture-trail entries since the
            // quantifier began, k = those that filed a name, n = the names
            // under the token, m = the names the level has filed.
            RxOp::SepNames { base, names } => {
                let since = regs[base as usize];
                levels.edit(|s| s.mark_filed_quantified(since, &program.name_sets[names as usize]));
            }
            // Cost: O(k), k = the slots folded.
            RxOp::Fold { tok, pos_base } => {
                let stride =
                    super::super::regex_helpers::count_capture_groups(&program.toks[tok as usize]);
                levels.edit(|s| s.fold_quantified(regs[pos_base as usize], stride, true));
            }
            // Cost: O(p + n), p = the padding slots, n = the alternation's
            // list-valued names; O(1) when it has neither.
            RxOp::AltTail {
                alt,
                pos_base,
                suppress_padding,
            } => {
                let taken = levels.top().caps().positional.len() - regs[pos_base as usize];
                if let Some(delta) =
                    alternation_tail_delta(&program.alts[alt as usize], taken, suppress_padding)
                {
                    levels.edit(|s| s.merge_delta(delta));
                }
            }
            // Cost: O(c) for the first branch's captures, plus one nested
            // run per other branch; when `seeded`, also O(v) per other branch,
            // v = the captures visible to it (its seed).
            RxOp::ConjTail { tok, start, seeded } => {
                let RegexAtom::Conjunction(branches) = &program.toks[tok as usize].atom else {
                    unreachable!("a ConjTail names a conjunction token");
                };
                let from = regs[start as usize];
                let mut merged = merge_regex_captures(RegexCaptures::default(), levels.close());
                for branch in &branches[1..] {
                    let branch_program = super::rx_entry::program_for(branch)
                        .expect("a compiled conjunction's branches compile");
                    // Every other branch is part of the same regex too: its
                    // nested run starts from the enclosing level's view and the
                    // earlier branches' captures, as rakudo's one cursor has them.
                    let seed = seeded.then(|| {
                        super::rx_levels::inline_level_caps(
                            levels.top().caps(),
                            Some(merged.clone()),
                            None,
                        )
                    });
                    let (_, mut caps) =
                        self.rx_run_seeded(branch_program, chars, from, pkg, Some(pos), seed)?;
                    caps.set_outer_backref(None);
                    merged = merge_regex_captures(merged, caps);
                }
                levels.edit(|s| s.merge_delta(merged));
            }
            // Cost: O(1) unless an action-driven parse reads a `$*` variable;
            // then one run of the iteration's action.
            RxOp::ReduceAction { tok } => {
                self.maybe_run_reduce_time_dynvar_action(
                    &program.toks[tok as usize],
                    levels.top().caps(),
                );
            }
            // Cost: O(1).
            RxOp::Collect { sep } => levels.collect(sep),
            // Cost: O(c), c = the captures of the inner pattern and the goal
            // (one close, one merge).
            RxOp::GoalEnd { base } => {
                let goal_caps = levels.close();
                let inner_caps = levels
                    .drain_collected(regs[base as usize])
                    .pop()
                    .map(|(_, caps)| caps)
                    .unwrap_or_default();
                // As the walk's `GoalMatch` arm merges them.
                let merged = merge_goal_captures(goal_caps, inner_caps);
                levels.edit(|s| s.merge_delta(merged));
            }
            // Cost: O(1).
            RxOp::GoalFail { tok } => {
                let RegexAtom::GoalMatch { goal_text, .. } = &program.toks[tok as usize].atom
                else {
                    debug_assert!(false, "a GoalFail names a goal-match token");
                    return None;
                };
                Self::record_goal_failure(goal_text, pos);
                return None;
            }
            // Cost: O(n + c), n = the names under the token, c = the
            // captures across the collected iterations.
            RxOp::SepEmit { tok, base, names } => {
                let token = &program.toks[tok as usize];
                let entries = levels.drain_collected(regs[base as usize]);
                let delta =
                    Self::rx_sep_fold(token, &program.name_sets[names as usize], entries, true);
                levels.edit(|s| s.merge_delta(delta));
            }
            _ => unreachable!("not a capture op: {op:?}"),
        }
        Some(pos)
    }
}

impl Interpreter {
    /// The capture delta of the separated quantifier `token`'s collected
    /// iterations, `(is_separator, captures)` in match order, folded side by
    /// side as the walk's chain does (`separated_capture_delta`). With
    /// `trailing`, a last separator is a `%%` chain's trailing one.
    // Cost: O(n + c), n = the names under the token (`names`, interned when
    // the pattern compiled), c = the iterations' captures.
    fn rx_sep_fold(
        token: &crate::runtime::regex_types::RegexToken,
        names: &[Symbol],
        mut entries: Vec<(bool, RegexCaptures)>,
        at_end: bool,
    ) -> RegexCaptures {
        let names = names.iter().copied();
        let trailing = (at_end && entries.last().is_some_and(|(sep, _)| *sep))
            .then(|| entries.pop().map(|(_, caps)| caps))
            .flatten();
        let (seps, atoms): (Vec<_>, Vec<_>) = entries.into_iter().partition(|(s, _)| *s);
        let atoms: Vec<RegexCaptures> = atoms.into_iter().map(|(_, c)| c).collect();
        let seps: Vec<RegexCaptures> = seps.into_iter().map(|(_, c)| c).collect();
        // Zero iterations fold an empty chain: the names marked, and the
        // atom's and separator's positional slots reserved as empty lists.
        let sep = &token.separator.as_ref().expect("a separated token").pattern;
        separated_capture_delta_syms(
            names,
            &atoms,
            &seps,
            trailing.as_ref(),
            count_capture_groups(token),
            separator_stride(sep),
        )
    }
}

/// Count a `CapAtom` on `MUTSU_VM_STATS`'s `regex-walk:` line: the walk's
/// single-atom arm matches it, a leaf.
// Cost: O(1).
#[inline]
fn walk_leaf_use(atom: &RegexAtom) {
    use crate::vm::vm_stats_regex_vm::{WalkUse, record_regex_walk};
    let (kind, reason) = match atom {
        RegexAtom::Lookaround { .. } => (WalkUse::Leaf, "lookaround"),
        RegexAtom::Backref(_) | RegexAtom::NamedBackref(_) => (WalkUse::Leaf, "backref"),
        RegexAtom::CaptureStartMarker | RegexAtom::CaptureEndMarker => (WalkUse::Leaf, "marker"),
        RegexAtom::ClosureInterpolation { .. } => (WalkUse::Leaf, "closure-interp"),
        RegexAtom::VarInterp(_) => (WalkUse::Leaf, "var-interp"),
        RegexAtom::QqInterp { .. } => (WalkUse::Leaf, "qq-interp"),
        RegexAtom::RecurseSelf(_) => (WalkUse::Leaf, "recurse-self"),
        _ => (WalkUse::Leaf, "other"),
    };
    record_regex_walk(kind, reason);
}
