//! The compiled engine's capture-writing ops. Each one applies the walk's own
//! capture transform (ADR-0135 D4) to the innermost capture level, through
//! `Levels::edit` so a backtrack undoes it.

use super::super::regex_helpers::{count_capture_groups, merge_regex_captures};
use super::super::regex_match_delta::{alternation_tail_delta, capture_group_delta};
use super::super::regex_match_sep::{separated_capture_delta, separator_stride};
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
            RxOp::OpenCapture => levels.open(pos),
            // Cost: O(1).
            RxOp::DropCapture => levels.discard(),
            // Cost: O(c) amortized, c = the closed level's captures (one
            // snapshot); O(1) for a group whose body captures nothing.
            RxOp::CloseCapture { start, nested } => {
                let from = regs[start as usize];
                let inner = if nested {
                    levels.close()
                } else {
                    RegexCaptures {
                        match_from: from,
                        ..Default::default()
                    }
                };
                levels.edit(|s| s.merge_delta(capture_group_delta(from, pos, inner)));
            }
            // Cost: O(n), n = the characters a backreference compares; O(1)
            // for a marker; one run of the body for a lookahead, and one per
            // candidate start (at most the body's longest match back) for a
            // lookbehind.
            RxOp::CapAtom(i) => {
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
            // Cost: O(a + n), a = the atom's capture groups, n = the names
            // under it (the walk's zero arm, same order).
            RxOp::ZeroArm { tok, pos_base } => {
                let token = &program.toks[tok as usize];
                let flags =
                    super::super::regex_helpers::capture_group_list_flags(&token.atom, false);
                let named_zero_capture =
                    !matches!(token.atom, RegexAtom::CaptureGroup(_) | RegexAtom::Named(_))
                        && !token.subrule_call_capture;
                let mut list_names = std::collections::HashSet::new();
                Self::collect_nested_list_quantified_names(&token.atom, &mut list_names);
                levels.edit(|s| {
                    s.reserve_nil(&flags);
                    if named_zero_capture {
                        Self::store_apply_named_capture(
                            s,
                            token,
                            pos,
                            pos,
                            regs[pos_base as usize],
                        );
                    }
                    for n in list_names {
                        s.insert_named_quantified(n);
                    }
                });
            }
            // Cost: O(n), n = the names under the token.
            RxOp::QuantNames { tok } => {
                let names = Self::collect_quantified_names_for_token(&program.toks[tok as usize]);
                levels.edit(|s| {
                    for n in names {
                        s.insert_named_quantified(n);
                    }
                });
            }
            // Cost: O(k), k = the slots folded.
            RxOp::Fold { tok, pos_base } => {
                let stride = super::super::regex_helpers::count_capture_groups(
                    &program.toks[tok as usize].atom,
                );
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
            // run per other branch.
            RxOp::ConjTail { tok, start } => {
                let RegexAtom::Conjunction(branches) = &program.toks[tok as usize].atom else {
                    unreachable!("a ConjTail names a conjunction token");
                };
                let from = regs[start as usize];
                let mut merged = merge_regex_captures(RegexCaptures::default(), levels.close());
                for branch in &branches[1..] {
                    let branch_program = super::rx_vm::program_for(branch)
                        .expect("a compiled conjunction's branches compile");
                    let (_, caps) = self.rx_run(branch_program, chars, from, pkg, Some(pos))?;
                    merged = merge_regex_captures(merged, caps);
                }
                levels.edit(|s| s.merge_delta(merged));
            }
            // Cost: O(1).
            RxOp::Collect { sep } => levels.collect(sep),
            // Cost: O(n + c), n = the names under the token, c = the
            // captures across the collected iterations.
            RxOp::SepEmit { tok, base } => {
                let token = &program.toks[tok as usize];
                let names = Self::collect_quantified_names_for_token(token);
                let mut entries = levels.drain_collected(regs[base as usize]);
                // A `%%` chain that ends on a separator took a trailing one.
                let trailing = entries
                    .last()
                    .is_some_and(|(sep, _)| *sep)
                    .then(|| entries.pop().map(|(_, caps)| caps))
                    .flatten();
                let (seps, atoms): (Vec<_>, Vec<_>) = entries.into_iter().partition(|(s, _)| *s);
                let atoms: Vec<RegexCaptures> = atoms.into_iter().map(|(_, c)| c).collect();
                let seps: Vec<RegexCaptures> = seps.into_iter().map(|(_, c)| c).collect();
                let delta = if atoms.is_empty() {
                    // Zero iterations mark the names only.
                    separated_capture_delta(&names, &[], &[], None, 0, 0)
                } else {
                    let sep = &token.separator.as_ref().expect("a separated token").pattern;
                    separated_capture_delta(
                        &names,
                        &atoms,
                        &seps,
                        trailing.as_ref(),
                        count_capture_groups(&token.atom),
                        separator_stride(sep),
                    )
                };
                levels.edit(|s| s.merge_delta(delta));
            }
            _ => unreachable!("not a capture op: {op:?}"),
        }
        Some(pos)
    }
}
