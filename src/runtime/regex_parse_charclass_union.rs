//! Bracket character classes that unite after a subtraction:
//! `-[lead] + [pos]` and `[a] - [b] + [c]`, which Raku evaluates left to
//! right (`([a] - [b]) + [c]`). Both become an alternation of their parts,
//! each guarded by the subtractions that follow.

use super::*;

impl Interpreter {
    /// `-[lead] + [pos] - [rest]`: every character except `lead`, united with
    /// `pos`, minus `rest`. Desugared to `[ <-[lead]> | <[pos]> ]`, each branch
    /// guarded by `<!before <[rest]>>` when a later subtraction exists.
    pub(super) fn union_with_negated_lead(
        lead: Vec<ClassItem>,
        positive: Vec<ClassItem>,
        rest: Vec<ClassItem>,
    ) -> RegexAtom {
        let lead = RegexAtom::CharClass(CharClass {
            items: lead,
            negated: true,
        });
        Self::union_alternation(lead, positive, rest)
    }

    /// `first` united with the class `positive` (when it has items), minus
    /// `rest`: `[ first | <[positive]> ]`, each branch guarded by
    /// `<!before <[rest]>>` when `rest` has items.
    // Cost: O(|positive| + |rest|) to build.
    pub(super) fn union_alternation(
        first: RegexAtom,
        positive: Vec<ClassItem>,
        rest: Vec<ClassItem>,
    ) -> RegexAtom {
        let pattern = |tokens: Vec<RegexAtom>| RegexPattern {
            tokens: tokens
                .into_iter()
                .map(|atom| RegexToken {
                    atom,
                    quant: RegexQuant::One,
                    named_capture: None,
                    secondary_named_capture: None,
                    hash_capture: None,
                    force_list_capture: false,
                    ratchet: false,
                    frugal: false,
                    separator: None,
                    from_runtime_interpolation: false,
                    subrule_call_capture: false,
                })
                .collect(),
            anchor_start: false,
            anchor_end: false,
            ignore_case: false,
            ignore_mark: false,
            derived: Default::default(),
        };
        let guard = (!rest.is_empty()).then(|| RegexAtom::Lookaround {
            pattern: pattern(vec![RegexAtom::CompositeClass {
                positive: rest,
                negative: vec![],
            }]),
            negated: true,
            is_behind: false,
        });
        let branch = |atom: RegexAtom| {
            let mut atoms: Vec<RegexAtom> = guard.iter().cloned().collect();
            atoms.push(atom);
            pattern(atoms)
        };
        let mut branches = vec![branch(first)];
        if !positive.is_empty() {
            branches.push(branch(RegexAtom::CharClass(CharClass {
                items: positive,
                negated: false,
            })));
        }
        RegexAtom::Alternation(branches)
    }
}
