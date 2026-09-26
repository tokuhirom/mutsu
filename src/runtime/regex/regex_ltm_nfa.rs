//! A `|` branch's declarative prefix, measured by a compiled NFA
//! ([ADR-0125](../../../docs/adr/0125-ltm-declarative-prefix-nfa.md)).
//!
//! The walker measures a prefix by running the backtracking matcher under
//! `LTM_DECLARATIVE_MODE`, paying for everything a real match needs and a
//! measurement does not (capture stores, candidate continuations, one
//! allocation per set of ends). That made `regex A { '{' [ <A> | . ]*? '}' }`
//! about 45 times slower than Rakudo (#9617). Rakudo compiles each rule's
//! declarative prefix into an NFA once and runs it; so does this module:
//!
//! - [`super::regex_ltm_nfa_build`] compiles a branch, inlining the subrules it
//!   calls, into [`LtmNfa`];
//! - [`super::regex_ltm_nfa_run`] simulates it over the subject;
//! - the result is cached on the pattern per `(package, TOKEN_DEFS_GEN)`,
//!   declines included.
//!
//! Only a measurement started from a real match uses it, and only for
//! `ltm_branch_rank_key`, which needs the length but not the walker's
//! "stopped" flag. Anything else, and anything the builder declines, is
//! measured by the walker as before.
//!
//! `MUTSU_LTM_NFA_VERIFY=1` measures every NFA ranking with the walker too and
//! reports each difference on stderr (ADR-0125 §4).

use super::super::*;
use super::regex_helpers::LTM_DECLARATIVE_MODE;
use std::cell::Cell;
use std::sync::{Arc, OnceLock};

/// One node of the NFA. Edges point at node indices.
pub(super) enum NfaNode {
    /// ε-edges to every target. A placeholder loop head is an empty split
    /// until the builder patches it.
    Split(Vec<u32>),
    /// One atom, answered by the existing matcher: the single-end prober, or
    /// (`plural`) the atom matcher that returns every end.
    Leaf {
        atom: Box<RegexAtom>,
        pkg: Symbol,
        ic: bool,
        plural: bool,
        next: u32,
    },
    /// `<.ws>`: a fate, except at the very start of the subject where a rule's
    /// leading whitespace is transparent (`ltm_leading_ws_is_transparent`).
    WsLead {
        atom: Box<RegexAtom>,
        pkg: Symbol,
        ic: bool,
        next: u32,
    },
    /// A pattern's `^`: only at position 0.
    AtStart(u32),
    /// A pattern's trailing `$`: only at the end of the subject.
    AtEnd(u32),
    /// Entering an inlined rule. A left-recursion activation live for the same
    /// call would make the walker read its seed instead of the body, so the
    /// whole simulation hands the measurement back to the walker.
    Enter { name: Symbol, next: u32 },
    /// A fate: the path ends here, and here counts toward the prefix.
    Fate,
    /// The end of the branch.
    Accept,
}

pub(crate) struct LtmNfa {
    pub(super) nodes: Vec<NfaNode>,
    pub(super) start: u32,
}

/// One package's entry in `PatternDerived::ltm_nfa`: the NFA, or `None` when
/// the builder declined the pattern.
pub(crate) struct LtmNfaSlot {
    pkg: Symbol,
    generation: u64,
    nfa: Option<Arc<LtmNfa>>,
}

/// `MUTSU_LTM_NFA_VERIFY`, read once.
fn verify_enabled() -> bool {
    static VERIFY: OnceLock<bool> = OnceLock::new();
    *VERIFY.get_or_init(|| std::env::var_os("MUTSU_LTM_NFA_VERIFY").is_some_and(|v| v != "0"))
}

impl Interpreter {
    /// The prefix length of `pattern` at `pos`, measured by its NFA; `None`
    /// when the NFA does not apply here and the walker must measure.
    // Cost: O(n * s) for the simulation, n = characters the prefix can reach
    // past `pos`, s = NFA nodes; plus one build per (pattern, package,
    // generation). Rakudo: the same order (an NFA run per ranking).
    pub(super) fn ltm_nfa_prefix_len(
        &mut self,
        pattern: &RegexPattern,
        chars: &[char],
        pos: usize,
        pkg: Symbol,
    ) -> Option<Option<usize>> {
        if LTM_DECLARATIVE_MODE.with(Cell::get)
            || self.has_any_wrap_chains()
            || !self.registry().grammar_custom_how.is_empty()
        {
            return None;
        }
        let nfa = self.ltm_nfa_for(pattern, pkg)?;
        let measured = nfa.run(self, chars, pos)?;
        if verify_enabled() {
            let (walked, _) = self.ltm_prefix_len_at(pattern, chars, pos, pkg);
            if walked.unwrap_or(0) != measured.unwrap_or(0) {
                let from = pos.saturating_sub(10);
                let to = (pos + 30).min(chars.len());
                let context: String = chars[from..to].iter().collect();
                eprintln!(
                    "LTM-NFA-VERIFY: nfa={measured:?} walker={walked:?} pos={pos} pkg={} near {context:?}",
                    pkg.as_str()
                );
            }
        }
        Some(measured)
    }

    /// The cached NFA of `pattern` in `pkg`, building it on first use.
    // Cost: O(k) for a cached hit, k = packages the pattern was ranked from;
    // a miss costs one build.
    fn ltm_nfa_for(&mut self, pattern: &RegexPattern, pkg: Symbol) -> Option<Arc<LtmNfa>> {
        let generation =
            crate::runtime::regex_parse::TOKEN_DEFS_GEN.load(std::sync::atomic::Ordering::Relaxed);
        {
            let mut slots = pattern.derived.ltm_nfa.lock().ok()?;
            if slots.iter().any(|slot| slot.generation != generation) {
                slots.clear();
            }
            if let Some(slot) = slots.iter().find(|slot| slot.pkg == pkg) {
                return slot.nfa.clone();
            }
        }
        let nfa = super::regex_ltm_nfa_build::NfaBuilder::new(self)
            .build(pattern, pkg)
            .map(Arc::new);
        if let Ok(mut slots) = pattern.derived.ltm_nfa.lock() {
            slots.push(LtmNfaSlot {
                pkg,
                generation,
                nfa: nfa.clone(),
            });
        }
        nfa
    }
}
