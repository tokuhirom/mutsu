//! ADR-0073 Slice 2: "can this rule reach itself?", answered over the rule
//! call graph.
//!
//! The `<subrule>` arm of the atom producer (`regex_match_atom.rs`) carries the
//! left-recursion growing-seed loop, which discovers that a rule is
//! left-recursive at a position by *evaluating* its candidates and then
//! checking whether the seed was consulted. That is what forces the arm to
//! collect a subrule's whole end set before the caller descends into any of
//! them — and, because an embedded `{ ... }` block runs inline for real
//! (ADR-0009), makes the block fire once per end *computed* rather than once
//! per end *entered*.
//!
//! Slice 2's first half sidestepped the loop with a purely syntactic
//! precondition ([`super::regex_subrule_lazy::pattern_is_rule_call_free`]): a
//! body that cannot invoke a named rule at all cannot re-enter its own key.
//! That admits leaf rules only, so a grammar's interior rules kept paying for
//! the full end set.
//!
//! This module answers the real question instead. A rule's key is re-entered
//! only by a *call* to a rule of the same name, so walking the call graph from
//! `(pkg, name)` and asking whether `name` appears again in the reachable set
//! decides it exactly. Anything the walk cannot resolve — a rule call with
//! arguments, `<::(EXPR)>` indirection, `<{ ... }>`, `<~~>`, a name that
//! resolves to a grammar *method* rather than to a rule, a body whose pattern
//! is not static enough to resolve at all — is answered "may re-enter", so the
//! verdict is a sound under-approximation of safety: `true` means proven safe,
//! `false` means only "not proven".
//!
//! The one construct deliberately treated as harmless is an embedded
//! `{ ... }` / `<?{ ... }>` block. It is arbitrary user code and could in
//! principle re-enter the same rule at the same position by hand; the callers
//! keep a runtime escape for that (`regex_match_atom.rs`'s `first_only`
//! retry, and the streamed subrule's seed-consulted fallback in
//! `regex_match_lazy_subrule.rs`), exactly as the first half of Slice 2 does.

use std::cell::RefCell;
// `STREAMABLE` is probed before every `<subrule>` call is even resolved, so
// these memo tables are Fx-hashed rather than SipHash-hashed — a grammar rule
// name is not adversarial input (<https://github.com/tokuhirom/mutsu/issues/7576>).
use rustc_hash::{FxHashMap as HashMap, FxHashSet as HashSet};
use std::collections::VecDeque;

use super::super::*;
use crate::runtime::regex_types::{RegexAtom, RegexPattern};

/// A `(package, rule name)` node of the call graph.
type RuleNode = (String, String);

/// Ceiling on the reachable set. A grammar with more rules than this in one
/// call cone is answered "not proven" rather than walked further — the
/// analysis exists to save work, not to spend it.
const MAX_REACHABLE_RULES: usize = 512;

/// Why a `<subrule>` call could not take the streamed path.
///
/// Each variant is one of the declined shapes #7548 enumerates, kept separate
/// because they cost very different amounts to clear: widening the *analysis*
/// (`NotKnowable`, `Arguments`, `Symbolic`) is cheap, while streaming through
/// the growing-seed loop (`ReentersOwnName`), a proto's rank-then-match
/// dispatch (`Proto`) or several candidates' interleaved walks
/// (`SeveralCandidates`) is the expensive machinery. `Str` names are what the
/// `MUTSU_VM_STATS` histogram reports.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum StreamDecline {
    /// `<expr($p-1)>` — the arguments are part of the left-recursion key and
    /// are evaluated per call, so the memoized verdict cannot apply.
    Arguments,
    /// `<::(EXPR)>` symbolic indirection: the target resolves per call.
    Symbolic,
    /// The name answers to no token/regex/rule here — a builtin assertion or
    /// character class, or a plain grammar method (arbitrary user code).
    NotARule,
    /// Several resolved candidates without a proto: the eager arm deduplicates
    /// ends *across* them, which a stream would have to interleave to preserve.
    SeveralCandidates,
    /// A proto/`multi` subrule: ADR-0046 dispatch wants the winning
    /// candidate's whole end set.
    Proto,
    /// `:m` remaps positions across the whole result set.
    IgnoreMark,
    /// The rule really is part of a call cycle. Left recursion is why the
    /// growing-seed loop exists; this is the genuinely hard residue.
    ReentersOwnName,
    /// Some rule in the call cone is a plain grammar METHOD — arbitrary user
    /// code, whose dispatch targets are not a property of the token
    /// generation. Nothing short of running it can widen this.
    CalleeIsMethod,
    /// Some rule in the call cone splices a value into its own pattern text
    /// outside a `{ ... }` code block, so its call edges are not stable across
    /// attempts. Widening this means tracking *which* interpolations can
    /// introduce a rule call, rather than refusing on any of them.
    CalleeInterpolates,
    /// Some rule in the call cone contains a construct
    /// (`<{ ... }>` closure interpolation, `<~~>`, ...) whose dispatch target
    /// `collect_pattern_calls` will not name.
    CalleeEdgeUnresolvable,
    /// The reachable set hit [`MAX_REACHABLE_RULES`].
    ReachableSetTooLarge,
    /// A `$*`-twigil rule parameter is declared somewhere in the program, so
    /// the call has to install and tear down a dynamic scope around itself.
    DynamicRuleParam,
    /// The grammar has a custom HOW, so dispatch is not static.
    CustomHow,
    /// This `(name, position)` key is already left-recursion-active.
    LrKeyActive,
    /// An embedded `{ ... }` block re-entered the key mid-stream, so the single
    /// pass was not the growing-seed loop's answer and the call was handed back
    /// to the eager arm.
    SeedConsulted,
}

impl StreamDecline {
    /// Short stable name for the `MUTSU_VM_STATS` histogram.
    pub(super) fn as_str(self) -> &'static str {
        match self {
            Self::Arguments => "call-arguments",
            Self::Symbolic => "symbolic-indirection",
            Self::NotARule => "not-a-rule",
            Self::SeveralCandidates => "several-candidates",
            Self::Proto => "proto-candidate",
            Self::IgnoreMark => "ignore-mark",
            Self::ReentersOwnName => "reenters-own-name",
            Self::CalleeIsMethod => "callee-is-grammar-method",
            Self::CalleeInterpolates => "callee-interpolates",
            Self::CalleeEdgeUnresolvable => "callee-edge-unresolvable",
            Self::ReachableSetTooLarge => "reachable-set-too-large",
            Self::DynamicRuleParam => "dynamic-rule-param",
            Self::CustomHow => "custom-how-grammar",
            Self::LrKeyActive => "lr-key-active",
            Self::SeedConsulted => "seed-consulted",
        }
    }
}

thread_local! {
    /// `(name, pkg) -> the rules its candidates call directly`, or the reason
    /// some construct in one of them cannot be resolved. Same generation key.
    #[allow(clippy::type_complexity)]
    static DIRECT_CALLS: RefCell<(
        u64,
        HashMap<RuleNode, Result<std::sync::Arc<Vec<RuleNode>>, StreamDecline>>,
    )> = RefCell::new((0, HashMap::default()));

    /// `pkg -> subrule atom text -> may this call be streamed?`. This is the
    /// hot lookup: it is consulted before the call is resolved (and before its
    /// name is even parsed), so a rule that is not streamable costs two
    /// borrowed hash lookups rather than a resolution plus a graph walk. The
    /// two-level shape is what keeps it allocation-free — a `(String, String)`
    /// key would have to be built to probe it. Same generation key.
    ///
    /// `None` is "streamable"; `Some(reason)` names the shape that declined it,
    /// so the memoized answer still feeds the per-call histogram
    /// ([`crate::vm::vm_stats::record_subrule_stream`]) without recomputing.
    #[allow(clippy::type_complexity)]
    static STREAMABLE: RefCell<(u64, HashMap<String, HashMap<String, Option<StreamDecline>>>)> =
        RefCell::new((0, HashMap::default()));
}

fn token_defs_gen() -> u64 {
    crate::runtime::regex_parse::TOKEN_DEFS_GEN.load(std::sync::atomic::Ordering::Relaxed)
}

impl Interpreter {
    /// The reachability walk itself: is a call to `name` reachable from
    /// `(pkg, name)`? `None` when it is proven unreachable; otherwise the
    /// reason the walk gave up, which is not the same question -- a real cycle
    /// and an unresolvable edge both mean "may re-enter" but cost entirely
    /// different work to clear.
    fn reenter_decline(&mut self, name: &str, pkg: &str) -> Option<StreamDecline> {
        let start: RuleNode = (pkg.to_string(), name.to_string());
        let mut seen: HashSet<RuleNode> = HashSet::from_iter([start.clone()]);
        let mut queue: VecDeque<RuleNode> = VecDeque::from([start]);
        while let Some((cur_pkg, cur_name)) = queue.pop_front() {
            let calls = match self.direct_rule_calls(&cur_name, &cur_pkg) {
                Ok(calls) => calls,
                Err(reason) => return Some(reason),
            };
            for callee in calls.iter() {
                // Reaching the starting NAME again closes the loop the
                // growing-seed algorithm exists for.
                if callee.1 == name {
                    return Some(StreamDecline::ReentersOwnName);
                }
                if seen.len() >= MAX_REACHABLE_RULES {
                    return Some(StreamDecline::ReachableSetTooLarge);
                }
                if seen.insert(callee.clone()) {
                    queue.push_back(callee.clone());
                }
            }
        }
        None
    }

    /// The rules every candidate of `<name>` in `pkg` calls directly, or `None`
    /// when the answer is not knowable for the whole token generation — either
    /// because a candidate contains a construct whose dispatch target is not
    /// static, or because the body is re-parsed per call and so could gain an
    /// edge between two attempts. Both are cached: a "not knowable" verdict is
    /// as stable as a known edge set, and re-deciding it per call is what made
    /// this analysis cost more than it saved.
    fn direct_rule_calls(
        &mut self,
        name: &str,
        pkg: &str,
    ) -> Result<std::sync::Arc<Vec<RuleNode>>, StreamDecline> {
        let generation = token_defs_gen();
        let key = (pkg.to_string(), name.to_string());
        if let Some(hit) = DIRECT_CALLS.with(|c| {
            let c = c.borrow();
            (c.0 == generation)
                .then(|| c.1.get(&key).cloned())
                .flatten()
        }) {
            return hit;
        }
        // The memoized resolver only answers for candidates whose pattern text
        // is static ANYWHERE, which a `{ $n++ }` block alone is enough to spoil.
        // When it declines, fall back to the same per-call resolution the
        // matcher itself uses — but only when the sole reason the body is
        // non-static lives inside a code block, because parse-time interpolation
        // treats those as opaque (`interpolate_bound_regex_scalars`). A body
        // that really does splice a value in is answered `None`.
        let computed =
            match self.resolve_parsed_token_candidates_in_pkg(name, Symbol::intern(name), pkg) {
                Some(candidates) => {
                    let raw_empty = candidates.is_empty();
                    self.rule_calls_of(name, pkg, &candidates, raw_empty)
                }
                None if self.rule_body_edges_are_generation_stable(name, pkg) => {
                    let spec = Self::parse_named_regex_lookup_spec(name);
                    let (candidates, raw_empty) = self.parsed_subrule_candidates(&spec, pkg, &[]);
                    self.rule_calls_of(name, pkg, &candidates, raw_empty)
                }
                None => Err(StreamDecline::CalleeInterpolates),
            };
        DIRECT_CALLS.with(|c| {
            let mut c = c.borrow_mut();
            if c.0 != generation {
                c.0 = generation;
                c.1.clear();
            }
            c.1.insert(key, computed.clone());
        });
        computed
    }

    /// The direct call edges of one already-resolved candidate list.
    fn rule_calls_of(
        &mut self,
        name: &str,
        pkg: &str,
        candidates: &[super::regex_token_resolve::ParsedTokenCandidate],
        raw_empty: bool,
    ) -> Result<std::sync::Arc<Vec<RuleNode>>, StreamDecline> {
        if raw_empty || candidates.is_empty() {
            // No token/regex/rule answers to this name here. It is either a
            // builtin assertion or character class (`<alpha>`, `<ws>` with no
            // grammar override, `<sym>`), which cannot dispatch to a user rule,
            // or a plain grammar METHOD — arbitrary user code, so unknowable.
            return match self.registry().user_method_overloads(pkg, name) {
                None => Ok(std::sync::Arc::new(Vec::new())),
                Some(_) => Err(StreamDecline::CalleeIsMethod),
            };
        }
        let mut out: Vec<RuleNode> = Vec::new();
        for (parsed, sub_pkg, _) in candidates.iter() {
            // A candidate's own body resolves its unqualified subrule
            // references against the package that DEFINED it, not against the
            // caller's — the same rule `subrule_candidate_ends` matches under.
            if !collect_pattern_calls(parsed, sub_pkg, &mut out) {
                return Err(StreamDecline::CalleeEdgeUnresolvable);
            }
        }
        out.sort();
        out.dedup();
        Ok(std::sync::Arc::new(out))
    }
}

impl Interpreter {
    /// Whether the streamed `<subrule>` path in `regex_match_lazy_subrule.rs`
    /// may take this call at all: the memoized front door, consulted BEFORE the
    /// call is resolved so an ineligible one costs a hash lookup instead of a
    /// resolution plus a call-graph walk.
    ///
    /// `atom_text` is the subrule atom exactly as written,
    /// so `<foo>` and `<&foo>` get their own entries rather than sharing one.
    pub(super) fn subrule_call_stream_decline(
        &mut self,
        atom_text: &str,
        pkg: &str,
    ) -> Option<StreamDecline> {
        let generation = token_defs_gen();
        if let Some(hit) = STREAMABLE.with(|c| {
            let c = c.borrow();
            (c.0 == generation)
                .then(|| c.1.get(pkg).and_then(|m| m.get(atom_text)).copied())
                .flatten()
        }) {
            return hit;
        }
        let verdict = self.compute_stream_decline(atom_text, pkg);
        STREAMABLE.with(|c| {
            let mut c = c.borrow_mut();
            if c.0 != generation {
                c.0 = generation;
                c.1.clear();
            }
            c.1.entry(pkg.to_string())
                .or_default()
                .insert(atom_text.to_string(), verdict);
        });
        verdict
    }

    fn compute_stream_decline(&mut self, atom_text: &str, pkg: &str) -> Option<StreamDecline> {
        let spec = Self::parse_named_regex_lookup_spec(atom_text);
        // A rule call with arguments, and `<::(EXPR)>` symbolic indirection,
        // both resolve per call; neither is a shape this path handles.
        if !spec.arg_exprs.is_empty() {
            return Some(StreamDecline::Arguments);
        }
        if spec.lookup_name == "::" {
            return Some(StreamDecline::Symbolic);
        }
        let (candidates, raw_empty) = self.parsed_subrule_candidates(&spec, pkg, &[]);
        // Exactly one plain candidate. A proto keeps its rank-then-match
        // dispatch (ADR-0046), several candidates need the cross-candidate dedup
        // the eager arm performs, and `:m` remaps positions across the whole
        // result set. All three are properties of the rule's DEFINITIONS, so the
        // verdict holds for the whole token generation even when the body's
        // parse does not.
        if raw_empty || candidates.is_empty() {
            return Some(StreamDecline::NotARule);
        }
        let [(parsed, _, sym)] = &candidates[..] else {
            return Some(StreamDecline::SeveralCandidates);
        };
        if sym.is_some() {
            return Some(StreamDecline::Proto);
        }
        if parsed.ignore_mark {
            return Some(StreamDecline::IgnoreMark);
        }
        self.reenter_decline(&spec.lookup_name, pkg)
    }

    /// `true` when every definition answering to `<name>` in `pkg` compiles to
    /// the same rule-call edges on every match attempt.
    ///
    /// A body is re-parsed per call when its text carries an interpolation, and
    /// a `$`-valued `Regex` is spliced in as pattern SOURCE
    /// (`interpolate_bound_regex_scalars`), so such a body really can gain or
    /// lose a call edge between two attempts. But that interpolation pass treats
    /// an embedded `{ ... }` code block as opaque, so a sigil that only appears
    /// inside one — `token part { \w+ { $n++ } }`, the overwhelmingly common
    /// case — does not make the parse value-dependent at all.
    fn rule_body_edges_are_generation_stable(&mut self, name: &str, pkg: &str) -> bool {
        self.resolve_token_patterns_static_in_pkg(name, pkg)
            .iter()
            .all(|(pattern, _, _)| pattern_text_is_static_outside_code_blocks(pattern))
    }
}

/// [`Interpreter::rule_body_edges_are_generation_stable`]'s scan: blank out
/// every balanced `{ ... }` code block and ask the ordinary staticness question
/// about what is left. An unbalanced block is answered `false` rather than
/// guessed at.
fn pattern_text_is_static_outside_code_blocks(pattern: &str) -> bool {
    let chars: Vec<char> = pattern.chars().collect();
    let mut blanked = String::with_capacity(pattern.len());
    let mut i = 0usize;
    while i < chars.len() {
        let c = chars[i];
        if c == '\\' {
            blanked.push(c);
            i += 1;
            if i < chars.len() {
                blanked.push(chars[i]);
                i += 1;
            }
            continue;
        }
        if c == '{' {
            let mut depth = 1usize;
            let mut j = i + 1;
            while j < chars.len() && depth > 0 {
                match chars[j] {
                    '\\' => j += 1,
                    '{' => depth += 1,
                    '}' => depth -= 1,
                    _ => {}
                }
                j += 1;
            }
            if depth > 0 {
                return false;
            }
            // Keep the braces so a `**{...}` quantifier still reads as one.
            blanked.push('{');
            blanked.push('}');
            i = j;
            continue;
        }
        blanked.push(c);
        i += 1;
    }
    crate::runtime::regex_parse::regex_pattern_is_static(&blanked)
}

/// Append every rule `pattern` (matched in `pkg`) can dispatch to. Returns
/// `false` when it contains a construct whose target is not statically known,
/// in which case `out` is meaningless.
fn collect_pattern_calls(pattern: &RegexPattern, pkg: &str, out: &mut Vec<RuleNode>) -> bool {
    pattern.tokens.iter().all(|token| {
        collect_atom_calls(&token.atom, pkg, out)
            && token
                .separator
                .as_ref()
                .is_none_or(|sep| collect_pattern_calls(&sep.pattern, pkg, out))
    })
}

fn collect_atom_calls(atom: &RegexAtom, pkg: &str, out: &mut Vec<RuleNode>) -> bool {
    match atom {
        // Matches text or asserts on its own; reaches no dispatcher.
        RegexAtom::Literal(_)
        | RegexAtom::Any
        | RegexAtom::CharClass(_)
        | RegexAtom::Newline
        | RegexAtom::NotNewline
        | RegexAtom::ZeroWidth
        // User code. See the module header: treated as harmless here, with a
        // runtime escape at both call sites.
        | RegexAtom::CodeAssertion { .. }
        | RegexAtom::UnicodeProp { .. }
        | RegexAtom::UnicodePropAssert { .. }
        | RegexAtom::CaptureStartMarker
        | RegexAtom::CaptureEndMarker
        | RegexAtom::VarDecl { .. }
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
        // Interpolates a variable's STRING value as a literal, not as a rule.
        | RegexAtom::VarInterp(_)
        | RegexAtom::SameAssertion { .. }
        | RegexAtom::AtPosition(_)
        | RegexAtom::TildeMarker => true,
        RegexAtom::Group(p) | RegexAtom::CaptureGroup(p) | RegexAtom::CaptureIsolatedGroup(p) => {
            collect_pattern_calls(p, pkg, out)
        }
        RegexAtom::Alternation(alts)
        | RegexAtom::SequentialAlternation(alts)
        | RegexAtom::Conjunction(alts) => alts
            .iter()
            .all(|alt| collect_pattern_calls(alt, pkg, out)),
        RegexAtom::Lookaround { pattern, .. } => collect_pattern_calls(pattern, pkg, out),
        RegexAtom::GoalMatch { goal, inner, .. } => {
            collect_pattern_calls(goal, pkg, out) && collect_pattern_calls(inner, pkg, out)
        }
        RegexAtom::WsRule => {
            // `<.ws>` dispatches to whatever `ws` the grammar resolves to.
            out.push((pkg.to_string(), "ws".to_string()));
            true
        }
        RegexAtom::Named(name) => {
            let spec = Interpreter::parse_named_regex_lookup_spec(name);
            // `<::(EXPR)>` names its target at runtime, and a rule call with
            // arguments resolves per argument list — neither is a static edge.
            if spec.lookup_name == "::" || !spec.arg_exprs.is_empty() {
                return false;
            }
            // Anything that is not a plain (possibly qualified) rule name is a
            // form this walk does not model — `<$rx>` interpolates a regex whose
            // own calls are not visible here, and a character-class spec that
            // reached this variant is not a dispatch at all. Both answer
            // "unknown" rather than being recorded as an edge to a rule that
            // does not exist.
            if spec.lookup_name.is_empty()
                || !spec
                    .lookup_name
                    .chars()
                    .all(|c| c.is_alphanumeric() || matches!(c, '_' | '-' | ':'))
            {
                return false;
            }
            out.push((pkg.to_string(), spec.lookup_name.clone()));
            true
        }
        // `<{ ... }>` matches whatever regex the code returns; `<~~>` re-enters
        // the enclosing rule by construction.
        RegexAtom::ClosureInterpolation { .. } | RegexAtom::RecurseSelf(_) => false,
    }
}
