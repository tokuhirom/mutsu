//! Lazily-built indexes over a chunk's `ops` that used to be answered by a
//! linear scan of the whole chunk on every query (#9173):
//!
//! - the `Label` table: `goto LABEL` / a labelled-loop control exception looked
//!   its target up by scanning every op, and every `run_inner` / `run_reuse`
//!   entry re-validated label uniqueness by scanning every op again (the
//!   latter once per `map`/`grep` callback iteration);
//! - the `StateVarInit` positions: resetting / syncing the `state` locals of a
//!   range scanned the range once per state local, O(t * b);
//! - the declaration facts of every `BlockScope` / `BlockLocalScope` range
//!   ([`BlockRangeFacts`]): each entry scanned the block's ops for the slots it
//!   declares and the topic binders it holds, O(b) per execution (#9170).
//!
//! Both are pure functions of `ops` and the constant pool, so they are built
//! once per chunk, on first use. Each index records the `ops.len()` it was
//! built from; a chunk whose op vector changed length since (a runtime-patched
//! chunk) is answered by the old linear scan instead, so a stale index can
//! never be consulted.

use crate::opcode::{CompiledCode, OpCode};
use crate::symbol::Symbol;
use rustc_hash::FxHashMap;

fn label_name(code: &CompiledCode, idx: u32) -> &str {
    code.constants[idx as usize].as_str().unwrap_or("")
}

#[derive(Clone, Debug, Default)]
pub(crate) struct OpScanIndex {
    /// `ops.len()` when this index was built.
    ops_len: usize,
    /// Label name -> the ip just past its FIRST `Label` op (what the linear
    /// `find_map` scan returned).
    label_targets: FxHashMap<String, usize>,
    /// The first label name declared twice, if any (`X::Redeclaration`).
    duplicate_label: Option<String>,
    /// Parallel to `CompiledCode::state_locals`: the ips of every
    /// `StateVarInit` op whose `(slot, key)` matches that entry, ascending.
    state_init_ips: Vec<Box<[usize]>>,
    /// Every `(ip, index into state_locals)` pair of `state_init_ips`, ordered
    /// by ip, so the state locals initialised in a range are a binary search
    /// plus the matches rather than a walk over all of `state_locals`.
    state_inits_by_ip: Box<[(usize, u32)]>,
    /// `slot` -> the key of the FIRST `state_locals` entry for it, as
    /// `publish_state_local` looks it up on every write to a local.
    state_key_by_slot: FxHashMap<usize, Symbol>,
    /// The ip of each `BlockScope` / `BlockLocalScope` op -> the facts of the
    /// range it scopes.
    block_facts: FxHashMap<usize, BlockRangeFacts>,
}

/// What a scope's exit cleanup needs to know about the ops of the scoped range:
/// pure functions of those ops (and the constant pool), so computed once per
/// chunk rather than by a scan on every entry.
#[derive(Clone, Debug, Default)]
pub(crate) struct BlockRangeFacts {
    /// The slots the range's own declarations (`SetLocalDecl`, a block-scoped
    /// `DeclareOurScalar`) own.
    pub(crate) owned_slots: rustc_hash::FxHashSet<usize>,
    /// The slots of the range's `state` declarations (`StateVarInit`).
    pub(crate) state_slots: rustc_hash::FxHashSet<usize>,
    /// Whether the range binds a topic of its own anywhere: an opcode that can
    /// leave `$_` pointing at something the block chose, or a `$_ := ...`
    /// rebinding (`MarkRebindContext` plus a store to `_`). A plain `$_ = ...`
    /// assignment is not one.
    pub(crate) binds_own_topic: bool,
}

impl BlockRangeFacts {
    // Cost: O(b), b = ops in `[start, end)`.
    pub(crate) fn scan(code: &CompiledCode, start: usize, end: usize) -> Self {
        let ops = &code.ops[start..end.min(code.ops.len()).max(start)];
        let mut facts = Self::default();
        let mut rebind_context = false;
        let mut stores_topic = false;
        for op in ops {
            match op {
                OpCode::SetLocalDecl { slot, .. } | OpCode::DeclareOurScalar { slot, .. } => {
                    facts.owned_slots.insert(*slot as usize);
                }
                OpCode::StateVarInit(slot, _) => {
                    facts.state_slots.insert(*slot as usize);
                }
                OpCode::SetTopic
                | OpCode::SaveTopic
                | OpCode::RestoreTopic
                | OpCode::EnterPointyTopic
                | OpCode::ExitPointyTopic
                | OpCode::ForLoop(_)
                | OpCode::Given { .. }
                | OpCode::DoGivenExpr { .. }
                | OpCode::When { .. } => facts.binds_own_topic = true,
                OpCode::MarkRebindContext => rebind_context = true,
                OpCode::SetGlobal(idx)
                    if code
                        .constants
                        .get(*idx as usize)
                        .is_some_and(|c| c.as_str() == Some("_")) =>
                {
                    stores_topic = true;
                }
                _ => {}
            }
        }
        facts.binds_own_topic |= rebind_context && stores_topic;
        facts
    }
}

impl OpScanIndex {
    fn build(code: &CompiledCode) -> Self {
        let mut label_targets: FxHashMap<String, usize> = FxHashMap::default();
        let mut duplicate_label = None;
        let mut state_slots: FxHashMap<(usize, Symbol), usize> = FxHashMap::default();
        for (i, (slot, key)) in code.state_locals.iter().enumerate() {
            state_slots.entry((*slot, *key)).or_insert(i);
        }
        let mut state_init_ips: Vec<Vec<usize>> = vec![Vec::new(); code.state_locals.len()];
        let mut block_facts: FxHashMap<usize, BlockRangeFacts> = FxHashMap::default();
        for (ip, op) in code.ops.iter().enumerate() {
            match op {
                OpCode::BlockScope { end, .. } => {
                    block_facts.insert(ip, BlockRangeFacts::scan(code, ip + 1, *end as usize));
                }
                OpCode::BlockLocalScope { body_end, .. } => {
                    block_facts.insert(ip, BlockRangeFacts::scan(code, ip + 1, *body_end as usize));
                }
                OpCode::Label(name_idx) => {
                    let name = label_name(code, *name_idx);
                    if label_targets.contains_key(name) {
                        if duplicate_label.is_none() {
                            duplicate_label = Some(name.to_string());
                        }
                    } else {
                        label_targets.insert(name.to_string(), ip + 1);
                    }
                }
                OpCode::StateVarInit(s, k) if !state_slots.is_empty() => {
                    if let Some(&i) = state_slots.get(&(*s as usize, *k)) {
                        state_init_ips[i].push(ip);
                    }
                }
                _ => {}
            }
        }
        // Several `state_locals` entries may share one `(slot, key)`; each must
        // see the same init ips, as the per-entry scan did.
        for (i, (slot, key)) in code.state_locals.iter().enumerate() {
            let first = state_slots[&(*slot, *key)];
            if first != i {
                state_init_ips[i] = state_init_ips[first].clone();
            }
        }
        let mut state_inits_by_ip: Vec<(usize, u32)> = state_init_ips
            .iter()
            .enumerate()
            .flat_map(|(i, ips)| ips.iter().map(move |&ip| (ip, i as u32)))
            .collect();
        state_inits_by_ip.sort_unstable();
        let mut state_key_by_slot: FxHashMap<usize, Symbol> = FxHashMap::default();
        for (slot, key) in &code.state_locals {
            state_key_by_slot.entry(*slot).or_insert(*key);
        }
        Self {
            ops_len: code.ops.len(),
            label_targets,
            duplicate_label,
            state_key_by_slot,
            state_inits_by_ip: state_inits_by_ip.into_boxed_slice(),
            state_init_ips: state_init_ips
                .into_iter()
                .map(Vec::into_boxed_slice)
                .collect(),
            block_facts,
        }
    }
}

impl CompiledCode {
    /// The chunk's op-scan index, or `None` when `ops` changed length since it
    /// was built (the caller then falls back to a linear scan).
    fn op_scan_index(&self) -> Option<&OpScanIndex> {
        let idx = self.op_scan_index.get_or_init(|| OpScanIndex::build(self));
        (idx.ops_len == self.ops.len()).then_some(idx)
    }

    /// The ip just past the first `Label` op named `label`.
    // Cost: O(1) amortized (one O(p) index build per chunk, p = ops).
    pub(crate) fn label_target(&self, label: &str) -> Option<usize> {
        match self.op_scan_index() {
            Some(idx) => idx.label_targets.get(label).copied(),
            None => self.ops.iter().enumerate().find_map(|(i, op)| match op {
                OpCode::Label(name_idx) if label_name(self, *name_idx) == label => Some(i + 1),
                _ => None,
            }),
        }
    }

    /// The first label declared twice in this chunk, if any.
    // Cost: O(1) amortized (one O(p) index build per chunk, p = ops).
    pub(crate) fn duplicate_label(&self) -> Option<String> {
        match self.op_scan_index() {
            Some(idx) => idx.duplicate_label.clone(),
            None => OpScanIndex::build(self).duplicate_label,
        }
    }

    /// The declaration facts of the range scoped by the `BlockScope` /
    /// `BlockLocalScope` op at `op_ip`, which covers `[op_ip + 1, end)`.
    // Cost: O(1) amortized (built with the chunk's index, O(sum of the scoped
    // ranges) once); a stale index rescans the range, O(b).
    pub(crate) fn block_range_facts(
        &self,
        op_ip: usize,
        end: usize,
    ) -> std::borrow::Cow<'_, BlockRangeFacts> {
        match self
            .op_scan_index()
            .and_then(|idx| idx.block_facts.get(&op_ip))
        {
            Some(facts) => std::borrow::Cow::Borrowed(facts),
            None => std::borrow::Cow::Owned(BlockRangeFacts::scan(self, op_ip + 1, end)),
        }
    }

    /// The key of the first `state_locals` entry for `slot`: what a write to a
    /// local publishes under (`publish_state_local`). A hash probe instead of
    /// a walk over every state local of the chunk on every local write (#11347).
    // Cost: O(1) amortized (one O(p) index build per chunk); a stale index
    // walks `state_locals`, O(t).
    pub(crate) fn state_key_for_slot(&self, slot: usize) -> Option<Symbol> {
        match self.op_scan_index() {
            Some(idx) => idx.state_key_by_slot.get(&slot).copied(),
            None => self
                .state_locals
                .iter()
                .find(|(s, _)| *s == slot)
                .map(|(_, k)| *k),
        }
    }

    /// Call `f` once with the index into `state_locals` of every entry that has
    /// a `StateVarInit` op in `[start, end)`, in ascending order of the first
    /// such op. What a loop's per-iteration state sync and its entry reset
    /// need, without a test per state local of the whole chunk (#11347).
    // Cost: O(log p + m), p = StateVarInit ops of the chunk, m = those in the
    // range (O(m^2) only to drop an entry initialised twice in the range).
    pub(crate) fn for_each_state_local_in_range(
        &self,
        start: usize,
        end: usize,
        mut f: impl FnMut(usize),
    ) {
        let Some(idx) = self.op_scan_index() else {
            for i in 0..self.state_locals.len() {
                if self.state_local_init_in_range(i, start, end) {
                    f(i);
                }
            }
            return;
        };
        let pairs = &idx.state_inits_by_ip;
        let lo = pairs.partition_point(|&(ip, _)| ip < start);
        let hi = lo + pairs[lo..].partition_point(|&(ip, _)| ip < end);
        for p in lo..hi {
            let i = pairs[p].1;
            if pairs[lo..p].iter().all(|&(_, earlier)| earlier != i) {
                f(i as usize);
            }
        }
    }

    /// Whether the `i`-th `state_locals` entry has a `StateVarInit` op in
    /// `[start, end)`.
    // Cost: O(log k), k = init ops of that state local (almost always 1).
    pub(crate) fn state_local_init_in_range(&self, i: usize, start: usize, end: usize) -> bool {
        if let Some(idx) = self.op_scan_index()
            && let Some(ips) = idx.state_init_ips.get(i)
        {
            let first_at_or_after = ips.partition_point(|&ip| ip < start);
            return ips.get(first_at_or_after).is_some_and(|&ip| ip < end);
        }
        let Some(&(slot, key)) = self.state_locals.get(i) else {
            return false;
        };
        self.ops[start..end].iter().any(|op| {
            matches!(op, OpCode::StateVarInit(s, k)
                if *s as usize == slot && *k == key)
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::value::Value;

    fn label(code: &mut CompiledCode, name: &str) {
        let idx = code.constants.len() as u32;
        code.constants.push(Value::str(name.to_string()));
        code.ops.push(OpCode::Label(idx));
    }

    #[test]
    fn label_targets_and_duplicates() {
        let mut code = CompiledCode::new();
        code.ops.push(OpCode::Pop);
        label(&mut code, "A");
        label(&mut code, "B");
        label(&mut code, "A");
        assert_eq!(code.label_target("A"), Some(2));
        assert_eq!(code.label_target("B"), Some(3));
        assert_eq!(code.label_target("C"), None);
        assert_eq!(code.duplicate_label().as_deref(), Some("A"));
    }

    #[test]
    fn state_init_ranges() {
        let mut code = CompiledCode::new();
        let k = Symbol::intern("$x@1");
        code.state_locals.push((0, k));
        code.ops.push(OpCode::Pop);
        code.ops.push(OpCode::StateVarInit(0, k));
        code.ops.push(OpCode::Pop);
        assert!(code.state_local_init_in_range(0, 0, 3));
        assert!(code.state_local_init_in_range(0, 1, 2));
        assert!(!code.state_local_init_in_range(0, 2, 3));
        assert!(!code.state_local_init_in_range(0, 0, 1));
    }

    #[test]
    fn state_locals_in_range_are_listed_once_in_order() {
        let mut code = CompiledCode::new();
        let (a, b, c) = (
            Symbol::intern("$a@1"),
            Symbol::intern("$b@1"),
            Symbol::intern("$c@1"),
        );
        code.state_locals.push((0, a));
        code.state_locals.push((1, b));
        code.state_locals.push((2, c));
        code.ops.push(OpCode::StateVarInit(0, a)); // 0
        code.ops.push(OpCode::Pop); // 1
        code.ops.push(OpCode::StateVarInit(1, b)); // 2
        code.ops.push(OpCode::StateVarInit(2, c)); // 3
        code.ops.push(OpCode::StateVarInit(1, b)); // 4: initialised twice
        let in_range = |start, end| {
            let mut seen = Vec::new();
            code.for_each_state_local_in_range(start, end, |i| seen.push(i));
            seen
        };
        assert_eq!(in_range(0, 5), vec![0, 1, 2]);
        assert_eq!(in_range(2, 5), vec![1, 2]);
        assert_eq!(in_range(1, 2), Vec::<usize>::new());
        assert_eq!(in_range(4, 5), vec![1]);
    }

    #[test]
    fn block_range_facts_are_indexed() {
        let mut code = CompiledCode::new();
        code.ops.push(OpCode::BlockLocalScope {
            body_end: 3,
            succeed_boundary: false,
        });
        code.ops
            .push(OpCode::StateVarInit(4, Symbol::intern("$n@1")));
        code.ops.push(OpCode::SetTopic);
        code.ops.push(OpCode::Pop);
        let facts = code.block_range_facts(0, 3);
        assert!(facts.state_slots.contains(&4));
        assert!(facts.owned_slots.is_empty());
        assert!(facts.binds_own_topic);
        assert!(matches!(facts, std::borrow::Cow::Borrowed(_)));
        // An ip that holds no scope op is answered by a scan of the range.
        let scanned = code.block_range_facts(2, 4);
        assert!(!scanned.binds_own_topic);
        assert!(matches!(scanned, std::borrow::Cow::Owned(_)));
    }

    #[test]
    fn stale_index_falls_back_to_scan() {
        let mut code = CompiledCode::new();
        label(&mut code, "A");
        assert_eq!(code.label_target("B"), None);
        label(&mut code, "B");
        assert_eq!(code.label_target("B"), Some(2));
    }
}
