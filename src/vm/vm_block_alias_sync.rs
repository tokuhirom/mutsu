//! A bare block's exit re-syncs `:=` alias targets with the frame's locals.
//!
//! A local with a sigilless alias (`my $a := $_`) that the block modified must
//! propagate its current value to the alias target in env, so the two stay in
//! sync. Only a local whose alias key is in some env can have one, and those
//! keys are the ones `alias_key_log` holds, so the exit visits their slots
//! instead of asking every local of the frame (#9170).

use crate::opcode::CompiledCode;
use crate::runtime::Interpreter;
use crate::value::{Value, ValueView};

impl Interpreter {
    // Cost: O(a + s), a = alias keys ever bound in the process, s = slots of
    // this chunk carrying one of them; O(1) for a program that never bound one.
    pub(super) fn sync_block_exit_alias_targets(&mut self, code: &CompiledCode) {
        if !crate::env::sigilless_meta_keys_possible() {
            return;
        }
        let mut slots: Vec<u32> = Vec::new();
        crate::alias_key_log::for_each(|alias| {
            debug_assert!(
                crate::symbol::maybe_env_key(alias),
                "a logged alias key has been an env key"
            );
            slots.extend_from_slice(code.alias_slots_of(alias));
        });
        if slots.is_empty() {
            return;
        }
        // Slot order, as the per-local scan this replaces visited them: two
        // locals aliasing one target must write it in the same order.
        slots.sort_unstable();
        slots.dedup();
        for slot in slots {
            let idx = slot as usize;
            let Some(alias_key) = code.alias_sym(idx) else {
                continue;
            };
            if let Some(ValueView::Str(target)) = self
                .env()
                .get_sym(alias_key)
                .cloned()
                .as_ref()
                .map(Value::view)
            {
                let local_val = self.locals[idx].clone();
                let differs = self
                    .env()
                    .get(target.as_str())
                    .is_some_and(|env_val| &local_val != env_val);
                if differs {
                    self.env_mut().insert(target.to_string(), local_val);
                }
            }
        }
    }
}
