//! Reuse of the closure-capture env across repeated creations of the same
//! closure literal.
//!
//! `Interpreter::capture_closure_env` builds the closure's captured env with
//! `Env::filtered_flat`, which walks every visible env key and inserts the kept
//! ones into a brand-new map. That is O(kept env) *per closure creation*, and
//! the kept set is dominated by names no closure body varies: the built-in
//! dynamics (`$*IN`, `$*OUT`, `$*CWD`, `%*ENV`, `@*ARGS`, ...) seeded once at
//! startup, `?FILE`, and `Any`. A trivial mainline script captures 23 such
//! entries per closure literal, and `filtered_flat` was 41% of the instructions
//! of a 200000-iteration `my $c = * + 1;` loop.
//!
//! The result is a pure function of the visible env contents and the closure's
//! own `CompiledCode` (the filter reads only the key, never the value), so a
//! creation whose inputs are unchanged since the last one can hand back the
//! same map. This module holds that one-entry memo.
//!
//! **Why an address comparison is sound here.** An armed entry holds an `Arc`
//! on every tier's overlay map. That does two things at once: the allocations
//! cannot be freed and recycled at the same address, and the extra strong count
//! makes `Env::cow_mut`'s `Arc::make_mut` clone the map before *any* by-name
//! write — so a tier that is written to necessarily moves to a new address.
//! Matching addresses therefore prove the contents are the ones the cached env
//! was built from. A write *through* a captured `ContainerRef` cell mutates
//! neither map and is deliberately not a mismatch: the cached env holds the
//! same cell, exactly as a freshly built capture would.
//!
//! **Why arming waits for a repeat.** Holding those `Arc`s costs a
//! copy-on-write clone on the next write to a pinned tier. In a loop that
//! writes the env by name on every iteration the memo could never hit anyway,
//! so an entry is armed only after two consecutive captures have seen the same
//! chain: a churning env is tracked with plain addresses (nothing held, nothing
//! pinned) and never pays for a memo that cannot pay off.

use std::sync::Arc;

use crate::env::{Env, SymMap, TierAddrs};
use crate::opcode::CompiledCode;

/// One armed memo: the inputs it was built from, held, plus the result.
struct ArmedCapture {
    /// Every tier's overlay map, leaf first — see the module docs for why these
    /// are held rather than merely addressed.
    tiers: Vec<Arc<SymMap>>,
    /// The closure chunk whose free-var / own-local sets shaped the filter.
    /// Held so `Arc::ptr_eq` against a later chunk cannot be fooled by a
    /// recycled allocation.
    code: Arc<CompiledCode>,
    /// The captured env `filtered_flat` produced for those inputs.
    env: Env,
}

/// The one-entry closure-capture memo. See the module docs.
#[derive(Default)]
pub(crate) struct CaptureCache {
    armed: Option<ArmedCapture>,
    /// Inputs the *previous* capture ran against, as bare addresses: nothing is
    /// held, so this observation pins nothing and can only ever be used to
    /// decide whether arming is worth it.
    last: Option<(TierAddrs, usize)>,
}

impl CaptureCache {
    /// The memoized capture for `(env, code)`, if this exact pair produced the
    /// armed entry. `addrs` is `env.tier_addrs()`, computed once by the caller
    /// because it needs it for [`Self::record`] too.
    pub(crate) fn get(&self, addrs: Option<TierAddrs>, code: &Arc<CompiledCode>) -> Option<&Env> {
        let addrs = addrs?;
        let armed = self.armed.as_ref()?;
        (Arc::ptr_eq(&armed.code, code) && addrs.matches(&armed.tiers)).then_some(&armed.env)
    }

    /// True when a capture that just ran against `(addrs, code)` is worth
    /// arming — i.e. the previous one ran against the same chain and chunk (see
    /// the module docs on why arming waits for a repeat). The caller passes the
    /// resulting `Env::tier_maps()` to [`Self::record`].
    pub(crate) fn wants_arm(&self, addrs: Option<TierAddrs>, code: &Arc<CompiledCode>) -> bool {
        let (Some(addrs), Some((last_addrs, last_code))) = (addrs, &self.last) else {
            return false;
        };
        *last_addrs == addrs && *last_code == Arc::as_ptr(code) as usize
    }

    /// Record a freshly built capture, arming the memo with `tiers` when
    /// [`Self::wants_arm`] said so. Disarms otherwise, so a stale entry never
    /// keeps tiers pinned once the env starts churning.
    pub(crate) fn record(
        &mut self,
        addrs: Option<TierAddrs>,
        code: &Arc<CompiledCode>,
        tiers: Option<Vec<Arc<SymMap>>>,
        captured: &Env,
    ) {
        self.last = addrs.map(|addrs| (addrs, Arc::as_ptr(code) as usize));
        self.armed = tiers.map(|tiers| ArmedCapture {
            tiers,
            code: Arc::clone(code),
            env: captured.clone(),
        });
    }

    /// GC roots: an armed entry holds a whole captured env of live values, plus
    /// an `Arc` on each source tier's map — which outlives the env it was taken
    /// from (that is the point), so its values are roots of their own.
    pub(crate) fn visit_roots(&self, visitor: &mut dyn crate::gc::RootVisitor) {
        let Some(armed) = &self.armed else {
            return;
        };
        armed.env.visit_values(visitor);
        for tier in &armed.tiers {
            for value in tier.values() {
                visitor.visit_value(value);
            }
        }
    }
}
