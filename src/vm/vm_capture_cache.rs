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
//!
//! **Why arming also backs off.** "The previous capture saw the same chain" is
//! an *address* observation, and the allocator recycles addresses: a closure
//! created inside a sub or method frame runs against a fresh overlay map per
//! call, which malloc keeps handing back at the same address once the previous
//! frame's map is freed. `wants_arm` therefore says yes on that path — and the
//! entry can never be hit, because arming holds an `Arc` on that very map, so
//! the *next* frame is forced to allocate somewhere else. Left alone the memo
//! arms on every other creation and hits on none, paying an `Env` clone, a
//! `tier_maps` vector and the drop of the previous entry's ~24 captured values
//! for nothing: on a 20000-iteration `sub make($n) { my $c = { $n + 1 } }` loop
//! that was 4.5% of the program's instructions at a 0% hit rate. Arms that are
//! replaced without ever being hit are counted, and once a few in a row have
//! been wasted the memo stops arming, retrying only occasionally so a program
//! that changes shape can pick the memo back up.

use std::sync::Arc;

use crate::env::{Env, Tier, TierAddrs};
use crate::opcode::CompiledCode;

/// Consecutive wasted arms (armed, then replaced without a single hit) after
/// which the memo stops arming. Small: on a path where the memo can pay off at
/// all, the very next capture hits.
const ARM_BACKOFF_AFTER: u32 = 2;

/// While backed off, arming is retried once every this many captures, so a
/// program whose closure creation moves from a churning scope to a stable one
/// is not locked out for the rest of the run.
const ARM_RETRY_EVERY: u32 = 512;

/// One armed memo: the inputs it was built from, held, plus the result.
struct ArmedCapture {
    /// Every tier's overlay map, leaf first — see the module docs for why these
    /// are held rather than merely addressed.
    tiers: Vec<Arc<Tier>>,
    /// The closure chunk whose free-var / own-local sets shaped the filter.
    /// Held so `Arc::ptr_eq` against a later chunk cannot be fooled by a
    /// recycled allocation.
    code: Arc<CompiledCode>,
    /// The captured env `filtered_flat` produced for those inputs.
    env: Env,
    /// Whether this entry was ever handed back by [`CaptureCache::get`]. An
    /// entry replaced with this still `false` was pure overhead — see the
    /// module docs on backing off.
    hit: bool,
}

/// The one-entry closure-capture memo. See the module docs.
#[derive(Default)]
pub(crate) struct CaptureCache {
    armed: Option<ArmedCapture>,
    /// Inputs the *previous* capture ran against, as bare addresses: nothing is
    /// held, so this observation pins nothing and can only ever be used to
    /// decide whether arming is worth it.
    last: Option<(TierAddrs, usize)>,
    /// Consecutive arms that were replaced without ever being hit; see the
    /// module docs. Reset by any hit.
    wasted_arms: u32,
    /// Captures recorded since the memo went into backoff, modulo
    /// [`ARM_RETRY_EVERY`]: zero on the capture that is allowed to re-arm.
    backoff_tick: u32,
}

impl CaptureCache {
    /// The memoized capture for `(env, code)`, if this exact pair produced the
    /// armed entry. `addrs` is `env.tier_addrs()`, computed once by the caller
    /// because it needs it for [`Self::record`] too.
    pub(crate) fn get(
        &mut self,
        addrs: Option<TierAddrs>,
        code: &Arc<CompiledCode>,
    ) -> Option<&Env> {
        let addrs = addrs?;
        let armed = self.armed.as_mut()?;
        if !(Arc::ptr_eq(&armed.code, code) && addrs.matches(&armed.tiers)) {
            return None;
        }
        armed.hit = true;
        self.wasted_arms = 0;
        Some(&armed.env)
    }

    /// True when a capture that just ran against `(addrs, code)` is worth
    /// arming — i.e. the previous one ran against the same chain and chunk (see
    /// the module docs on why arming waits for a repeat). The caller passes the
    /// resulting `Env::tier_maps()` to [`Self::record`].
    pub(crate) fn wants_arm(&self, addrs: Option<TierAddrs>, code: &Arc<CompiledCode>) -> bool {
        if self.wasted_arms >= ARM_BACKOFF_AFTER && self.backoff_tick != 0 {
            return false;
        }
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
        tiers: Option<Vec<Arc<Tier>>>,
        captured: &Env,
    ) {
        self.last = addrs.map(|addrs| (addrs, Arc::as_ptr(code) as usize));
        // An entry being replaced without a single hit is an arm that cost an
        // `Env` clone, a `tier_maps` vector and (on drop) a refcount pass over
        // every captured value, and bought nothing.
        if self.armed.as_ref().is_some_and(|armed| !armed.hit) {
            self.wasted_arms = self.wasted_arms.saturating_add(1);
        }
        if self.wasted_arms >= ARM_BACKOFF_AFTER {
            self.backoff_tick = (self.backoff_tick + 1) % ARM_RETRY_EVERY;
        } else {
            self.backoff_tick = 0;
        }
        self.armed = tiers.map(|tiers| ArmedCapture {
            tiers,
            code: Arc::clone(code),
            env: captured.clone(),
            hit: false,
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::value::Value;

    fn chunk() -> Arc<CompiledCode> {
        Arc::new(CompiledCode::new())
    }

    /// One capture against `env`: the shape `capture_closure_env` runs — ask
    /// the memo, and on a miss record the freshly built result. Returns whether
    /// the memo answered.
    fn capture(cache: &mut CaptureCache, env: &Env, code: &Arc<CompiledCode>) -> bool {
        let addrs = env.tier_addrs();
        if cache.get(addrs, code).is_some() {
            return true;
        }
        let built = env.filtered_flat(&|_, _| true);
        let tiers = cache.wants_arm(addrs, code).then(|| env.tier_maps());
        cache.record(addrs, code, tiers, &built);
        false
    }

    #[test]
    fn a_stable_scope_still_arms_and_hits() {
        // The memo's whole point: repeated creations of the same literal from
        // an untouched scope reuse the map instead of rebuilding it. The
        // backoff must not get in the way of that.
        let mut root = Env::new();
        root.insert("a".into(), Value::int(1));
        let code = chunk();
        let mut cache = CaptureCache::default();

        assert!(!capture(&mut cache, &root, &code), "nothing armed yet");
        assert!(
            !capture(&mut cache, &root, &code),
            "arming waits for a repeat"
        );
        for _ in 0..8 {
            assert!(
                capture(&mut cache, &root, &code),
                "an unchanged scope keeps hitting"
            );
        }
        assert_eq!(cache.wasted_arms, 0, "a hit clears the wasted-arm count");
    }

    #[test]
    fn a_scope_that_never_hits_stops_arming() {
        // A closure created inside a sub/method frame runs against a fresh
        // overlay per call, which the allocator keeps handing back at the same
        // address once the previous one is freed — so `wants_arm` says yes and
        // the entry can never be hit, because arming pins that very address.
        // Simulated here by capturing from a fresh chain every time while
        // reporting the same tier addresses through a re-used, re-created env.
        let code = chunk();
        let mut cache = CaptureCache::default();
        let mut armed_at_least_once = false;
        for _ in 0..(ARM_BACKOFF_AFTER + 6) {
            let mut root = Env::new();
            root.insert("a".into(), Value::int(1));
            assert!(
                !capture(&mut cache, &root, &code),
                "a fresh chain can never hit"
            );
            armed_at_least_once |= cache.armed.is_some();
        }
        assert!(
            armed_at_least_once || cache.wasted_arms == 0,
            "either it armed (and then backed off) or it never armed at all"
        );
        // However many arms were wasted, the memo must not keep arming for the
        // rest of the run: once the count is past the limit, `wants_arm` is
        // false except on the periodic retry.
        if cache.wasted_arms >= ARM_BACKOFF_AFTER {
            let root = Env::new();
            let addrs = root.tier_addrs();
            cache.last = addrs.map(|a| (a, Arc::as_ptr(&code) as usize));
            assert!(
                !cache.wants_arm(addrs, &code),
                "a memo that never pays off stops arming"
            );
        }
    }

    #[test]
    fn backoff_retries_periodically() {
        // A program whose closure creation moves from a churning scope to a
        // stable one must be able to pick the memo back up.
        let code = chunk();
        let mut cache = CaptureCache {
            wasted_arms: ARM_BACKOFF_AFTER + 1,
            ..Default::default()
        };
        let root = Env::new();
        let addrs = root.tier_addrs();
        cache.last = addrs.map(|a| (a, Arc::as_ptr(&code) as usize));

        let mut allowed = 0usize;
        for _ in 0..(ARM_RETRY_EVERY * 2) {
            if cache.wants_arm(addrs, &code) {
                allowed += 1;
            }
            cache.record(addrs, &code, None, &Env::new());
        }
        assert!(
            (1..=3).contains(&allowed),
            "retried a handful of times over {} captures, not never and not always (got {allowed})",
            ARM_RETRY_EVERY * 2
        );
    }
}
