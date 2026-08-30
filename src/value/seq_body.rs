//! `Seq`'s reification/consumption body (docs/adr/0034).
//!
//! rakudo's `Seq` has **two** primitive operations mutsu used to conflate into
//! one: `.cache` reifies the iterator **into the Seq**, idempotently, and
//! leaves every alias of the value able to read it again; `.iterator`/`.list`
//! *steal* the source and hand it away once. `SeqBody` gives mutsu that same
//! split. See the ADR for the full design and the measured raku-vs-mutsu
//! consumption matrix this module implements ([`seq_method_consumes`]).
//!
//! # The generation graveyard
//!
//! Reifying a `SeqBody` under a shared `&self` (every alias of a `Value::Seq`
//! shares one `Arc<SeqBody>`, so there is never an exclusive `&mut` to reify
//! through) needs the same [`SyncUnsafeCell`] + generation-graveyard technique
//! `NativeBacking` uses for the native-array decode cache (docs/adr/0030 §2.2,
//! `src/value/native_backing.rs`) — see that module's docs for why the
//! graveyard (rather than an audited overwrite) is what makes the write sound.
//! A `Seq` body reifies **at most once** (seed -> reified), so the graveyard
//! here never grows past two entries.

use super::sync_cell::SyncUnsafeCell;
use super::{RuntimeError, Value};
use std::sync::{Arc, Mutex};

/// What a `Seq` still has to do to produce its elements.
#[derive(Clone)]
pub(crate) enum SeqSource {
    /// Already reified — the common case (`Value::seq(vec)` and every other
    /// eagerly-built `Seq`).
    Reified,
    /// `Seq.new($iterator)`: pull `pull-one` until `IterationEnd`.
    Iterator(Value),
    /// `IO::Handle.lines` / `.words` (formerly `ValueRepr::LazyIoLines`).
    IoLines {
        handle: Value,
        words: bool,
        kv: bool,
    },
    /// The source was handed away by a consuming method (`.iterator`,
    /// `.list`, ...). A later attempt to reify or take again throws
    /// `X::Seq::Consumed`.
    Taken,
}

struct SeqState {
    source: SeqSource,
    /// `.cache` was called: the *next* pull (from whichever method needs it
    /// first) stores into `gens` instead of handing the source away, and
    /// every later touch — including a nominally-consuming one — is served
    /// from the stored elements instead of erroring. Deliberately does NOT
    /// pull eagerly (`.cache` itself is lazy in rakudo — see the ADR §1.4
    /// oracle and the module tests), so this is a flag, not an eager reify.
    cache_requested: bool,
    /// `Seq.from-loop` without a stopping condition: a genuinely infinite
    /// source, so a strict force (`.List`, `.elems`, ...) must throw
    /// `X::Cannot::Lazy` instead of hanging.
    lazy: bool,
    /// The `:batch`/`:degree` a `HyperSeq`/`RaceSeq` was created with, read
    /// back by `.configuration`. `None` means unspecified (report defaults).
    hyper: Option<(Option<i64>, Option<i64>)>,
    /// `HyperSeq`/`RaceSeq` only: whether `.iterator` has already been
    /// claimed once (rakudo #4413 — a SECOND `.iterator` throws even though
    /// the body is fully reified from birth, unlike a plain `Seq` where a
    /// reified body always serves `.iterator` repeatably). Orthogonal to
    /// `source`/`cache_requested`, which is why it is its own flag rather
    /// than routed through `take`.
    hyper_iterator_claimed: bool,
    /// A `for`-loop's own single-use gate — see [`SeqBody::claim_single_use_once`]
    /// for why `for` needs its own flag rather than routing through `take`
    /// like every other `seq_method_consumes` entry (raku exempts `for` on a
    /// `$`-sigiled Scalar-contained Seq from consuming at all, which mutsu
    /// does not yet distinguish from a sigilless/argument-passed one — see
    /// that method's doc comment for the measured raku evidence).
    single_use_claimed: bool,
    /// Whether this body has already been served once by a "keeps" touch
    /// ([`SeqBody::reify`]) — either a genuine pull, or reading a body that
    /// was already `Reified` at birth ([`SeqBody::reified`], the common case:
    /// `Value::seq(vec)` and every other eager Seq constructor). Once
    /// `retained` (or `cache_requested`), `take` never steals: rakudo's
    /// model is that EVERY Seq — even one built from a fully-known literal
    /// list — wraps a single-use iterator by default (`my $s = (1,2,3).Seq;
    /// $s.List; $s.List` throws `X::Seq::Consumed` on the SECOND `.List`,
    /// measured against raku directly), and only a prior non-consuming touch
    /// (`.Str`, `.gist`, `.elems`, ...) or an explicit `.cache` earns a Seq
    /// its "durable" behavior. `SeqBody::reified` therefore does NOT itself
    /// set this — only an actual [`SeqBody::reify`] call does, so a body
    /// nobody has read yet still steals on its first `take`.
    retained: bool,
    /// A plain `$scalar = SEQ` (or `my $scalar = SEQ`) assignment itemized
    /// this body into a Scalar container (`SeqBody::mark_itemized`'s call
    /// site, `vm_var_assign_set_local.rs`). Raku's `sink` never forces an
    /// itemized value — sinking a bare `$s;` after `my $s = (gather die)[];`
    /// only warns "Useless use of $s in sink context" and does NOT run the
    /// `gather`'s body (measured against raku); only a genuinely un-itemized
    /// Seq (returned bare, with no assignment, e.g. `sub f { gather { ... }
    /// } }; f();`) is forced by `sink`. Once a body is itemized this way the
    /// exemption travels with it through any later read/return — including
    /// back out through a routine or closure call whose OWN result is then
    /// sunk by ITS caller (`sub call-it(&c) { c() }; call-it({ my $s = ... })
    /// ;` — `c()`'s discarded return must not force the `$s` the closure
    /// itemized), because it is the same shared `Arc<SeqBody>` throughout.
    itemized: bool,
}

/// Outcome of [`SeqBody::take`]: whether the caller may treat the Seq as
/// still usable afterward.
pub(crate) enum SeqTaken {
    /// The body is retained — the elements returned may be read again later
    /// through [`SeqBody::reify`] (already reified, or `.cache` flipped this
    /// touch from stealing to storing).
    Served,
    /// The source was handed away once; the body is now [`SeqSource::Taken`].
    Taken,
}

/// Which Raku type a `SeqBody` **handle** (a particular `Value`) presents as
/// (docs/adr/0038 S2). Deliberately NOT a field inside the shared,
/// mutex-guarded [`SeqState`]: `.cache` on a not-yet-reified `Seq` must
/// return a `List`-typed value while the ORIGINAL `Seq` value stays a `Seq`
/// (measured against `raku` — `my $s = Seq.new(...); my $c = $s.cache; say
/// $s.^name, " ", $c.^name` prints `Seq List`), so the two views must be able
/// to live at once over the SAME reification/consumption state. See
/// [`SeqBody::as_list_view`].
#[derive(Clone, Copy, PartialEq, Eq, Debug, Default)]
pub(crate) enum SeqView {
    #[default]
    Seq,
    List,
    /// A `List`-view handle stored in a `$` Scalar container. This stays on
    /// the handle (not the shared core), just like `ArrayKind::ItemList`, so
    /// itemizing the cached handle cannot change the original `Seq` handle.
    ItemList,
}

/// The shared reification/consumption machinery a `Seq`, `HyperSeq`, or
/// `RaceSeq` body owns. Behind its own `Arc` (inside [`SeqBody::core`]) so
/// that two [`SeqBody`] handles with different [`SeqView`]s — e.g. a `Seq`
/// and the `List` view its `.cache` returned — can share ONE copy of this
/// state: a `retained`/`Taken` transition made through either handle must be
/// visible through both (docs/adr/0038 S5).
struct SeqCore {
    #[allow(clippy::vec_box)]
    gens: SyncUnsafeCell<Vec<Box<Vec<Value>>>>,
    state: Mutex<SeqState>,
}

/// The reification/consumption state of a `Seq`, `HyperSeq`, or `RaceSeq`.
/// See the module docs for the generation graveyard `gens` relies on.
pub(crate) struct SeqBody {
    core: Arc<SeqCore>,
    view: SeqView,
}

impl std::fmt::Debug for SeqBody {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // `SyncUnsafeCell` is deliberately not `Debug` (see `native_backing.rs`);
        // show only what a shared borrow can read safely (the live generation,
        // via `Deref`).
        f.debug_struct("SeqBody")
            .field("elements", &**self)
            .field("view", &self.view)
            .finish_non_exhaustive()
    }
}

impl std::ops::Deref for SeqBody {
    type Target = Vec<Value>;
    /// A read that arrives before reification sees the empty seed, exactly
    /// as mutsu's plain `Arc<Vec<Value>>` Seq did — reification is triggered
    /// by the dispatch sites (`reify`/`take`), never by a read, so this stays
    /// allocation-free and cannot re-enter the VM.
    fn deref(&self) -> &Vec<Value> {
        self.live_generation()
    }
}

impl SeqBody {
    /// Build an already-reified body (the common case: `Value::seq(vec)` and
    /// every eager Seq/HyperSeq/RaceSeq constructor).
    pub(crate) fn reified(items: Vec<Value>) -> Arc<Self> {
        Arc::new(SeqBody {
            core: Arc::new(SeqCore {
                gens: SyncUnsafeCell::new(vec![Box::new(items)]),
                state: Mutex::new(SeqState {
                    source: SeqSource::Reified,
                    cache_requested: false,
                    lazy: false,
                    hyper: None,
                    hyper_iterator_claimed: false,
                    single_use_claimed: false,
                    retained: false,
                    itemized: false,
                }),
            }),
            view: SeqView::Seq,
        })
    }

    /// Build a body whose elements are not yet available (`Seq.new($iterator)`,
    /// `IO::Handle.lines`, or a pre-consumed `Seq.new()`).
    pub(crate) fn deferred(source: SeqSource) -> Arc<Self> {
        Arc::new(SeqBody {
            core: Arc::new(SeqCore {
                gens: SyncUnsafeCell::new(Vec::new()),
                state: Mutex::new(SeqState {
                    source,
                    cache_requested: false,
                    lazy: false,
                    hyper: None,
                    hyper_iterator_claimed: false,
                    single_use_claimed: false,
                    retained: false,
                    itemized: false,
                }),
            }),
            view: SeqView::Seq,
        })
    }

    /// A second handle over the SAME reification/consumption core, presenting
    /// as [`SeqView::List`] — what `.cache` (docs/adr/0038 phase 3) and
    /// `.List` return for a Seq whose source is not yet reified. Does NOT
    /// pull or clone any elements: `gens`/`state` (via `core`) stay the exact
    /// same shared object, so a `retained`/`cache_requested`/`Taken`
    /// transition made through either handle is visible through the other
    /// (docs/adr/0038 S5) — only the type-facing `view` tag differs.
    pub(crate) fn as_list_view(self: &Arc<Self>) -> Arc<Self> {
        Arc::new(SeqBody {
            core: Arc::clone(&self.core),
            view: SeqView::List,
        })
    }

    /// Itemize a List-view handle without touching its deferred source.
    pub(crate) fn as_item_list_view(self: &Arc<Self>) -> Arc<Self> {
        Arc::new(SeqBody {
            core: Arc::clone(&self.core),
            view: SeqView::ItemList,
        })
    }

    /// Which Raku type this handle presents as — read by `value_type_name`
    /// (docs/adr/0038 S2), the single oracle for "what type is this value".
    pub(crate) fn view(&self) -> SeqView {
        self.view
    }

    /// The current generation's elements — `pub(crate)` (rather than
    /// private) so `seq_body_shapes.rs`'s Miri probes can take a reference
    /// directly, without routing through `Deref` (which coerces through
    /// `Arc<SeqBody>` and trips rustc's `invalid_reference_casting` lint
    /// when the result is later cast to a raw pointer for the
    /// retired-generation probe — see that module for the full shape).
    pub(crate) fn live_generation(&self) -> &Vec<Value> {
        // SAFETY: this only ever reads the graveyard. `pull_and_store` is the
        // sole writer and never overwrites an existing slot (module docs), so
        // a reference into any generation — including this one — stays valid
        // across a later push.
        static EMPTY: Vec<Value> = Vec::new();
        unsafe { &*self.core.gens.get() }
            .last()
            .map(|b| b.as_ref())
            .unwrap_or(&EMPTY)
    }

    /// Pull `source` via `pull` and store the result as a fresh generation.
    /// Fails (leaving the body `Taken`) if `pull` errors — a failed pull is
    /// not retried. No-ops when `source` is already `Reified` — the elements
    /// are already in `gens`, nothing to pull.
    fn pull_and_store(
        &self,
        pull: impl FnOnce(&SeqSource) -> Result<Vec<Value>, RuntimeError>,
    ) -> Result<(), RuntimeError> {
        let source = {
            let mut state = self.core.state.lock().unwrap();
            if matches!(state.source, SeqSource::Reified) {
                return Ok(());
            }
            std::mem::replace(&mut state.source, SeqSource::Taken)
        };
        let items = pull(&source)?;
        // SAFETY: the shape `SyncUnsafeCell` exists for — a write under the
        // shared `&self` every alias of this `Arc<SeqBody>` keeps using
        // afterward. No reference into `gens` is held across this push:
        // `live_generation` never keeps its `&Vec<Value>` past its own call,
        // and previously-returned references point at earlier, never-
        // rewritten slots (module docs).
        unsafe { (*self.core.gens.get()).push(Box::new(items)) };
        self.core.state.lock().unwrap().source = SeqSource::Reified;
        Ok(())
    }

    /// rakudo's `.cache`, and every other non-consuming touch that needs the
    /// elements: pull the source exactly once (idempotent) and store the
    /// elements in THIS body, so every alias reads them back forever after.
    /// This is the ONLY thing that marks a body `retained` (see that field's
    /// doc comment) — a reifying touch, unlike a consuming one, earns the
    /// Seq its durable behavior. Fails with `X::Seq::Consumed` if the source
    /// was already handed away by a consuming method.
    pub(crate) fn reify(
        &self,
        pull: impl FnOnce(&SeqSource) -> Result<Vec<Value>, RuntimeError>,
    ) -> Result<&Vec<Value>, RuntimeError> {
        {
            let mut state = self.core.state.lock().unwrap();
            match &state.source {
                SeqSource::Reified => {
                    state.retained = true;
                    return Ok(self.live_generation());
                }
                SeqSource::Taken => return Err(super::seq_consumed_error()),
                _ => {}
            }
        }
        self.pull_and_store(pull)?;
        self.core.state.lock().unwrap().retained = true;
        Ok(self.live_generation())
    }

    /// rakudo's `.List`/`.list`/`.sort`/`.map`/`.iterator`/`.skip`/... (the
    /// whole of [`seq_method_consumes`]): produce the element vector,
    /// stealing the body's single read UNLESS it is exempt — `.cache` was
    /// requested, or an earlier NON-consuming touch already [`reify`]d it
    /// ([`SeqState::retained`]). Fails with `X::Seq::Consumed` if the body
    /// was already taken (by this or an earlier steal).
    ///
    /// **A body built already-`Reified`** (`Value::seq(vec)`, the common
    /// case) is NOT automatically exempt: rakudo's `Seq` always wraps a
    /// single-use iterator by default, even over a fully-known literal list
    /// — `my $s = (1,2,3).Seq; $s.List; $s.List` throws `X::Seq::Consumed`
    /// on the SECOND `.List`, measured directly against raku. So the first
    /// `take` on a fresh, never-`reify`d `Reified` body steals too (moving
    /// the elements already sitting in `gens` out, rather than pulling —
    /// there is no separate source to pull from).
    ///
    /// [`reify`]: SeqBody::reify
    pub(crate) fn take(
        &self,
        pull: impl FnOnce(&SeqSource) -> Result<Vec<Value>, RuntimeError>,
    ) -> Result<(Vec<Value>, SeqTaken), RuntimeError> {
        let (reified, servable) = {
            let state = self.core.state.lock().unwrap();
            match &state.source {
                SeqSource::Reified => (true, state.cache_requested || state.retained),
                SeqSource::Taken => return Err(super::seq_consumed_error()),
                _ => (false, state.cache_requested),
            }
        };
        if servable {
            self.pull_and_store(pull)?;
            return Ok((self.live_generation().clone(), SeqTaken::Served));
        }
        if reified {
            let mut state = self.core.state.lock().unwrap();
            state.source = SeqSource::Taken;
            drop(state);
            return Ok((self.live_generation().clone(), SeqTaken::Taken));
        }
        let source = {
            let mut state = self.core.state.lock().unwrap();
            std::mem::replace(&mut state.source, SeqSource::Taken)
        };
        let items = pull(&source)?;
        Ok((items, SeqTaken::Taken))
    }

    /// A `for`-loop's own single-use gate (`vm_for_loop_dispatch.rs`):
    /// unlike every other consumer, `for` does NOT go through `take` at all
    /// (it is not a `seq_method_consumes` entry — reading a bare `for $s {}`
    /// leaves `$s` fully reusable afterward, including for a further
    /// `.List`/`.sort`/..., measured directly against raku), but a SECOND
    /// `for`-loop over the exact same Seq value still throws
    /// `X::Seq::Consumed` (also measured). This one-shot gate is what
    /// enforces that second half without disturbing `take`/`reify`'s own
    /// bookkeeping — `for`'s own data read still goes through the ordinary
    /// non-consuming `reify` path (via `reify_or_consume_seq_target`, method
    /// `"for"`, which is not in `seq_method_consumes`) and marks the body
    /// `retained` like any other reifying touch.
    pub(crate) fn claim_single_use_once(&self) -> Result<(), RuntimeError> {
        let mut state = self.core.state.lock().unwrap();
        if state.single_use_claimed && !state.cache_requested {
            return Err(super::seq_consumed_error());
        }
        state.single_use_claimed = true;
        Ok(())
    }

    /// rakudo's `sink`: run the source to completion for side effects and
    /// discard the result, UNLESS the body is exempt — `.cache` was
    /// requested (`.cache` is itself lazy, so sinking a cache-requested Seq
    /// stays lazy too: measured, `$s.cache; $s.sink` does not run the
    /// source, only a later real read does), or an earlier reifying touch
    /// already `retained` it (`$s.gist; $s.sink` does not consume either).
    /// **A fresh, never-`reify`d `Reified` body IS consumed** — same as
    /// every other `seq_method_consumes` entry (measured: `(1,2,3).Seq.sink`
    /// then `.List` throws `X::Seq::Consumed`) — even though there is
    /// nothing to pull, since the elements were already sitting in `gens`.
    /// Re-sinking an ALREADY-taken body is a harmless no-op (unlike every
    /// other method, which throws on an already-consumed Seq): measured,
    /// `.sink`ing an uncached Seq twice does not throw the second time.
    pub(crate) fn sink(
        &self,
        pull: impl FnOnce(&SeqSource) -> Result<Vec<Value>, RuntimeError>,
    ) -> Result<(), RuntimeError> {
        let source = {
            let mut state = self.core.state.lock().unwrap();
            match &state.source {
                SeqSource::Taken => return Ok(()),
                _ if state.itemized => return Ok(()),
                SeqSource::Reified if state.cache_requested || state.retained => {
                    return Ok(());
                }
                _ if state.cache_requested => return Ok(()),
                _ => std::mem::replace(&mut state.source, SeqSource::Taken),
            }
        };
        if matches!(source, SeqSource::Iterator(_) | SeqSource::IoLines { .. }) {
            pull(&source)?;
        }
        Ok(())
    }

    /// `.cache`: flip this body from "consume once" to "reify and keep" for
    /// every future touch. Deliberately does not pull now (see
    /// `cache_requested`'s doc comment).
    pub(crate) fn mark_cache_requested(&self) {
        self.core.state.lock().unwrap().cache_requested = true;
    }

    /// A plain scalar assignment (`$s = SEQ` / `my $s = SEQ`) itemized this
    /// body — see `itemized`'s doc comment for why that permanently exempts
    /// it from `sink`'s forcing.
    pub(crate) fn mark_itemized(&self) {
        self.core.state.lock().unwrap().itemized = true;
    }

    /// Whether `.cache` was requested, or the body is already reified — the
    /// two conditions under which a later touch is non-destructive.
    pub(crate) fn is_cached(&self) -> bool {
        let state = self.core.state.lock().unwrap();
        state.cache_requested || matches!(state.source, SeqSource::Reified)
    }

    pub(crate) fn mark_lazy(&self) {
        self.core.state.lock().unwrap().lazy = true;
    }

    pub(crate) fn is_lazy(&self) -> bool {
        self.core.state.lock().unwrap().lazy
    }

    /// Whether the source has already been handed away (and never reified in
    /// between) — a later `reify`/`take` on this body will throw.
    pub(crate) fn is_consumed(&self) -> bool {
        matches!(self.core.state.lock().unwrap().source, SeqSource::Taken)
    }

    /// Whether this body still has a source to pull from (an unreified
    /// `Seq.new($iterator)` or `IO::Handle.lines`) rather than being already
    /// reified or already taken.
    pub(crate) fn has_deferred_source(&self) -> bool {
        matches!(
            self.core.state.lock().unwrap().source,
            SeqSource::Iterator(_) | SeqSource::IoLines { .. }
        )
    }

    /// Non-destructive peek at a not-yet-touched `IoLines` source's parts
    /// (`None` for every other shape, including an already-taken/reified
    /// one). Used ONLY as a read-only self-check — by
    /// `builtins/methods_0arg/mod.rs`'s native `"kv"` dispatch, to recognize
    /// "this Seq IS ALREADY the `kv`-transformed marker
    /// `reify_or_consume_seq_target`'s `"kv"` special case built" and pass it
    /// through unchanged instead of re-running `.kv`'s positional-index
    /// transform on its (still-empty) elements. The actual `.kv` TRANSFORM
    /// uses the destructive `claim_io_lines_for_streaming` instead (it must
    /// also mark the ORIGINAL body `Taken`, matching `.kv`'s normal consuming
    /// contract) — this accessor exists so that self-check can happen from a
    /// context (`native_method_0arg`) with no `&mut Interpreter` to justify
    /// a state-mutating call, and where mutating would be wrong anyway (the
    /// native fast path may be probed more than once for the same call).
    pub(crate) fn peek_io_lines_parts(&self) -> Option<(Value, bool, bool)> {
        match &self.core.state.lock().unwrap().source {
            SeqSource::IoLines { handle, words, kv } => Some((handle.clone(), *words, *kv)),
            _ => None,
        }
    }

    /// Whether this body's (not-yet-pulled) source is specifically an
    /// `IO::Handle.lines`/`.words` read — narrower than `has_deferred_source`,
    /// for the handful of call sites that historically special-cased only
    /// that flavour (formerly `ValueView::LazyIoLines`) and left a plain
    /// `Seq.new($iterator)` alone.
    pub(crate) fn is_io_lines_source(&self) -> bool {
        matches!(
            self.core.state.lock().unwrap().source,
            SeqSource::IoLines { .. }
        )
    }

    /// Whether a dispatch site needs to call `reify`/`take` at all before
    /// reading/consuming this body — `false` only once the body is BOTH
    /// `Reified` (the data is actually sitting in `gens`) AND permanently
    /// durable (`retained` by an earlier reifying touch, or `.cache` was
    /// requested), so THAT path stays a cheap state check with no further
    /// work. `true` covers "needs a pull" (`Iterator`/`IoLines` — even with
    /// `cache_requested` already set: that flag only changes what a FUTURE
    /// pull does, it is not itself a pull), "already taken" (so the caller's
    /// `reify`/`take` throws `X::Seq::Consumed`), AND a fresh, never-touched
    /// `Reified` body — that last one still needs routing through `take` on
    /// a `seq_method_consumes` method, since rakudo's `Seq` is single-use by
    /// default even when its data was fully known at birth (see
    /// `SeqBody::take`'s doc comment).
    pub(crate) fn needs_touch(&self) -> bool {
        let state = self.core.state.lock().unwrap();
        !(matches!(state.source, SeqSource::Reified) && (state.retained || state.cache_requested))
    }

    /// Bounded partial read for an `IoLines` source only
    /// (`vm_var_index_ops.rs`'s subscript special case): pull only enough
    /// additional records to reach `needed` total, leaving the handle open
    /// (and the source still `IoLines`) unless `pull_n` reports EOF. A body
    /// that is already `Reified`/`Taken`, or whose deferred source is not
    /// `IoLines` (a plain `Seq.new($iterator)`), is left completely
    /// untouched — the caller falls back to a full [`SeqBody::reify`] for
    /// those. This is what lets `words($fh, :close)[1, 2]` read only the
    /// first two words and leave `:close`'s auto-close from firing (it only
    /// fires when a read actually hits EOF — see `read_word_from_handle_value`).
    pub(crate) fn pull_io_lines_prefix(
        &self,
        needed: usize,
        pull_n: impl FnOnce(&Value, bool, usize) -> Result<(Vec<Value>, bool), RuntimeError>,
    ) -> Result<(), RuntimeError> {
        let (handle, words) = {
            let state = self.core.state.lock().unwrap();
            match &state.source {
                // kv-mode IoLines never occurs at any construction site today
                // (`Value::lazy_io_lines` is always called with `kv: false`);
                // skip the bounded path for it rather than getting the
                // interleaved indices wrong, and let the caller fully reify.
                SeqSource::IoLines {
                    handle,
                    words,
                    kv: false,
                } => {
                    if self.live_generation().len() >= needed {
                        return Ok(());
                    }
                    (handle.clone(), *words)
                }
                _ => return Ok(()),
            }
        };
        let have = self.live_generation().len();
        let (new_items, exhausted) = pull_n(&handle, words, needed - have)?;
        if new_items.is_empty() && !exhausted {
            return Ok(());
        }
        let mut combined = self.live_generation().clone();
        combined.extend(new_items);
        // SAFETY: same reasoning as `pull_and_store` — no reference into
        // `gens` is held across this push, and earlier generations (still
        // reachable through outstanding `&Vec<Value>` borrows) are never
        // rewritten, only superseded by a longer one.
        unsafe { (*self.core.gens.get()).push(Box::new(combined)) };
        if exhausted {
            self.core.state.lock().unwrap().source = SeqSource::Reified;
        }
        Ok(())
    }

    pub(crate) fn set_hyper_config(&self, batch: Option<i64>, degree: Option<i64>) {
        self.core.state.lock().unwrap().hyper = Some((batch, degree));
    }

    pub(crate) fn hyper_config(&self) -> Option<(Option<i64>, Option<i64>)> {
        self.core.state.lock().unwrap().hyper
    }

    /// `for`-loop streaming special case (`vm_for_loop_dispatch.rs`): iterate
    /// an `IO::Handle.lines`/`.words` source one line at a time (so
    /// `$fh.tell` reflects the current read position inside the loop body)
    /// instead of batch-reifying through `reify`/`take`. Claims the source
    /// (transitions to `Taken`) and returns its parts ONLY for a genuine,
    /// not-yet-cached `IoLines` source; `Ok(None)` for every other shape
    /// (already reified, an `Iterator` source, or `.cache` requested — those
    /// fall back to ordinary batch reification). `Err` if the body was
    /// already taken by an earlier consuming touch.
    #[allow(clippy::type_complexity)]
    pub(crate) fn claim_io_lines_for_streaming(
        &self,
    ) -> Result<Option<(Value, bool, bool)>, RuntimeError> {
        let mut state = self.core.state.lock().unwrap();
        match &state.source {
            SeqSource::Taken => Err(super::seq_consumed_error()),
            SeqSource::IoLines { .. } if !state.cache_requested => {
                let SeqSource::IoLines { handle, words, kv } =
                    std::mem::replace(&mut state.source, SeqSource::Taken)
                else {
                    unreachable!("matched IoLines above")
                };
                Ok(Some((handle, words, kv)))
            }
            _ => Ok(None),
        }
    }

    /// `HyperSeq`/`RaceSeq.iterator` (rakudo #4413): claim the single
    /// allowed iterator, atomically. `Err` if already claimed.
    pub(crate) fn claim_hyper_iterator_once(&self) -> Result<(), RuntimeError> {
        let mut state = self.core.state.lock().unwrap();
        if state.hyper_iterator_claimed {
            return Err(super::seq_consumed_error());
        }
        state.hyper_iterator_claimed = true;
        Ok(())
    }

    /// Every `Value` edge this body retains — used only by `Trace`, which
    /// must never pull (a pull mid-collect would re-enter allocation/the VM
    /// from inside the collector; same reasoning as `NativeBacking::trace_edges`).
    pub(crate) fn trace_edges(&self, visit: &mut dyn FnMut(&crate::gc::ErasedGc)) {
        // SAFETY: read-only. GC trace runs at a collect safepoint, never
        // concurrently with a `pull_and_store` on this same body (mirrors
        // `NativeBacking::trace_edges`'s reasoning).
        let gens = unsafe { &*self.core.gens.get() };
        for generation in gens.iter() {
            for v in generation.iter() {
                v.gc_trace(visit);
            }
        }
        let state = self.core.state.lock().unwrap();
        match &state.source {
            SeqSource::Iterator(v) => v.gc_trace(visit),
            SeqSource::IoLines { handle, .. } => handle.gc_trace(visit),
            SeqSource::Reified | SeqSource::Taken => {}
        }
    }
}

/// True iff this method answers purely from type/identity information and
/// must NEVER pull the source — not even to serve-and-retain. Measured
/// against `raku` with a side-effecting `Iterator` (docs/adr/0034 §1.4's
/// oracle, extended): calling any of these on a still-untouched deferred Seq
/// leaves the side effect log empty, unlike `.Str`/`.elems`/... which DO
/// pull (and belong in the "reify" default, not here). `.cache` is also
/// never-touch (it only sets a flag — see `SeqBody::mark_cache_requested`'s
/// doc comment) but is handled by its own dispatch arm, not this table.
pub(crate) fn seq_method_never_touches(method: &str) -> bool {
    matches!(
        method,
        "WHAT"
            | "WHICH"
            | "WHERE"
            | "HOW"
            | "VAR"
            | "defined"
            | "DEFINITE"
            | "isa"
            | "does"
            | "^name"
            | "is-lazy"
            // `$s<>` (postcircumfix `< >` zen slice, desugared to this synthetic
            // method by the parser): a pure pass-through that returns the SAME
            // Seq value unchanged (`builtins/methods_0arg/coercion.rs`'s
            // `"__mutsu_zen_angle"` arm). Must NOT route through the default
            // "reify" branch — that would mark the body `retained`, silently
            // exempting it from later consumption ("ZEN slices do not cache
            // Seqs", measured against raku: `$z<>.iterator; $z.iterator`
            // throws `X::Seq::Consumed` on the second call).
            | "__mutsu_zen_angle"
    )
}

/// True iff this method steals the Seq's iterator (rakudo: routes through
/// `.iterator`/`.list` rather than `.cache`). **The default is `false`** — a
/// method that merely needs the elements reifies and leaves the Seq usable.
/// Measured against `raku` (docs/adr/0034 §1.4); pinned by
/// `t/seq-consumption-matrix.t`.
///
/// **`"list"` (lowercase) is deliberately absent — it gets its own
/// compromise policy in `reify_or_consume_seq_target`, not this table.**
/// mutsu's parser desugars the sigil array-context deref `@$s` to the SAME
/// method-name string `"list"` an explicit `.list()` call uses
/// (`src/parser/primary/var/sigil_vars.rs`), but raku treats them
/// differently: `@$s` never consumes (measured, even over a genuinely
/// deferred source) while explicit `.list` does (measured: `$s.list;
/// $s.list` throws `X::Seq::Consumed` the second time, matching `.List`
/// below). See that call site's comment for the compromise and the two
/// local tests (`t/seq-array-context-reiterate.t`,
/// `t/io-handle-lines-words-seq.t`) it reconciles.
pub(crate) fn seq_method_consumes(method: &str) -> bool {
    matches!(
        method,
        "iterator"
            | "List"
            | "Array"
            | "eager"
            | "flat"
            | "sort"
            | "reverse"
            | "join"
            | "head"
            | "tail"
            | "first"
            | "sum"
            | "min"
            | "max"
            | "minmax"
            | "map"
            | "grep"
            | "pick"
            | "roll"
            | "unique"
            | "repeated"
            | "squish"
            | "kv"
            | "pairs"
            | "antipairs"
            | "values"
            | "keys"
            | "Slip"
            | "Set"
            | "SetHash"
            | "Bag"
            | "BagHash"
            | "Mix"
            | "MixHash"
            | "hyper"
            | "race"
            | "lazy"
            | "sink"
            | "classify"
            | "categorize"
            | "produce"
            | "rotor"
            | "batch"
            | "reduce"
            | "combinations"
            | "permutations"
            | "skip"
    )
}
