//! A frame root's static link: by-name lookups that skip the caller frames
//! (ADR-12529 phase 3).
//!
//! A frame's env is chained over its caller's (`Env::scoped_child`), so a
//! by-name lookup that misses the frame walks on into the frames that called
//! it. For a name that resolves dynamically (`$*x`, `CALLER::`) that walk is
//! the point; for a lexical it is the bug of the ADR's §1.3: `EVAL q[$x]` or
//! `::('$x')` in a routine finds whatever `$x` its caller happens to hold.
//!
//! A routine declared at the top of a compunit -- outside every routine and
//! closure body -- has the program scope as its lexical outer. When such a
//! routine's body looks names up reflectively (`CompiledCode::
//! needs_reflective_capture`), its frame root carries [`StaticLink::UnitOuter`]:
//! a *plain user lexical* (`Symbol::is_plain_user_lexical`) that misses the
//! frame's own tiers continues at the program scope's tiers, past every caller
//! frame in between. Every other name -- dynamics, `self`, the topic, `__mutsu_*`
//! metadata -- walks the whole chain as before.
//!
//! `EVAL $code, context => $ctx` asks for the scope `$ctx` names instead of
//! the running one. mutsu does not carry a frame's lexical scope in a
//! `PseudoStash` yet, so such an EVAL resolves through the whole chain, as
//! every lookup did before the static link existed ([`suppressed`]).
//!
//! Only the lookups follow the link. A flattened copy of the chain
//! (`Env::flattened` and kin) stays faithful to the whole chain, because it is
//! not only a view: a deep chain is replaced by its flattening, and a frame's
//! saved and restored env is one, so dropping the caller frames' names there
//! would lose their writes (a closure's write to `$z` that its caller has not
//! merged back yet is in exactly such a tier).

use super::Env;
use crate::symbol::Symbol;
use crate::value::Value;
use std::cell::Cell;
use std::sync::Arc;

/// How a frame root's lookups treat the frames chained below it.
pub(crate) enum StaticLink {
    /// The code's lexical outer is the program scope, whose tiers start at
    /// `target` (the chain below the deepest caller frame). A plain user
    /// lexical that misses the frame continues there. With `capture_first`
    /// -- a closure created inside a call frame -- the frame's own capture
    /// (`Env::fallback`) answers first: it holds the creating frame's scope,
    /// which lies between the closure and the program scope.
    UnitOuter {
        target: Arc<Env>,
        capture_first: bool,
    },
}

thread_local! {
    /// Depth of `EVAL ..., context => ...` calls running on this thread. While
    /// it is non-zero a static link is not followed -- see the module docs.
    static SUPPRESSED: Cell<u32> = const { Cell::new(0) };
}

/// Whether static links are currently ignored on this thread.
#[inline]
pub(crate) fn suppressed() -> bool {
    SUPPRESSED.with(Cell::get) != 0
}

/// Run `f` with static links ignored: an EVAL whose `context` names a scope
/// the running frame cannot express. Restores the previous depth on unwind.
pub(crate) fn with_static_links_suppressed<R>(f: impl FnOnce() -> R) -> R {
    struct Guard;
    impl Drop for Guard {
        fn drop(&mut self) {
            SUPPRESSED.with(|c| c.set(c.get() - 1));
        }
    }
    SUPPRESSED.with(|c| c.set(c.get() + 1));
    let _guard = Guard;
    f()
}

impl Env {
    /// Give this frame root a static link to the program scope: by-name
    /// lookups of plain user lexicals that miss the frame skip the caller
    /// frames below it. The program scope is the part of the chain below the
    /// deepest frame root -- or, when a deeper root already links there, that
    /// root's target.
    // Cost: O(d), d = tiers of the caller chain walked to its deepest frame
    // root (bounded by MAX_OVERLAY_DEPTH); O(1) when the nearest caller frame
    // is linked already.
    pub(crate) fn link_static_outer_to_unit(&mut self, capture_first: bool) {
        debug_assert!(self.frame_root, "only a frame root has a static link");
        let Some(parent) = &self.parent else {
            return;
        };
        let mut seg: &Arc<Env> = parent;
        let mut cur: &Arc<Env> = parent;
        loop {
            if cur.frame_root {
                if let Some(link) = cur.inner.static_link() {
                    let StaticLink::UnitOuter { target, .. } = link;
                    seg = target;
                    break;
                }
                if let Some(below) = &cur.parent {
                    seg = below;
                }
            }
            match &cur.parent {
                Some(below) => cur = below,
                None => break,
            }
        }
        let seg = Arc::clone(seg);
        self.cow_mut()
            .set_static_link(Arc::new(StaticLink::UnitOuter {
                target: seg,
                capture_first,
            }));
        self.chain_has_static_link = true;
    }

    /// Whether any tier of this chain is a call frame's root -- false for an
    /// env running the program scope itself (the mainline and its blocks).
    // Cost: O(d), d = chain tiers.
    pub(crate) fn chain_has_frame_root(&self) -> bool {
        let mut cur = self;
        loop {
            if cur.frame_root {
                return true;
            }
            match &cur.parent {
                Some(parent) => cur = parent,
                None => return false,
            }
        }
    }

    /// [`Self::flattened`] as the scope this env's static links describe: a
    /// plain user lexical that only a skipped caller frame binds is dropped,
    /// and one a skipped frame shadows takes the value the link's target
    /// gives it. A *view*, for a reflective closure's capture -- never a
    /// replacement for the chain (see the module docs).
    // Cost: O(n) for the flatten, n = visible entries; plus O(h * d) when the
    // chain holds a static link, h = plain user lexicals the skipped frames
    // bind, d = chain tiers.
    pub(crate) fn flattened_static_view(&self) -> Env {
        let mut flat = self.flattened();
        if !self.chain_has_static_link || suppressed() {
            return flat;
        }
        let mut hidden: Vec<Symbol> = Vec::new();
        let mut target: Option<*const Env> = None;
        let mut cur = self;
        loop {
            if target.is_some_and(|t| std::ptr::eq(t, cur)) {
                target = None;
            }
            if target.is_some() {
                hidden.extend(
                    cur.inner
                        .keys()
                        .copied()
                        .filter(|k| k.is_plain_user_lexical()),
                );
                if let Some(fb) = &cur.fallback {
                    hidden.extend(
                        fb.iter()
                            .map(|(k, _)| *k)
                            .filter(|k| k.is_plain_user_lexical()),
                    );
                }
            } else if let Some(link) = cur.inner.static_link() {
                let StaticLink::UnitOuter { target: seg, .. } = link;
                target = Some(Arc::as_ptr(seg));
            }
            match &cur.parent {
                Some(parent) => cur = parent,
                None => break,
            }
        }
        for key in hidden {
            match self.get_sym(key) {
                Some(v) => {
                    let v = v.clone();
                    flat.insert_sym(key, v);
                }
                None => {
                    flat.remove_sym(key);
                }
            }
        }
        flat
    }

    /// Mark this env, a reflective closure's capture, as taken at program
    /// scope: the closure's frame then links to the program scope
    /// ([`Self::link_static_outer_to_unit`]).
    // Cost: O(1), plus the overlay's copy-on-write when it is shared.
    pub(crate) fn mark_program_scope_capture(&mut self) {
        self.cow_mut().mark_program_scope_capture();
    }

    /// Whether this env is a capture marked by
    /// [`Self::mark_program_scope_capture`].
    // Cost: O(1).
    pub(crate) fn is_program_scope_capture(&self) -> bool {
        self.inner.is_program_scope_capture()
    }

    /// [`Self::get`] through the whole chain, callers included, whatever
    /// static links it holds. For the writeback machinery that carries a
    /// value *across* frames (a runtime-named write a callee frame holds for
    /// its caller): it reads where a value currently is, not what a name
    /// means in this frame's scope.
    // Cost: O(d), d = chain tiers.
    pub(crate) fn get_through_callers(&self, key: &str) -> Option<&Value> {
        with_static_links_suppressed(|| self.get(key))
    }

    /// Where a lookup of `key` that missed this env continues, when this env
    /// is a frame root whose static link skips the callers for it.
    // Cost: O(1).
    #[inline]
    pub(super) fn static_skip(&self, key: Symbol) -> Option<&Env> {
        let link = self.inner.static_link()?;
        if !key.is_plain_user_lexical() || suppressed() {
            return None;
        }
        let StaticLink::UnitOuter { target, .. } = link;
        Some(target)
    }

    /// The value this frame root's own capture gives `key`, when its static
    /// link puts the capture ahead of the program scope.
    // Cost: O(l), l = the capture's layers.
    #[inline]
    pub(super) fn static_capture_hit(&self, key: Symbol) -> Option<&Value> {
        let StaticLink::UnitOuter {
            capture_first: true,
            ..
        } = self.inner.static_link()?
        else {
            return None;
        };
        if !key.is_plain_user_lexical() || suppressed() {
            return None;
        }
        self.fallback.as_deref()?.get(&key)
    }
}
