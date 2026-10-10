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

use super::{Env, SymMap};
use crate::symbol::Symbol;
use std::cell::Cell;
use std::sync::Arc;
use std::sync::atomic::{AtomicBool, Ordering};

/// How a frame root's lookups treat the frames chained below it.
pub(crate) enum StaticLink {
    /// The routine's lexical outer is the program scope, whose tiers start at
    /// this env (the chain below the deepest caller frame). A plain user
    /// lexical that misses the frame continues here.
    UnitOuter(Arc<Env>),
}

/// Whether any frame root in the process ever carried a static link. The
/// flatten family asks it before walking a chain for hidden names, so a
/// program with no reflective routine pays one relaxed load per flatten.
static STATIC_LINKS_USED: AtomicBool = AtomicBool::new(false);

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
    pub(crate) fn link_static_outer_to_unit(&mut self) {
        debug_assert!(self.frame_root, "only a frame root has a static link");
        let Some(parent) = &self.parent else {
            return;
        };
        let mut seg: &Arc<Env> = parent;
        let mut cur: &Arc<Env> = parent;
        loop {
            if cur.frame_root {
                if let Some(link) = &cur.static_link {
                    let StaticLink::UnitOuter(target) = &**link;
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
        STATIC_LINKS_USED.store(true, Ordering::Relaxed);
        self.static_link = Some(Arc::new(StaticLink::UnitOuter(seg)));
    }

    /// Where a lookup of `key` that missed this env continues, when this env
    /// is a frame root whose static link skips the callers for it.
    // Cost: O(1).
    #[inline]
    pub(super) fn static_skip(&self, key: Symbol) -> Option<&Env> {
        let link = self.static_link.as_deref()?;
        if !key.is_plain_user_lexical() || suppressed() {
            return None;
        }
        let StaticLink::UnitOuter(target) = link;
        Some(target)
    }

    /// Make `merged`, a flattening of this env's whole chain, agree with the
    /// static links in it: a plain user lexical that only a skipped caller
    /// frame binds is dropped, and one a skipped frame shadows takes the value
    /// the program scope gives it.
    // Cost: O(1) when no static link was ever made; otherwise O(d + h * d),
    // d = chain tiers, h = plain user lexicals the skipped frames bind.
    pub(super) fn apply_static_links(&self, merged: &mut SymMap) {
        if !STATIC_LINKS_USED.load(Ordering::Relaxed) || suppressed() {
            return;
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
            } else if let Some(link) = &cur.static_link {
                let StaticLink::UnitOuter(seg) = &**link;
                target = Some(Arc::as_ptr(seg));
            }
            match &cur.parent {
                Some(parent) => cur = parent,
                None => break,
            }
        }
        for key in hidden {
            match self.get_sym_walk(key) {
                Some(v) => {
                    merged.insert(key, v.clone());
                }
                None => {
                    merged.remove(&key);
                }
            }
        }
    }
}
