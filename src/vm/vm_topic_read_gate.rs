//! The gates of `GetGlobal`'s plain-env shortcut (#18274).
//!
//! The shortcut serves a free-variable read straight from `env` when no store
//! the slow chain asks before `env` can hold the name. For a general name
//! that takes four checks: no mainline capture active (ADR-0024), no
//! routine-nested sub alias active (ADR-0114), a GLOBAL current package (a
//! package-block lexical), and no file-scope bucket of a package-less module.
//! The topic `$_` is every routine's own (`env::routine_owned`), so none of
//! those stores holds it and a module routine's topic read need not leave the
//! shortcut; [`Interpreter::topic_outside_env_stores_miss`] checks that claim
//! in debug builds.

use super::*;

impl Interpreter {
    /// Whether none of the stores the slow chain asks before `env` can hold
    /// `name` for the running frame: see the module docs.
    // Cost: O(1) for the three latches; the bucket probe is one hash lookup.
    #[inline]
    pub(super) fn env_only_free_read_possible(&self, name: &str) -> bool {
        !self.mainline_lexical_frame_active()
            && !self.lexsub_alias_frame_active()
            && self.current_package_is_global()
            // A package-less module file's routine runs in GLOBAL too, and its
            // file-scope lexicals are not in `env` (`vm_compunit_lexicals`).
            && self.compunit_lexical_bucket(name).is_none()
    }

    /// Debug check behind the topic's exemption from
    /// [`Self::env_only_free_read_possible`]: every store the slow chain
    /// consults ahead of `env` misses `_`.
    // Cost: the sum of those stores' probes; debug builds only.
    pub(super) fn topic_outside_env_stores_miss(&self, code: &CompiledCode) -> bool {
        let topic = crate::symbol::wk::topic();
        self.per_call_anon_state_read("_", Value::NIL).is_none()
            && self.eval_unit_package_capture("_").is_none()
            && self.package_scope_lexical_in(code, "_").is_none()
            && self.unit_scope_lexical("_", Some(topic)).is_none()
            && self.compunit_lexical_bucket("_").is_none()
    }
}
