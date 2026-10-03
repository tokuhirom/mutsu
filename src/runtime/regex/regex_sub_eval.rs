//! Running Raku code from inside a regex/grammar match on the caller's own
//! interpreter.
//!
//! The regex engine reaches user code at several points besides the inline
//! `{ … }` / `<?{ … }>` atoms (which run through `eval_regex_inline_code`):
//! a `<{ … }>` interpolation, a `** { … }` quantifier, a subrule argument
//! expression, a parameterized `token t($x)` body, a regex value's signature
//! binding, a grammar method called as a subrule, a custom-HOW `find_method`,
//! and the in-parse action runs behind `$<x>.made`. Each of them used to build
//! a whole scratch `Interpreter` for the call — an interpreter construction, an
//! env clone and a registry copy per call — and run the code there (#10151).
//!
//! They now run on `self`, inside [`Interpreter::run_regex_sub_eval`]. What
//! the scratch gave them was isolation: every write the code made to its env
//! was dropped with the scratch. The helper keeps exactly that contract on the
//! running interpreter by swapping the env out and back — `Env` is a
//! copy-on-write `Arc`, so both swaps are O(1) — together with the few other
//! pieces of per-evaluation state a fresh interpreter started empty (the
//! compiled-local writeback log, the readonly-name set). State
//! outside those (the registry, the IO handle table, the unit and package
//! lexical stores, the in-progress `:actions` object) is the caller's own, which
//! the scratch could only approximate by copying it in.

use super::super::*;
use crate::env::Env;
use crate::symbol::Symbol;

impl Interpreter {
    /// Run `f` on this interpreter with `env` as its env and, when `pkg` is
    /// given, `pkg` as the current package; restore both afterwards.
    ///
    /// The env writes `f` makes are discarded, as they were when this code ran
    /// in a scratch interpreter: a caller that wants one of them reads it out
    /// of `self.env` inside `f`. The compiled-local writeback log
    /// (`pending_local_updates`) and the inline-code-block flag are started
    /// empty and restored for the same reason — an entry logged against the
    /// swapped-in env must not be replayed into the caller's slots, and the
    /// caller's own pending entries must not be drained by `f`. So is the
    /// readonly-name set, which lives beside the env rather than in it: a
    /// parameter `f` binds (`token t(:$value)`) is marked readonly there, and
    /// left in place that mark made the caller's own same-named `$value`
    /// unassignable after the match.
    ///
    /// Cost: O(1) (two `Env` Arc swaps, a readonly-set swap and a package
    /// switch), plus `f`.
    pub(in crate::runtime) fn run_regex_sub_eval<R>(
        &mut self,
        env: Env,
        pkg: Option<Symbol>,
        f: impl FnOnce(&mut Self) -> R,
    ) -> R {
        let _pkg_guard = pkg.map(|p| self.enter_package_guarded_sym(p));
        let saved_env = std::mem::replace(&mut self.env, env);
        let saved_pending = std::mem::take(&mut self.pending_local_updates);
        let saved_in_code_block =
            std::mem::replace(&mut self.regex_state.in_regex_code_block, false);
        let saved_readonly = self.take_readonly_state();
        let result = f(self);
        self.restore_readonly_state(saved_readonly);
        self.regex_state.in_regex_code_block = saved_in_code_block;
        self.pending_local_updates = saved_pending;
        self.env = saved_env;
        result
    }

    /// [`Self::run_regex_sub_eval_here`] for a ROUTINE CALL the engine makes on
    /// the user's behalf (a grammar method reached as a subrule, a custom-HOW
    /// `find_method` and the wrapper it returns): the call keeps the env
    /// isolation, except that an assignment to a dynamic variable the caller
    /// already has reaches the caller, as it does from any method call
    /// (`method ws { $*HIGHWATER = self.pos; ... }`, #11326). A `my $*X`
    /// declared inside the call stays local: it is gone from the env once the
    /// routine returns, and a name the caller does not bind is never written.
    ///
    /// The call runs over a block tier ([`Self::open_block_env_tier`]), so the
    /// writes to replay are read back from the tier's own overlay rather than
    /// diffed against the whole visible env.
    ///
    /// Cost: O(w), w = names the call wrote by name (O(v) over the visible env
    /// when the call replaced the env wholesale), plus `f`.
    pub(in crate::runtime) fn run_regex_sub_call_here<R>(
        &mut self,
        pkg: Option<Symbol>,
        f: impl FnOnce(&mut Self) -> R,
    ) -> R {
        let _pkg_guard = pkg.map(|p| self.enter_package_guarded_sym(p));
        let save = self.open_block_env_tier();
        let saved_pending = std::mem::take(&mut self.pending_local_updates);
        let saved_in_code_block =
            std::mem::replace(&mut self.regex_state.in_regex_code_block, false);
        let saved_readonly = self.take_readonly_state();
        let result = f(self);
        self.restore_readonly_state(saved_readonly);
        self.regex_state.in_regex_code_block = saved_in_code_block;
        self.pending_local_updates = saved_pending;
        let closed = self.close_block_env_tier(save);
        let mut base = closed.base;
        for (key, value) in closed.writes.iter() {
            if key.is_dynamic_var_env_key()
                && base
                    .get_sym(*key)
                    .is_some_and(|old| !old.same_binding(value))
            {
                base.insert_sym(*key, value.clone());
            }
        }
        self.env = base;
        result
    }

    /// [`Self::run_regex_sub_eval`] over a copy of the current env.
    ///
    /// Cost: O(1), plus `f`.
    pub(in crate::runtime) fn run_regex_sub_eval_here<R>(
        &mut self,
        pkg: Option<Symbol>,
        f: impl FnOnce(&mut Self) -> R,
    ) -> R {
        let env = self.env.clone();
        self.run_regex_sub_eval(env, pkg, f)
    }
}
