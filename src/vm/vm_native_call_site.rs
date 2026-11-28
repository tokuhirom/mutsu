//! `OpCode::CallFunc` of a built-in routine, with a per-site route memo
//! (#18028).
//!
//! A call such as `abs($x)`, `max($a, $b)` or `uniname($_)` names a routine
//! that no user code declares, so the general function-call path walks its
//! whole resolution ladder -- the light/OTF caches, the lexical `&name`
//! override, CALL-ME, autothreading, the export hook, the wrap chain, then
//! inside `dispatch_func_call_inner` the compiled-function, proto, multi and
//! user-shadow probes -- before it reaches `try_native_function`. That was
//! ~15k instructions and ~20 allocations a call, against ~2k for the builtin
//! itself.
//!
//! Every rung ahead of the native arm declines for such a name for a reason
//! that is one of three kinds, and the lane re-establishes each kind the way it
//! can change:
//!
//! - **What is declared.** No routine, proto or multi of the name exists, and
//!   so no compiled function, user shadow or composed CALL-ME either. A
//!   declaration is a registry write, so the site remembers its route for one
//!   registry write generation (`CompiledCode::native_call_sites`), and the
//!   facts are checked when the route is remembered.
//! - **What the name is.** Compiler-internal names (`__mutsu_*`), `nqp::` ops,
//!   `make`, the redispatchers and the core type coercers each have an arm of
//!   their own ahead of the ladder; such a site is never remembered as a
//!   native route. `make`'s arm remembers a route of its own (`ROUTE_MAKE`)
//!   when it answered, under the same generation and run-time re-checks.
//! - **What is bound at run time.** A lexical `&name` (a local slot, or an
//!   `env` binding from an outer scope, an import or an EXPORT hook), a wrap
//!   of any routine, an EXPORT-hook override of the name, a `my method` of the
//!   name, an lvalue context, and the arguments (a Junction autothreads, a
//!   Proxy is FETCHed per parameter) are re-checked on every hit, and any of
//!   them sends the call down the general path.
//!
//! A hit calls `try_native_function` with the arguments prepared exactly as
//! the general path prepares them for a name nothing declares (`VarRef` tags
//! unwrapped, the callsite-line marker taken off). Should the builtin decline
//! these particular arguments, the call continues at the general path's
//! `dispatch_func_call_inner`, whose ladder declines down to the native arm
//! again and goes on from there: the memo decides only which call does less
//! work, never which routine answers.

use super::vm_call_helpers::CallArgKinds;
use super::*;
use crate::runtime::dispatch_key;

impl Interpreter {
    /// Answer a `CallFunc` from its site's remembered native route, or `None`
    /// to take the general path.
    // Cost: O(a) on a hit, a = arguments, plus one `env` probe for the
    // name's `&` binding and the builtin's own cost; O(1) on a miss.
    pub(super) fn try_native_call_site(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
        arity: u32,
        arg_kinds: CallArgKinds,
        arg_sources_idx: Option<u32>,
        compiled_fns: &CompiledFns,
    ) -> Option<Result<(), RuntimeError>> {
        let generation = self.registry_write_generation();
        let sites = code.constants.len();
        let route = code
            .native_call_sites
            .cached(sites, name_idx as usize, generation)?;
        let name_sym = code.const_sym(name_idx);
        if self.in_lvalue_assignment
            || self
                .env()
                .get_sym(dispatch_key::amp_sym(name_sym))
                .is_some()
            || self.any_routine_wrapped()
            || self.module.export_amp_override_names.contains(&name_sym)
            || self.class_lexical_method_callable(code, name_sym).is_some()
        {
            return None;
        }
        let arity = arity as usize;
        let start = self.stack.len().checked_sub(arity)?;
        // The arguments as the general path hands them to a name nothing
        // declares. A Junction (in a container, or as a named argument's
        // value) is left to the general path's autothreading, which the
        // fallback below does not repeat.
        let args: Vec<Value> = self.stack[start..]
            .iter()
            .cloned()
            .map(Self::unwrap_var_ref_arg)
            .collect();
        if args.iter().any(|arg| {
            let junction = match arg.view() {
                ValueView::Pair(_, value) => Self::unwrap_junction_value(value).is_some(),
                _ => Self::unwrap_junction_value(arg).is_some(),
            };
            junction || arg.is_proxy_value()
        }) {
            return None;
        }
        // `make`'s lane takes one positional; any other shape is the
        // funnel's to bind (and reject).
        if route == ROUTE_MAKE && !matches!(&args[..], [arg] if !arg.is_string_pair_value()) {
            return None;
        }
        self.stack.truncate(start);
        let (mut args, callsite_line) = self.sanitize_call_args_owned(args);
        loan_env!(self, set_pending_callsite_line(callsite_line));
        self.set_pending_call_arg_sources(None);
        if route == ROUTE_MAKE {
            // What the funnel's `make` lane does, without the funnel
            // (`try_make_builtin_call`): nothing it checks can have changed
            // but what was re-checked above.
            return Some(
                self.make_on_frame_match_topic(code, &args)
                    .map(|value| self.stack.push(value)),
            );
        }
        // A builtin with no arm of its own in `try_native_function` (see
        // `ROUTE_PLAIN`), called with defined plain scalars: every arm there
        // declines for such arguments, so the routine itself answers.
        let plain = if route == ROUTE_PLAIN && args.iter().all(Self::is_plain_scalar_arg) {
            crate::builtins::native_function(name_sym, &args)
        } else {
            None
        };
        let result = match plain.or_else(|| self.try_native_function(name_sym, &mut args)) {
            Some(result) => result,
            None => {
                // The builtin declined these arguments: go on down the
                // ladder, as the general path does after the native arm.
                let name = Self::const_str(code, name_idx);
                let decoded = self.decode_arg_sources(code, arg_sources_idx);
                let (args, arg_sources) =
                    Self::spread_call_args_by_syntax(code, args, arg_sources_idx, decoded);
                self.dispatch_func_call_inner(
                    code,
                    name,
                    name_sym,
                    args,
                    arg_sources,
                    arg_kinds.named,
                    None,
                    compiled_fns,
                    None,
                )
            }
        };
        self.apply_pending_rw_writeback(code);
        match result {
            Ok(value) => {
                self.stack.push(value);
                Some(Ok(()))
            }
            Err(error) => Some(Err(error)),
        }
    }

    /// Remember that the `CallFunc` of constant `name_idx` was answered by the
    /// native arm, when the general path's call just was and nothing the lane
    /// relies on was in the way. `generation` is the registry write generation
    /// read before the call, so a call that itself declared something is not
    /// remembered.
    // Cost: O(1) when the call took another route; otherwise a handful of
    // memoized registry probes and one lock.
    pub(super) fn note_native_call_site(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
        name_sym: Symbol,
        generation: u64,
    ) {
        if self.caches.native_arm_answered.take() != Some(name_sym)
            || self.registry_write_generation() != generation
        {
            return;
        }
        let name = name_sym.as_str();
        if name.starts_with("__")
            || name.starts_with("nqp::")
            || crate::qualified::is_qualified(name_sym)
            || name_sym == crate::symbol::wk::make()
            || matches!(
                name,
                "callsame" | "callwith" | "nextsame" | "nextwith" | "samewith"
            )
            || Self::name_is_core_type_coercer(name)
            || !code
                .local_slots_of(dispatch_key::amp_sym(name_sym))
                .is_empty()
            || self.has_declared_function_cached_sym(name, name_sym)
            || self.has_multi_candidates_cached_sym(name_sym)
            || self.has_proto_cached_sym(name, name_sym)
        {
            return;
        }
        let route = if Self::native_name_has_own_arm(name) {
            ROUTE_NATIVE
        } else {
            ROUTE_PLAIN
        };
        code.native_call_sites
            .remember(code.constants.len(), name_idx as usize, generation, route);
    }

    /// Whether `try_native_function` has an arm for `name` that may answer
    /// even for defined plain scalar arguments (`zip`'s streaming, the
    /// free-function `floor`/`ceiling` of a literal, the `Map` coercer, the
    /// attribute trait). Every other arm there keys on an argument that is a
    /// Seq, a LazyList, a shaped array, an Instance or a type object.
    // Cost: O(1), a constant-table match.
    fn native_name_has_own_arm(name: &str) -> bool {
        matches!(
            name,
            "zip" | "floor" | "ceiling" | "ceil" | "Map" | "trait_mod:<is>"
        )
    }

    /// Whether `arg` is a defined plain scalar: an Int, a Num, a Str, a Bool
    /// or a Rat, which no special arm of `try_native_function` acts on.
    // Cost: O(1).
    fn is_plain_scalar_arg(arg: &Value) -> bool {
        matches!(
            arg.view(),
            ValueView::Int(_)
                | ValueView::BigInt(_)
                | ValueView::Num(_)
                | ValueView::Str(_)
                | ValueView::Bool(_)
                | ValueView::Rat(..)
        )
    }
}

/// A site's remembered route: the unshadowed `make` builtin
/// (`make_on_frame_match_topic`), which the funnel answers in a lane of its
/// own ahead of the native arm (`try_make_builtin_call`).
pub(super) const ROUTE_MAKE: u32 = 3;
/// A site's remembered route: the native arm, through `try_native_function`.
const ROUTE_NATIVE: u32 = 1;
/// The native arm of a name with no arm of its own in `try_native_function`,
/// which a call with plain scalar arguments skips.
const ROUTE_PLAIN: u32 = 2;
