//! Linking a `CallGen` site to the TRIR routine it turns out to reach
//! (ADR-0111 Step 1).
//!
//! A TRIR body links a call statically (`CallTr`) only to a routine declared
//! BEFORE it, because that is all its compile has seen. Every other call is a
//! `CallGen`: arguments boxed and containerized, the callee resolved by name
//! through `call_function`'s whole chain, and the callee then run on the
//! UNTYPED path even when it has a chunk of its own. JSON::Fast pays that on
//! every value, because `parse-obj`/`parse-array` call `parse-thing`, which is
//! declared after them. Mutual recursion makes one direction a forward
//! reference, whatever the order. Measured at 8.9 µs a call, against 172 ns
//! for a `CallTr` and 80 ns in rakudo (ADR-0111 §1.3).
//!
//! The fix links such a site at run time, from what the generic dispatch
//! actually did. The first call of a site goes the generic way, with an
//! observer armed for the site's callee name. When `call_function_fallback`
//! reaches its plain user-routine branch for that name, and no multi dispatch
//! is involved, it records the def it picked. If that def has a TRIR chunk,
//! the site remembers the chunk, keyed by everything the resolution read:
//! `fn_resolve_gen`, the current package, and the running frame's lexical
//! package (`bare_name_packages_syms`' two inputs). A later call in the same
//! state runs the chunk directly, the way a `CallTr` does.
//!
//! This is sound for two reasons. The routine is the one the generic path
//! would have picked in that state, because it is what the generic path did
//! pick. And running that routine's chunk instead of its untyped body is the
//! equivalence ADR-0110's differential gate holds TRIR to. Every argument
//! goes through the same bind checks as `CallTr`, before anything is
//! consumed. A shape they do not cover (a junction to autothread, an aggregate
//! a `$` parameter would share, a type the chunk cannot bind) takes the
//! generic path instead, exactly as the first call did.

use std::collections::HashMap;
use std::sync::Arc;

use super::exec::TrOutcome;
use super::frame::TrFrame;
use super::{TrArg, TrChunk, TrInnerCall, TrKind};
use crate::opcode::CompiledFns;
use crate::runtime::Interpreter;
use crate::symbol::Symbol;
use crate::value::{RuntimeError, Value, ValueView};

/// The resolution state a link was observed in.
type GenContext = (u64, Symbol, Option<Symbol>);

/// What [`Interpreter::trir_gen_arm`] hands [`Interpreter::trir_gen_settle`]:
/// the state the call started in, and the enclosing call's observer, which
/// this one displaces for its duration.
pub(super) struct GenArmed {
    ctx: GenContext,
    outer_observe: Option<Symbol>,
    outer_observed: Option<Arc<crate::opcode::CompiledFunction>>,
}

struct GenLink {
    ctx: GenContext,
    chunk: Arc<TrChunk>,
    pkg: Option<Symbol>,
}

/// Every `CallGen` site's observed link, plus the observer the generic path
/// fills. One per interpreter (it lives in [`super::frame::TrStacks`]); a
/// worker thread starts with an empty one.
#[derive(Default)]
pub(crate) struct GenLinks {
    links: HashMap<(u64, u32), GenLink>,
    /// Armed with the callee name while a `CallGen` dispatches generically.
    pub(crate) observe: Option<Symbol>,
    /// What the generic path's plain user-routine branch picked for the
    /// armed name.
    pub(crate) observed: Option<Arc<crate::opcode::CompiledFunction>>,
}

impl std::fmt::Debug for GenLinks {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("GenLinks")
            .field("links", &self.links.len())
            .finish()
    }
}

/// What [`Interpreter::try_trir_gen_link`] did.
pub(super) enum GenOutcome {
    /// Nothing: no link, or a shape the link does not serve. No operand has
    /// been consumed, so the generic path takes the call.
    NotLinked,
    /// The linked chunk ran and its result is on the bank.
    Done,
    /// The linked chunk bailed. As for a `CallTr`, the caller bails too.
    Bail,
}

impl Interpreter {
    fn trir_gen_context(&self) -> GenContext {
        (
            self.fn_resolve_gen,
            self.current_package_sym(),
            self.routine_stack().last().and_then(|f| f.lexical_package),
        )
    }

    /// Arm the observer for a generic call to `name`, answering the state the
    /// call starts in.
    ///
    /// Calls nest: the routine a generic call reaches runs on the untyped
    /// path, and a TRIR routine IT calls may make a generic call of its own,
    /// arming the observer again. That happens after the outer call's
    /// resolution has already been recorded, so the outer observer is set
    /// aside here and put back by [`Self::trir_gen_settle`] rather than being
    /// cleared: clearing it lost every link whose callee makes a generic call
    /// (`parse-thing`, reached from `parse-array`, calls `parse-string-slow`).
    pub(super) fn trir_gen_arm(&mut self, name: Symbol) -> GenArmed {
        GenArmed {
            ctx: self.trir_gen_context(),
            outer_observe: self.trir.gen_links.observe.replace(name),
            outer_observed: self.trir.gen_links.observed.take(),
        }
    }

    /// Disarm after the generic call, restore the enclosing call's observer,
    /// and link the site when the generic path reached a routine with a
    /// chunk.
    pub(super) fn trir_gen_settle(&mut self, chunk: &TrChunk, site: u32, armed: GenArmed) {
        let GenArmed {
            ctx,
            outer_observe,
            outer_observed,
        } = armed;
        let mine = std::mem::replace(&mut self.trir.gen_links.observed, outer_observed);
        self.trir.gen_links.observe = outer_observe;
        let Some(cf) = mine else {
            return;
        };
        let Some(callee) = cf.trir.clone() else {
            return;
        };
        let pkg = super::entry::trir_body_package(&cf);
        self.trir.gen_links.links.insert(
            (chunk.id, site),
            GenLink {
                ctx,
                chunk: callee,
                pkg,
            },
        );
    }

    /// Run a `CallGen` site through its link, when it has one that holds in
    /// the current state and its arguments bind.
    pub(super) fn try_trir_gen_link(
        &mut self,
        chunk: &TrChunk,
        site: u32,
        call: &TrInnerCall,
        frame: TrFrame,
        compiled_fns: &CompiledFns,
    ) -> Result<GenOutcome, RuntimeError> {
        let Some(link) = self.trir.gen_links.links.get(&(chunk.id, site)) else {
            return Ok(GenOutcome::NotLinked);
        };
        if link.ctx != self.trir_gen_context() {
            return Ok(GenOutcome::NotLinked);
        }
        let callee = link.chunk.clone();
        let callee_pkg = link.pkg;
        if callee.params.len() != call.args.len()
            || (self.any_routine_wrapped() && self.routine_is_wrapped(&call.name.resolve()))
        {
            return Ok(GenOutcome::NotLinked);
        }
        // The generic path coerces every evaluated argument to a boxed value,
        // so a native value on the native operand stack is not a shape it
        // emits. Decline it rather than guess where it sits.
        if call
            .args
            .iter()
            .any(|a| matches!(a, TrArg::Value(TrKind::Int | TrKind::Num)))
        {
            return Ok(GenOutcome::NotLinked);
        }
        self.guard_native_stack()?;
        let n_values = call
            .args
            .iter()
            .filter(|a| matches!(a, TrArg::Value(TrKind::Obj)))
            .count();
        let os_first = self.trir.os.len() - n_values;
        let nbase = frame.nbase as usize;
        let obase = frame.obase as usize;

        // Bind by PEEKING, so a declined argument leaves every operand where
        // the generic path expects it.
        let mut callee_frame = self.trir.push_frame(callee.n_native, callee.n_obj);
        let cnb = callee_frame.nbase as usize;
        let mut next_value = os_first;
        for (i, p) in callee.params.iter().enumerate() {
            let arg = &call.args[i];
            if p.is_rw {
                // Only a native `is rw` parameter is TRIR, and it aliases a
                // variable: hand it the caller slot's absolute index.
                let target = match arg {
                    TrArg::Native(s) if p.kind.is_native() => nbase + *s as usize,
                    TrArg::Ref(s) if p.kind.is_native() => {
                        self.trir.nl[nbase + *s as usize] as usize
                    }
                    _ => {
                        self.trir.pop_frame(callee_frame);
                        return Ok(GenOutcome::NotLinked);
                    }
                };
                self.trir.nl[cnb + p.slot as usize] = target as i64;
                continue;
            }
            let val = match arg {
                TrArg::Native(s) => Value::int(self.trir.nl[nbase + *s as usize]),
                TrArg::Ref(s) => {
                    let r = self.trir.nl[nbase + *s as usize] as usize;
                    Value::int(self.trir.nl[r])
                }
                TrArg::Obj(s) => self.trir.ol[obase + *s as usize].clone(),
                TrArg::Value(_) => {
                    let v = self.trir.os[next_value].clone();
                    next_value += 1;
                    v
                }
            };
            // A junction autothreads and an aggregate shares its container
            // into a `$` parameter; both are the generic binder's business.
            let declines = matches!(
                val.view(),
                ValueView::Junction { .. } | ValueView::Array(..) | ValueView::Hash(..)
            ) || self.bind_ro_param(callee_frame, p, &val).is_none();
            if declines {
                self.trir.pop_frame(callee_frame);
                return Ok(GenOutcome::NotLinked);
            }
        }
        let guard = callee_pkg.map(|p| self.enter_package_guarded_sym(p));
        if !self.trir_seed_outers(&callee, callee_frame) {
            drop(guard);
            self.trir.pop_frame(callee_frame);
            return Ok(GenOutcome::NotLinked);
        }
        // Committed: the values are consumed now. They sit below the callee
        // frame's operand marks, so drop them from under it.
        self.trir.os.drain(os_first..os_first + n_values);
        callee_frame.os_mark -= n_values as u32;
        let outcome = self.run_trir_chunk(&callee, callee_frame, compiled_fns);
        drop(guard);
        self.trir.pop_frame(callee_frame);
        super::stats::record_gen_link();
        match outcome? {
            TrOutcome::Value(v) => {
                self.push_trir_result(v, call.result);
                Ok(GenOutcome::Done)
            }
            TrOutcome::Bail => Ok(GenOutcome::Bail),
        }
    }
}
