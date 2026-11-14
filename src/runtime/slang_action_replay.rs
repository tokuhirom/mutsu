//! The actions of a slang production re-run in the importing unit (ADR-17243).
//!
//! A slang production's token and action run in the slang host, a separate
//! interpreter. What reaches the unit is the node the action attached; any other
//! effect of the action (Qwiratry's `mold` pushes a `Mold` onto a module-level
//! registry) stays in the host. While a hook runs, the slang's own action calls
//! are recorded with their Match (`slang_class_replay::record_slang_action`); the
//! statement the host returns for the production calls
//! `__mutsu_slang_replay_actions` with that record, which calls the same action
//! methods over the same Match nodes here, with the dynamics (`:my $*BLOCK`) their
//! rule declared. The effects then happen in the interpreter that runs the unit.

use super::*;
use std::cell::{Cell, RefCell};
use std::collections::HashSet;

thread_local! {
    /// The entry that builds the replay's actions object, evaluated on first use.
    static REPLAY_ENTRY: RefCell<Option<Value>> = const { RefCell::new(None) };
    /// The productions already replayed (by the site the host named them): a
    /// statement the unit executes again (a loop body, a routine) must not
    /// register twice what rakudo's single compile-time action run registered once.
    static REPLAYED: RefCell<HashSet<i64>> = RefCell::new(HashSet::new());
    static CLASS_SERIAL: Cell<u64> = const { Cell::new(0) };
    /// The actions class (with its stubs) built for a set of roles.
    static BUILT: RefCell<Vec<(String, Value)>> = const { RefCell::new(Vec::new()) };
    /// The package the production being replayed stands in (a class body), while
    /// its actions run.
    static REPLAY_OWNER: RefCell<Option<String>> = const { RefCell::new(None) };
}

/// The package a replayed action's production stands in: the class whose body
/// declares the method a `routine-declarator` production built, so the action's
/// `$method.meta-object` is that class's method (EEL's `pattern`), as the
/// compile-time action run of rakudo sees it. `None` outside a replay.
// Cost: O(1).
pub(crate) fn slang_replay_owner() -> Option<String> {
    REPLAY_OWNER.with(|o| o.borrow().clone())
}

/// The actions class and its stubs, built from the slang's action roles.
const REPLAY_BUILDER: &str = r#"
sub mutsu-slang-replay-build(@roles, Str $name) {
    my $a = Metamodel::ClassHOW.new_type(:$name);
    $a.^add_parent(Mutsu::Slang::Host::Actions);
    $a.^add_role($_) for @roles;
    $a.^compose;
    ($a.new, Mutsu::Slang::Host::Resolver.new, Mutsu::Slang::Host::CompUnit.new)
}
# The resolver of a package declarator's replay (ADR-17741 §2.3): the package
# the production stands in and `GLOBAL`, the real ones.
sub mutsu-slang-replay-resolver(Mu $package) {
    Mutsu::Slang::Host::Resolver.new(:known, :$package, :global(GLOBAL))
}
(&mutsu-slang-replay-build, &mutsu-slang-replay-resolver)
"#;

impl Interpreter {
    /// `__mutsu_slang_replay_actions(@roles, @events)`: call each recorded action
    /// of the production here, in the order the host ran them.
    // Cost: O(a) action calls, a = the recorded actions (once per production).
    pub(super) fn builtin_slang_replay_actions(
        &mut self,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let roles = args.first().cloned().unwrap_or(Value::NIL);
        let events = args.get(1).and_then(Value::as_list_items).ok_or_else(|| {
            RuntimeError::new("slang action replay without a record of the actions")
        })?;
        // A role the host defined under a name the unit does not know (a `my` or
        // anonymous role) cannot be composed here: such a slang keeps its effects in
        // the host, as before.
        // TODO: slangs registered through a role only the host can name (#17008).
        if !self.slang_replay_roles_known(&roles) {
            return Ok(Value::NIL);
        }
        let site = events
            .first()
            .and_then(|e| e.as_list_items())
            .and_then(|parts| parts.get(3).and_then(Value::as_int));
        if let Some(site) = site
            && !REPLAYED.with(|r| r.borrow_mut().insert(site))
        {
            return Ok(Value::NIL);
        }
        self.replay_action_events(roles, &events, None)
    }

    /// Whether every role of `roles` is one this interpreter can compose (a
    /// `my` or anonymous role the host defined is not).
    // Cost: O(r), r = the roles.
    pub(super) fn slang_replay_roles_known(&self, roles: &Value) -> bool {
        roles.as_list_items().is_none_or(|items| {
            items.iter().all(|role| match role.view() {
                ValueView::Package(name) => self.registry().roles.contains_key(&*name.resolve()),
                _ => false,
            })
        })
    }

    /// The two subs of the replay entry: the actions builder and the resolver
    /// builder.
    // Cost: O(1) after the first call, which compiles the stubs.
    fn slang_replay_entry(&mut self) -> Result<(Value, Value), RuntimeError> {
        let mut entry = REPLAY_ENTRY.with(|e| e.borrow().clone());
        if entry.is_none() {
            let built = self.eval_code_string(&format!(
                "{}{REPLAY_BUILDER}",
                super::slang_host::HOST_STUBS
            ))?;
            REPLAY_ENTRY.with(|e| *e.borrow_mut() = Some(built.clone()));
            entry = Some(built);
        }
        let entry = entry.unwrap_or(Value::NIL);
        let subs = entry.as_list_items().unwrap_or_default();
        match (subs.first(), subs.get(1)) {
            (Some(build), Some(resolver)) => Ok((build.clone(), resolver.clone())),
            _ => Err(RuntimeError::new("slang action replay has no entry")),
        }
    }

    /// The replay resolver for a production standing in `package`.
    // Cost: O(1) plus the resolver's construction.
    pub(super) fn slang_replay_resolver(&mut self, package: Value) -> Result<Value, RuntimeError> {
        let (_, resolver) = self.slang_replay_entry()?;
        self.call_sub_value(resolver, vec![package])
    }

    /// Call each recorded action of `events` here, with the actions of `roles`,
    /// `$*R` bound to `resolver` (the stub one when `None`).
    // Cost: O(a) action calls, a = the recorded actions.
    pub(super) fn replay_action_events(
        &mut self,
        roles: Value,
        events: &[Value],
        resolver: Option<Value>,
    ) -> Result<Value, RuntimeError> {
        let (entry, _) = self.slang_replay_entry()?;
        let entry = Some(entry);
        // One actions class per set of roles, however many productions use it.
        let key = roles
            .as_list_items()
            .map(|items| items.iter().map(Value::to_string_value).collect::<Vec<_>>().join(","))
            .unwrap_or_default();
        let cached = BUILT.with(|b| b.borrow().iter().find(|(k, _)| *k == key).map(|(_, v)| v.clone()));
        let built = match cached {
            Some(built) => built,
            None => {
                let serial = CLASS_SERIAL.with(|s| {
                    s.set(s.get() + 1);
                    s.get()
                });
                let built = self.call_sub_value(
                    entry.unwrap_or(Value::NIL),
                    vec![roles, Value::str(format!("Mutsu::Slang::Replay::Actions{serial}"))],
                )?;
                BUILT.with(|b| b.borrow_mut().push((key, built.clone())));
                built
            }
        };
        let Some(parts) = built.as_list_items() else {
            return Err(RuntimeError::new("slang action replay built no actions"));
        };
        let actions = parts.first().cloned().unwrap_or(Value::NIL);
        let mut saved: Vec<(String, Option<Value>)> = Vec::new();
        let mut install = |interp: &mut Self, key: String, value: Value| {
            if !saved.iter().any(|(k, _)| *k == key) {
                saved.push((key.clone(), interp.env.get(&key).cloned()));
            }
            interp.env.insert(key, value);
        };
        let resolver = resolver.unwrap_or_else(|| parts.get(1).cloned().unwrap_or(Value::NIL));
        install(self, "*R".to_string(), resolver);
        install(self, "*CU".to_string(), parts.get(2).cloned().unwrap_or(Value::NIL));
        let _replay_unit =
            crate::unit_source_file::ReplayUnitGuard::enter(self.current_source_file_sym());
        let mut outcome = Ok(Value::NIL);
        let owner = Some(self.current_package());
        let previous_owner = REPLAY_OWNER.with(|o| std::mem::replace(&mut *o.borrow_mut(), owner));
        // The slang host already emitted the action's stdout and stderr when
        // it built this production. Replay is for its interpreter-local state;
        // buffering and dropping its writes keeps each user-visible message
        // at the parse-time position where the action originally wrote it.
        let (immediate, out_len, err_len, emitted) = {
            let sink = self.output_sink();
            (
                sink.immediate_stdout,
                sink.output.len(),
                sink.stderr_output.len(),
                sink.output_emitted,
            )
        };
        self.set_immediate_stdout(false);
        for event in events.iter() {
            let Some(event) = event.as_list_items() else {
                continue;
            };
            let method = event.first().map(Value::to_string_value).unwrap_or_default();
            let match_obj = event.get(1).cloned().unwrap_or(Value::NIL);
            let match_obj = self.unit_language_cursor(match_obj);
            if let Some(dynamics) = event.get(2).and_then(Value::as_list_items) {
                for dynamic in dynamics.iter() {
                    if let ValueView::Pair(name, value) = dynamic.view() {
                        for key in Self::grammar_dynvar_env_keys(name) {
                            install(self, key, value.clone());
                        }
                    }
                }
            }
            if let Err(error) = self.call_action_method(&actions, &method, &match_obj) {
                outcome = Err(error);
                break;
            }
        }
        {
            let mut sink = self.output_sink_mut();
            sink.immediate_stdout = immediate;
            sink.output.truncate(out_len);
            sink.stderr_output.truncate(err_len);
            sink.output_emitted = emitted;
        }
        REPLAY_OWNER.with(|o| *o.borrow_mut() = previous_owner);
        for (key, previous) in saved {
            match previous {
                Some(value) => self.env.insert(key, value),
                None => self.env.remove(&key),
            };
        }
        outcome
    }
}

impl Interpreter {
    /// A recorded Match as the replayed action sees it here: a cursor of the
    /// host's language grammar (a type the host minted, `Raku::Grammar` by
    /// name, unknown to this interpreter) becomes a cursor of this unit's
    /// `Raku::Grammar` (`raku_bases`), so its methods (`panic`, #16888) answer.
    // Cost: O(a), a = the cursor's attributes, when it is re-made; O(1) otherwise.
    fn unit_language_cursor(&mut self, match_obj: Value) -> Value {
        let ValueView::Instance {
            class_name,
            attributes,
            ..
        } = match_obj.view()
        else {
            return match_obj;
        };
        let name = class_name.resolve();
        // The host mints its grammar under the plain name the first time it
        // composes one, and under a minted `Raku::Grammar\0<id>` after that.
        let shown = if super::metamodel_new_type::is_minted_type_name(&name) {
            match name.split_once('\u{0}') {
                Some((shown, _)) => shown,
                None => return match_obj,
            }
        } else {
            name.as_str()
        };
        if shown != "Raku::Grammar"
            || self.registry().classes.contains_key(&*name)
            || self.ensure_raku_bases().is_err()
        {
            return match_obj;
        }
        Value::make_instance(Symbol::intern(shown), attributes.to_map())
    }
}
