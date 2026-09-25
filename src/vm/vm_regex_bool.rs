//! `Regex.Bool`: boolifying a regex value matches it against a topic.
//!
//! Rakudo's `Regex.Bool` matches against the `$_` of the scope the regex
//! literal was written in, not the caller's: `my &f = { /foo/ }; so f("foo")`
//! is `True`, because the block's `$_` (bound to the argument) is the regex's
//! lexical topic. A literal whose value escapes carries that `$_`'s container
//! (`RegexClosure::topic`, filled by `OpCode::LoadRegexClosure` through
//! [`Interpreter::topic_container_cell`]), so a later assignment to it is seen
//! and a rebind (a `for` loop, a routine's own `$_`) is not; any other regex
//! falls back to the `$_` visible where it is boolified.
//!
//! Every boolification site — `?$re` / `if $re` (`eval_truthy`), `$re.Bool` /
//! `$re.so` through either method-call opcode — goes through
//! [`Interpreter::regex_bool`], so they cannot disagree about which topic wins.

use crate::runtime::Interpreter;
use crate::value::{Value, ValueView};

impl Interpreter {
    /// Whether `val` is a regex value whose truthiness is a match against a
    /// topic (a regex literal, an adverbed one, or a `regex`/`token` routine).
    pub(crate) fn is_boolifying_regex(val: &Value) -> bool {
        matches!(
            val.view(),
            ValueView::Regex(_)
                | ValueView::RegexWithAdverbs { .. }
                | ValueView::Routine { is_regex: true, .. }
        )
    }

    /// Boolify a regex value (see the module doc). `quiet` coerces an
    /// undefined *implicit* topic silently, as a bare `/regex/` does (see
    /// `quiet_topic_for_regex_match`).
    // Cost: O(m), m = one regex match against the topic.
    pub(crate) fn regex_bool(&mut self, regex: &Value, quiet: bool) -> bool {
        let topic = regex
            .regex_captured_topic()
            .or_else(|| self.env().get("_").cloned())
            .map(|t| t.deref_container())
            .unwrap_or(Value::NIL);
        let topic = if quiet {
            self.quiet_topic_for_regex_match(topic)
        } else {
            topic
        };
        self.vm_smart_match(&topic, regex)
    }

    /// The current frame's `$_` container, for a regex literal to capture
    /// (`OpCode::LoadRegexClosure`). The first capture boxes the env-held `$_`
    /// into a shared `ContainerRef` cell and rebinds `_` to it, so a later
    /// assignment (`$_ = "foo"`, which writes through the cell) is seen by the
    /// regex, while a `for`/`given`/routine topic *binding* (`SetTopic`, a new
    /// frame) installs a different value and leaves the captured cell alone --
    /// Rakudo's container-not-value topic capture (issue #9396).
    // Cost: O(1), one env probe plus at most one cell allocation.
    pub(crate) fn topic_container_cell(&mut self) -> Value {
        let cur = self.env().get("_").cloned();
        if let Some(cell) = cur.as_ref().filter(|v| v.is_container_ref()) {
            return cell.clone();
        }
        let cell = cur
            .unwrap_or_else(|| Value::package(crate::symbol::Symbol::intern("Any")))
            .into_container_ref();
        self.env_mut().insert("_".to_string(), cell.clone());
        cell
    }

    /// Record an assignment to `$_` for the map rw writeback.
    // Cost: O(1).
    pub(crate) fn note_rw_map_topic(&mut self, val: &Value) {
        self.env_mut()
            .insert("__mutsu_rw_map_topic__".to_string(), val.clone());
    }

    /// The live write-back of an assignment to `$_` into the scalar variable a
    /// `given $x` / `with $x` topic aliases (`topic_source_var`).
    // Cost: O(1) env/slot writes.
    pub(crate) fn write_topic_to_source_var(
        &mut self,
        code: &crate::opcode::CompiledCode,
        val: &Value,
    ) {
        if Self::is_topic_ro_assignment(val) {
            return;
        }
        let Some(source_name) = self.topic_source_var.clone() else {
            return;
        };
        // A sigiled "$h" tag is the deref'd-container source (`for @$h`): the
        // per-element loop writeback owns it; the whole-topic scalar write
        // would pollute a "$h" env key.
        if source_name.starts_with('@')
            || source_name.starts_with('%')
            || source_name.starts_with('$')
        {
            return;
        }
        self.set_env_with_main_alias(&source_name, val.clone());
        self.update_local_if_exists(code, &source_name, val);
        // An attribute topic (`with $!result { .PQclear; $_ = Nil }` --
        // DBDish::Pg's StatementHandle.finish) must reach self's attribute
        // cell, not just the env mirror: the stale cell otherwise keeps the
        // freed C pointer and the next finish double-frees it (SEGV).
        if Self::attr_twigil_base(&source_name).is_some()
            && !Self::is_non_mirrorable_attr_value(val)
        {
            self.write_self_attr_cell(&source_name, val.clone());
        }
    }

    /// The `$re.Bool` / `$re.so` method forms, shared by `CallMethod` and
    /// `CallMethodMut`. `None` when `method`/`target` is not that call.
    // Cost: O(m), m = one regex match against the topic (O(1) when declined).
    pub(crate) fn try_regex_bool_method(
        &mut self,
        target: &Value,
        method: &str,
        args: &[Value],
    ) -> Option<Value> {
        if !matches!(method, "Bool" | "so")
            || !args.is_empty()
            || !Self::is_boolifying_regex(target)
        {
            return None;
        }
        Some(Value::truth(self.regex_bool(target, false)))
    }
}
