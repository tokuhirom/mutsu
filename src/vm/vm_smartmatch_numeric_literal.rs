//! `$topic ~~ 2` with an `Int`/`Num` literal on the right (#16750).
//!
//! Rakudo compiles a smartmatch whose RHS is written as an `Int` or `Num`
//! literal (`~~ 2`, `~~ -2`, `~~ 2e0`, `when 2`) as a numeric comparison
//! against the topic's `.Numeric(:fail-or-nil)`, so:
//!
//! - a `Failure` topic throws its exception;
//! - a defined topic whose type declares no `Numeric` — a `Block`/`Sub`, a
//!   `Pair`, a bare `Mu` or `Exception`, an instance of a plain user class
//!   — dies with `X::Multi::NoMatch` (`Mu.Numeric` has only its `:U`
//!   candidate; `Cool` adds none);
//! - everything else compares as before.
//!
//! The same match against a variable (`$t ~~ $two`) goes through
//! `Int.ACCEPTS` and answers `False` for those topics, which is why this is a
//! compile-time flag on the opcode, not a property of the RHS value.

use super::*;

impl Interpreter {
    /// `Err` when matching `topic` against a numeric literal dies in Rakudo
    /// (see the module docs); `Ok(())` lets the ordinary smartmatch run.
    // Cost: O(1) for a non-instance topic; an instance adds the memoized
    // method and role probes of `instance_has_no_numeric`.
    pub(super) fn numeric_literal_topic_check(
        &mut self,
        topic: &Value,
    ) -> Result<(), RuntimeError> {
        let topic = topic.deref_container();
        let type_name = match topic.view() {
            ValueView::Instance { class_name, .. } if class_name == "Failure" => {
                return Self::sink_failure_to_error(topic.clone()).map(|_| ());
            }
            ValueView::Sub(_) | ValueView::WeakSub(_) | ValueView::Routine { .. } => {
                crate::runtime::utils::value_type_name(&topic).to_string()
            }
            ValueView::Pair(..) | ValueView::ValuePair(..) => "Pair".to_string(),
            ValueView::Instance { class_name, .. } if self.instance_has_no_numeric(&topic) => {
                crate::value::user_facing_type_name(class_name.resolve().as_str()).into_owned()
            }
            _ => return Ok(()),
        };
        Err(crate::runtime::mu_coercion_no_match_error_named(
            "Numeric",
            &type_name,
            ":fail-or-nil",
        ))
    }

    /// Whether the instance `topic` has no defined `Numeric`: a bare `Mu` or
    /// `Exception`, or an object of a user class (an exception class too) that neither declares
    /// `Numeric`/`Bridge` nor does `Numeric` (`Cool` declares no `Numeric`
    /// of its own: `class C is Cool {}; C.new ~~ 2` dies too). Other built-in
    /// instances keep their own coercions.
    // Cost: O(d) on a cold memo, d = MRO depth; O(1) amortized.
    fn instance_has_no_numeric(&mut self, topic: &Value) -> bool {
        let ValueView::Instance { class_name, .. } = topic.view() else {
            return false;
        };
        let class = class_name.resolve();
        // A built-in `X::*` keeps its own coercions (`X::AdHoc ~~ 2` is
        // `False`); a bare `Exception` and a user exception class have none.
        if class == "Mu" || class == "Exception" {
            return true;
        }
        self.types.user_declared_classes.contains(class.as_str())
            && !self.has_user_method(&class, "Numeric")
            && !self.has_user_method(&class, "Bridge")
            && !self.type_matches_value("Numeric", topic)
    }
}
