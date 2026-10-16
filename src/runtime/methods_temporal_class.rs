//! `Date.today` and `Date.new-from-daycount` (ADR-11276 §9.123), for `Date` and
//! a user subclass's type object. The rows are in
//! `builtins/method_table/instances/temporal_class.rs`; `DateTime.now` is
//! [`Interpreter::dispatch_datetime_now`].

use super::*;
use crate::builtins::methods_0arg::temporal;
use crate::symbol::Symbol;

impl Interpreter {
    /// The date `y-m-d` as an instance of the type object `target`: a `Date`
    /// directly, a subclass through its constructor.
    // Cost: O(1) for a `Date`; a subclass's constructor otherwise.
    fn date_of_class(
        &mut self,
        target: &Value,
        (y, m, d): (i64, i64, i64),
    ) -> Result<Value, RuntimeError> {
        if matches!(target.view(), ValueView::Package(name) if name == "Date") {
            return Ok(temporal::make_date(y, m, d));
        }
        let date_args = vec![
            Value::pair("year".to_string(), Value::int(y)),
            Value::pair("month".to_string(), Value::int(m)),
            Value::pair("day".to_string(), Value::int(d)),
        ];
        self.dispatch_new(target.clone(), date_args)
    }

    /// `Date.today`: today's date in the local timezone (`$*TZ`), as Rakudo
    /// does. Without the offset the date would be UTC, which differs from
    /// `DateTime.now.Date` in non-UTC zones.
    // Cost: O(1), plus a subclass's constructor.
    pub(crate) fn date_class_today(&mut self, target: &Value) -> Result<Value, RuntimeError> {
        let tz = self
            .env
            .get("*TZ")
            .and_then(|v| match v.view() {
                ValueView::Int(n) => Some(n),
                _ => None,
            })
            .unwrap_or(0i64);
        let secs = crate::value::current_time_secs_f64() as i64 + tz;
        let date = temporal::epoch_days_to_civil(secs.div_euclid(86400));
        self.date_of_class(target, date)
    }

    /// `Date.new-from-daycount($daycount)`: `.daycount` is the Modified Julian
    /// Day (epoch days + 40587), inverted to the civil date.
    // Cost: O(1), plus a subclass's constructor.
    pub(crate) fn date_class_from_daycount(
        &mut self,
        target: &Value,
        daycount: &Value,
    ) -> Result<Value, RuntimeError> {
        let date = temporal::epoch_days_to_civil(crate::runtime::to_int(daycount) - 40587);
        self.date_of_class(target, date)
    }

    /// The owner whose class methods the type object `target` answers:
    /// `Date` or `DateTime`, or a user class whose MRO reaches one of them.
    // Cost: O(m), m = the class's MRO (cached), for a user class; O(1) else.
    pub(crate) fn temporal_class_owners(
        &mut self,
        target: &Value,
    ) -> Option<&'static [&'static str]> {
        let ValueView::Package(name) = target.view() else {
            return None;
        };
        match name.as_str() {
            "Date" => return Some(&["Date"]),
            "DateTime" => return Some(&["DateTime"]),
            _ => {}
        }
        if !self.registry().classes.contains_key(name.as_str()) {
            return None;
        }
        let mro = self.class_mro(name.as_str());
        if mro.contains(&Symbol::intern("DateTime")) {
            Some(&["DateTime"])
        } else if mro.contains(&Symbol::intern("Date")) {
            Some(&["Date"])
        } else {
            None
        }
    }
}
