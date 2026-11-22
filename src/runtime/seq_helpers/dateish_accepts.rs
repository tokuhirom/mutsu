//! `Date.ACCEPTS` and `DateTime.ACCEPTS` for those classes and their
//! subclasses (#18818).

use crate::value::temporal_core::{date_attrs, datetime_attrs, datetime_to_posix};
use crate::value::{Value, ValueView};

impl crate::runtime::Interpreter {
    /// `topic ~~ matcher` when `matcher` is a `Date` or `DateTime` instance,
    /// or an instance of a subclass of either; `None` otherwise.
    ///
    /// As in rakudo:
    /// - a `Date` matcher accepts a `DateTime` on the same calendar day, and a
    ///   `Date` of its own type on the same day (a Date is a value type, so the
    ///   type is part of the value: `T.new(…) ~~ Date.new(…)` is False);
    /// - a `DateTime` matcher accepts a `DateTime` naming the same instant,
    ///   whatever its time zone or subclass, and never a `Date`.
    // Cost: O(m), m = the matcher's (and topic's) MRO length.
    pub(crate) fn dateish_accepts(&mut self, topic: &Value, matcher: &Value) -> Option<bool> {
        let matcher_owner = self.temporal_instance_owners(matcher)?[0];
        let topic_owner = self.temporal_instance_owners(topic).map(|o| o[0]);
        let (ValueView::Instance {
            class_name: topic_class,
            attributes: topic_attrs,
            ..
        }, ValueView::Instance {
            class_name: matcher_class,
            attributes: matcher_attrs,
            ..
        }) = (topic.view(), matcher.view())
        else {
            return None;
        };
        let accepted = match (matcher_owner, topic_owner) {
            ("Date", Some("DateTime")) => {
                let (y, m, d, ..) = datetime_attrs(&topic_attrs.as_map());
                (y, m, d) == date_attrs(&matcher_attrs.as_map())
            }
            ("Date", Some("Date")) => {
                topic_class == matcher_class
                    && date_attrs(&topic_attrs.as_map()) == date_attrs(&matcher_attrs.as_map())
            }
            ("DateTime", Some("DateTime")) => {
                let (y, mo, d, h, mi, s, tz) = datetime_attrs(&topic_attrs.as_map());
                let (ry, rmo, rd, rh, rmi, rs, rtz) = datetime_attrs(&matcher_attrs.as_map());
                datetime_to_posix(y, mo, d, h, mi, s, tz)
                    == datetime_to_posix(ry, rmo, rd, rh, rmi, rs, rtz)
            }
            _ => false,
        };
        Some(accepted)
    }
}
