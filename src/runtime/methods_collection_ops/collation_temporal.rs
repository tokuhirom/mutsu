use super::*;

impl Interpreter {
    /// Dispatch methods on Collation instances (set, primary, secondary, tertiary, quaternary).
    pub(in crate::runtime) fn dispatch_collation_method(
        &mut self,
        target: Value,
        method: &str,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let Value::Instance {
            class_name,
            attributes,
            id,
        } = &target
        else {
            return Err(RuntimeError::new(
                "Collation method called on non-Collation value",
            ));
        };
        debug_assert!(class_name == "Collation");

        match method {
            "primary" | "secondary" | "tertiary" | "quaternary" if args.is_empty() => {
                Ok(attributes.get(method).cloned().unwrap_or(Value::Int(1)))
            }
            "set" => {
                // .set accepts named arguments: primary, secondary, tertiary, quaternary
                // Each can be -1, 0, 1, or Bool (False=0, True=1)
                let mut new_attrs = (**attributes).clone();

                for arg in args {
                    if let Value::Pair(key, val) = arg {
                        let int_val = match val.as_ref() {
                            Value::Bool(b) => {
                                if *b {
                                    1
                                } else {
                                    0
                                }
                            }
                            Value::Int(n) => *n,
                            other => crate::runtime::utils::to_int(other),
                        };
                        match key.as_str() {
                            "primary" | "secondary" | "tertiary" | "quaternary" => {
                                new_attrs.insert(key.clone(), Value::Int(int_val));
                            }
                            _ => {}
                        }
                    }
                }

                let result = Value::make_instance_with_id(*class_name, new_attrs, *id);
                // Also update the target in-place (Collation.set mutates and returns self)
                Ok(result)
            }
            "gist" => {
                let settings = crate::builtins::collation::CollationSettings::from_value(&target);
                let level = settings.collation_level();
                Ok(Value::str(format!(
                    "collation-level => {}, Country => International, Language => None, primary => {}, secondary => {}, tertiary => {}, quaternary => {}",
                    level,
                    settings.primary,
                    settings.secondary,
                    settings.tertiary,
                    settings.quaternary
                )))
            }
            _ => Err(RuntimeError::new(format!(
                "Unknown Collation method '{}'",
                method
            ))),
        }
    }

    pub(in crate::runtime) fn dispatch_collate(
        &mut self,
        target: Value,
    ) -> Result<Value, RuntimeError> {
        use crate::builtins::collation::{CollationSettings, collate_sort};

        // Get $*COLLATION settings
        let settings = self
            .get_dynamic_var("*COLLATION")
            .ok()
            .or_else(|| self.env.get("$*COLLATION").cloned())
            .map(|v| CollationSettings::from_value(&v))
            .unwrap_or_default();

        match target {
            Value::Package(class_name) if class_name == "Supply" => {
                Ok(Value::Seq(Arc::new(vec![Value::Package(class_name)])))
            }
            Value::Instance {
                class_name,
                attributes,
                ..
            } if class_name == "Supply" => {
                let values = match attributes.get("values") {
                    Some(Value::Array(items, ..)) => items.to_vec(),
                    _ => Vec::new(),
                };
                let sorted = collate_sort(values, &settings);
                let mut attrs = HashMap::new();
                attrs.insert("values".to_string(), Value::array(sorted));
                attrs.insert("taps".to_string(), Value::array(Vec::new()));
                attrs.insert("live".to_string(), Value::Bool(false));
                Ok(Value::make_instance(Symbol::intern("Supply"), attrs))
            }
            Value::Array(items, ..) => Ok(Value::Seq(Arc::new(collate_sort(
                items.to_vec(),
                &settings,
            )))),
            other => {
                let values = Self::value_to_list(&other);
                Ok(Value::Seq(Arc::new(collate_sort(values, &settings))))
            }
        }
    }

    #[allow(dead_code)]
    pub(in crate::runtime) fn civil_to_epoch_days(year: i64, month: i64, day: i64) -> i64 {
        let y = year - i64::from(month <= 2);
        let era = if y >= 0 { y } else { y - 399 } / 400;
        let yoe = y - era * 400;
        let mp = month + if month > 2 { -3 } else { 9 };
        let doy = (153 * mp + 2) / 5 + day - 1;
        let doe = yoe * 365 + yoe / 4 - yoe / 100 + doy;
        era * 146_097 + doe - 719_468
    }

    #[allow(dead_code)]
    pub(in crate::runtime) fn leap_seconds_before_day(epoch_days: i64) -> i64 {
        const LEAP_EFFECTIVE_DATES: &[(i64, i64, i64)] = &[
            (1972, 7, 1),
            (1973, 1, 1),
            (1974, 1, 1),
            (1975, 1, 1),
            (1976, 1, 1),
            (1977, 1, 1),
            (1978, 1, 1),
            (1979, 1, 1),
            (1980, 1, 1),
            (1981, 7, 1),
            (1982, 7, 1),
            (1983, 7, 1),
            (1985, 7, 1),
            (1988, 1, 1),
            (1990, 1, 1),
            (1991, 1, 1),
            (1992, 7, 1),
            (1993, 7, 1),
            (1994, 7, 1),
            (1996, 1, 1),
            (1997, 7, 1),
            (1999, 1, 1),
            (2006, 1, 1),
            (2009, 1, 1),
            (2012, 7, 1),
            (2015, 7, 1),
            (2017, 1, 1),
        ];
        LEAP_EFFECTIVE_DATES
            .iter()
            .filter(|&&(y, m, d)| Self::civil_to_epoch_days(y, m, d) <= epoch_days)
            .count() as i64
    }

    #[allow(dead_code)]
    pub(in crate::runtime) fn date_days_to_epoch_with_leap_seconds(days: i64) -> f64 {
        (days * 86_400 + Self::leap_seconds_before_day(days)) as f64
    }
}
