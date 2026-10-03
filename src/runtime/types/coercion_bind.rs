//! Binding a value to a parameter whose type is a coercion type (`Int()`,
//! `Int(Str)`), shared by signature binding and `for` loop parameters.

use super::*;
use crate::value::ValueView;

/// Why a coercion-typed parameter refused its value. Each caller decorates the
/// error its own way (a signature attaches the `Parameter` object).
pub(crate) enum CoercionBindError {
    /// The value matches neither the source type nor the target type.
    TypeCheck(RuntimeError),
    /// The coercion itself threw.
    Coerce(RuntimeError),
    /// The coercion ran, but its result is not the target type.
    Impossible(RuntimeError),
}

impl Interpreter {
    /// Coerce `value` for a parameter declared with the coercion type
    /// `constraint` (`target(source)`). A `T(S)` parameter accepts a value that
    /// is already a `T` as well as an `S`. A Failure produced by the coercion is
    /// passed through as is; it throws when sunk or used.
    // Cost: O(1) plus the coercion method the target type runs.
    pub(crate) fn bind_coercion_param_value(
        &mut self,
        display_name: &str,
        constraint: &str,
        target: &str,
        source: Option<&str>,
        value: Value,
    ) -> Result<Value, CoercionBindError> {
        if let Some(src) = source
            && !self.type_matches_value(src, &value)
            && !self.type_matches_value(target, &value)
        {
            return Err(CoercionBindError::TypeCheck(
                self.typecheck_binding_parameter_failure(display_name, constraint, &value),
            ));
        }
        let original = value.clone();
        let value = self
            .try_coerce_value_for_constraint(constraint, value)
            .map_err(CoercionBindError::Coerce)?;
        if !matches!(value.view(), ValueView::Instance { class_name, .. } if class_name.resolve() == "Failure")
            && !self.type_matches_value(target, &value)
        {
            return Err(CoercionBindError::Impossible(coerce_impossible_error(
                constraint, &original,
            )));
        }
        Ok(value)
    }
}
