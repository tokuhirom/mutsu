use super::*;
use crate::symbol::Symbol;

impl Interpreter {
    pub(super) fn normalize_categorical_operator_name(name: &str) -> String {
        // Keep categorical operator names as-written. Parenthesized operators
        // like infix:<(==)> are distinct from infix:<==>.
        name.to_string()
    }

    pub(super) fn inferred_operator_arity(name: &str) -> Option<usize> {
        if name.starts_with("infix:<") && name.ends_with('>') {
            return Some(2);
        }
        if (name.starts_with("prefix:<") || name.starts_with("postfix:<")) && name.ends_with('>') {
            return Some(1);
        }
        None
    }

    fn callable_return_type_inner(callable: &Value) -> Option<String> {
        match callable {
            Value::Sub(data) => match data.env.get("__mutsu_return_type") {
                Some(Value::Str(rt)) => Some(rt.to_string()),
                _ => None,
            },
            _ => None,
        }
    }

    pub(crate) fn callable_return_type(&self, callable: &Value) -> Option<String> {
        Self::callable_return_type_inner(callable)
    }

    pub(crate) fn routine_return_spec_by_name(&self, name: &str) -> Option<String> {
        let code_key = format!("&{}", name);
        for key in [code_key.as_str(), name] {
            if let Some(Value::Sub(data)) = self.env.get(key)
                && let Some(Value::Str(spec)) = data.env.get("__mutsu_return_type")
            {
                return Some(spec.to_string());
            }
        }
        // Also check the FunctionDef registry
        if let Some(def) = self.resolve_function(name)
            && let Some(ref rt) = def.return_type
        {
            return Some(rt.clone());
        }
        None
    }

    fn return_spec_scalar_name(spec: &str) -> Option<String> {
        let s = spec.trim();
        let rest = s.strip_prefix('$')?;
        if rest.is_empty()
            || !rest
                .chars()
                .all(|c| c.is_ascii_alphanumeric() || c == '_' || c == '-')
        {
            return None;
        }
        Some(rest.to_string())
    }

    pub(crate) fn is_definite_return_spec(&self, spec: &str) -> bool {
        let s = spec.trim();
        if s.is_empty() {
            return false;
        }
        // Coercion types like Str(Numeric:D) or Foo:D() are type constraints, not definite returns
        if s.contains('(') && s.ends_with(')') {
            return false;
        }
        // Parameterized types like Positional[Int] or Array[Str] are type
        // constraints, not definite return-value expressions.
        if s.contains('[') && s.ends_with(']') {
            return false;
        }
        if Self::return_spec_scalar_name(s).is_some() {
            return true;
        }
        if matches!(s, "Nil" | "True" | "False" | "Empty" | "pi") {
            return true;
        }
        if s.starts_with('\"')
            || s.starts_with('\'')
            || s.chars().next().is_some_and(|c| c.is_ascii_digit())
            || (s.starts_with('-') && s[1..].chars().next().is_some_and(|c| c.is_ascii_digit()))
        {
            return true;
        }
        if crate::runtime::utils::is_known_type_constraint(s) || self.has_type(s) {
            return false;
        }
        if let Some(base) = s.strip_suffix(":D").or_else(|| s.strip_suffix(":U"))
            && (crate::runtime::utils::is_known_type_constraint(base) || self.has_type(base))
        {
            return false;
        }
        if s.chars().next().is_some_and(|c| c.is_ascii_uppercase())
            && s.chars()
                .all(|c| c.is_ascii_alphanumeric() || c == ':' || c == '_')
        {
            return false;
        }
        true
    }

    fn failure_value_to_error(exception: &Value) -> RuntimeError {
        let message = if let Value::Instance { attributes, .. } = exception {
            attributes
                .as_map()
                .get("message")
                .map(|v| v.to_string_value())
                .unwrap_or_else(|| "Died".to_string())
        } else {
            "Died".to_string()
        };
        let mut err = RuntimeError::new(&message);
        err.exception = Some(Box::new(exception.clone()));
        err
    }

    pub(crate) fn failure_to_runtime_error_if_unhandled(
        &self,
        value: &Value,
    ) -> Option<RuntimeError> {
        let Value::Instance {
            class_name,
            attributes,
            ..
        } = value
        else {
            return None;
        };
        if class_name != "Failure" {
            return None;
        }
        if value.is_failure_handled() {
            return None;
        }
        if let Some(exception) = attributes.as_map().get("exception") {
            return Some(Self::failure_value_to_error(exception));
        }
        Some(RuntimeError::new("Failed"))
    }

    pub(crate) fn fail_error_to_failure_value(&self, err: &RuntimeError) -> Value {
        let exception = err.exception.as_deref().cloned().unwrap_or_else(|| {
            let mut attrs = std::collections::HashMap::new();
            attrs.insert("message".to_string(), Value::str(err.message.clone()));
            Value::make_instance(Symbol::intern("X::AdHoc"), attrs)
        });
        let mut failure_attrs = std::collections::HashMap::new();
        failure_attrs.insert("exception".to_string(), exception);
        // When UNDO phasers ran for this fail, the Failure is marked as handled
        // (Raku semantics: UNDO acts as a handler, so sinking the Failure won't throw).
        failure_attrs.insert("handled".to_string(), Value::Bool(err.fail_handled));
        let failure = Value::make_instance(Symbol::intern("Failure"), failure_attrs);
        // Register in the pending failure registry for $!.pending support
        if let Value::Instance { id, .. } = &failure {
            crate::value::register_pending_failure(*id, failure.clone());
        }
        failure
    }

    fn malformed_return_value_error(&self, value: &Value) -> RuntimeError {
        if let Value::Instance {
            class_name,
            attributes,
            ..
        } = value
            && class_name == "Failure"
            && let Some(ex) = attributes.as_map().get("exception")
        {
            return Self::failure_value_to_error(ex);
        }
        RuntimeError::new("Malformed return value")
    }

    fn evaluate_definite_return_value(&mut self, spec: &str) -> Result<Value, RuntimeError> {
        if let Some(name) = Self::return_spec_scalar_name(spec) {
            return Ok(self.env.get(&name).cloned().unwrap_or(Value::Nil));
        }
        self.eval_eval_string(spec.trim())
    }

    fn sink_for_definite_return(&mut self, value: &Value) -> Result<(), RuntimeError> {
        match value {
            Value::LazyList(list) => {
                let items = self.force_lazy_list(list)?;
                for item in &items {
                    self.sink_for_definite_return(item)?;
                }
                Ok(())
            }
            _ if value.as_list_items().is_some() => {
                for item in value.as_list_items().unwrap().iter() {
                    self.sink_for_definite_return(item)?;
                }
                Ok(())
            }
            _ => Ok(()),
        }
    }

    pub(crate) fn prepare_definite_return_slot(&mut self, return_spec: Option<&str>) {
        let Some(spec) = return_spec else {
            return;
        };
        if !self.is_definite_return_spec(spec) {
            return;
        }
        if let Some(name) = Self::return_spec_scalar_name(spec) {
            self.env.insert(name, Value::Nil);
        }
    }

    pub(crate) fn finalize_return_with_spec(
        &mut self,
        result: Result<Value, RuntimeError>,
        return_spec: Option<&str>,
    ) -> Result<Value, RuntimeError> {
        let Some(spec) = return_spec else {
            return match result {
                Ok(v) => Ok(v),
                Err(e) if e.return_value.is_some() => Ok(e.return_value.unwrap()),
                Err(e) => Err(e),
            };
        };
        if !self.is_definite_return_spec(spec) {
            let value = match result {
                Ok(v) => v,
                Err(e) if e.return_value.is_some() => e.return_value.unwrap(),
                Err(e) => return Err(e),
            };
            return self.enforce_return_type_constraint(spec, value);
        }

        match result {
            Ok(v) => {
                self.sink_for_definite_return(&v)?;
                self.evaluate_definite_return_value(spec)
            }
            Err(e) if e.return_value.is_some() => {
                let explicit = e.return_value.unwrap();
                if !matches!(explicit, Value::Nil) {
                    return Err(self.malformed_return_value_error(&explicit));
                }
                self.evaluate_definite_return_value(spec)
            }
            Err(e) => Err(e),
        }
    }

    /// Create an X::TypeCheck::Return exception
    fn throw_type_check_return(&self, expected: &str, got: &Value) -> RuntimeError {
        let got_type = Self::display_type_name(got);
        let got_gist = Self::display_gist(got);
        let msg = format!(
            "Type check failed for return value; expected {} but got {} ({})",
            expected, got_type, got_gist
        );
        let mut attrs = std::collections::HashMap::new();
        attrs.insert("message".to_string(), Value::str(msg.clone()));
        attrs.insert("got".to_string(), got.clone());
        attrs.insert(
            "expected".to_string(),
            Value::Package(Symbol::intern(expected)),
        );
        let exception = Value::make_instance(Symbol::intern("X::TypeCheck::Return"), attrs);
        let mut err = RuntimeError::new(msg);
        err.exception = Some(Box::new(exception));
        err
    }

    /// Get the display type name for error messages (Package shows its name)
    fn display_type_name(value: &Value) -> String {
        match value {
            Value::Package(name) => name.resolve().to_string(),
            _ => super::utils::value_type_name(value).to_string(),
        }
    }

    /// Get a gist-like representation for error messages
    fn display_gist(value: &Value) -> String {
        match value {
            Value::Package(name) => name.resolve().to_string(),
            Value::Str(s) => format!("\"{}\"", s),
            Value::Int(i) => i.to_string(),
            Value::BigInt(n) => n.to_string(),
            Value::Num(n) => format!("{}", n),
            Value::Bool(b) => if *b { "True" } else { "False" }.to_string(),
            Value::Rat(n, d) => format!("{}", *n as f64 / *d as f64),
            Value::Nil => "Nil".to_string(),
            _ => value.to_string_value(),
        }
    }

    /// Create an X::Coerce::Impossible exception
    fn throw_coerce_impossible(&self, target: &str, got: &Value) -> RuntimeError {
        let msg = format!(
            "Impossible coercion from '{}' into '{}': no acceptable coercion method found",
            super::utils::value_type_name(got),
            target
        );
        let mut attrs = std::collections::HashMap::new();
        attrs.insert("message".to_string(), Value::str(msg.clone()));
        let exception = Value::make_instance(Symbol::intern("X::Coerce::Impossible"), attrs);
        let mut err = RuntimeError::new(msg);
        err.exception = Some(Box::new(exception));
        err
    }

    /// Enforce a return type constraint on a return value.
    /// Handles coercion types like Str(Numeric:D), Foo:D(), and subset types.
    fn enforce_return_type_constraint(
        &mut self,
        spec: &str,
        value: Value,
    ) -> Result<Value, RuntimeError> {
        let resolved_spec = self.resolved_type_capture_name(spec);
        let spec = resolved_spec.as_str();
        // Nil and Failure pass through unconditionally
        if matches!(value, Value::Nil) || Self::is_failure_value(&value) {
            return Ok(value);
        }

        // Check if this is a subset type
        let subset = self.registry().subsets.get(spec).cloned();
        if let Some(subset) = subset {
            // For subsets with coercion base types, coerce first then check predicate
            let coerced = if subset.base.contains('(') && subset.base.ends_with(')') {
                self.enforce_coercion_return(&subset.base, value)?
            } else {
                // Non-coercion subset: just check the base type
                if !self.type_matches_value(&subset.base, &value) {
                    return Err(self.throw_type_check_return(spec, &value));
                }
                value
            };
            // Check the where predicate if present
            if let Some(ref pred) = subset.predicate {
                let pred_clone = pred.clone();
                if !self.check_where_constraint(&pred_clone, &coerced) {
                    return Err(self.throw_type_check_return(spec, &coerced));
                }
            }
            return Ok(coerced);
        }

        // Check if this is a coercion type like Str(Numeric:D) or Foo:D()
        if spec.contains('(') && spec.ends_with(')') {
            return self.enforce_coercion_return(spec, value);
        }

        // A parameterized Positional/Array/List return type checks the array's
        // DECLARED element type strictly: an untyped array does not satisfy
        // `Positional[Int]` even if its current values happen to be Int. (Elsewhere
        // `~~` is lenient for empty/untyped arrays; the return contract is strict.)
        if let Some(open) = spec.find('[')
            && spec.ends_with(']')
            && matches!(&spec[..open], "Positional" | "Array" | "List")
            && matches!(&value, Value::Array(..))
        {
            let inner = &spec[open + 1..spec.len() - 1];
            let (inner_base, _) = super::types::strip_type_smiley(inner);
            let ok = match self.container_type_metadata(&value) {
                Some(meta) if !meta.value_type.is_empty() => {
                    let (mvt_base, _) = super::types::strip_type_smiley(&meta.value_type);
                    Self::type_matches(inner_base, mvt_base)
                        || inner_base == "Mu"
                        || inner_base == "Any"
                }
                _ => inner_base == "Mu" || inner_base == "Any",
            };
            if !ok {
                return Err(self.throw_type_check_return(spec, &value));
            }
            return Ok(value);
        }

        // Plain type constraint (e.g., Str, Int:D)
        if !self.type_matches_value(spec, &value) {
            return Err(self.throw_type_check_return(spec, &value));
        }
        Ok(value)
    }

    /// Enforce a coercion return type like Str(Numeric:D) or Foo:D().
    fn enforce_coercion_return(&mut self, spec: &str, value: Value) -> Result<Value, RuntimeError> {
        // Parse the coercion type: strip smiley from the whole spec first
        let (spec_no_smiley, outer_smiley) = super::types::strip_type_smiley(spec);
        let (full_target, source) = if let Some(open) = spec_no_smiley.find('(')
            && spec_no_smiley.ends_with(')')
        {
            let target = &spec_no_smiley[..open];
            let src = &spec_no_smiley[open + 1..spec_no_smiley.len() - 1];
            (target, if src.is_empty() { None } else { Some(src) })
        } else if let Some(open) = spec.find('(')
            && spec.ends_with(')')
        {
            let target = &spec[..open];
            let src = &spec[open + 1..spec.len() - 1];
            (target, if src.is_empty() { None } else { Some(src) })
        } else {
            return Ok(value);
        };

        // Determine definite requirement from target smiley
        let (base_target, target_smiley) = super::types::strip_type_smiley(full_target);
        let requires_definite = target_smiley == Some(":D") || outer_smiley == Some(":D");

        // If there's a source constraint, check it
        if let Some(src) = source {
            // The value must match the source type, OR already be of the target type
            if !self.type_matches_value(src, &value)
                && !self.type_matches_value(base_target, &value)
            {
                return Err(self.throw_type_check_return(spec, &value));
            }
        }

        // If value already matches target type, return it directly
        if self.type_matches_value(full_target, &value) {
            return Ok(value);
        }

        // Try to coerce the value
        let coerced = self.try_coerce_value_for_return(base_target, value.clone())?;

        // Check if coercion produced a valid result
        if requires_definite && !super::types::value_is_defined(&coerced) {
            return Err(self.throw_coerce_impossible(spec, &value));
        }

        // Check if the coerced value actually matches the target type
        if !self.type_matches_value(base_target, &coerced) {
            return Err(self.throw_coerce_impossible(spec, &value));
        }

        // For subset targets, check the where constraint
        let subset = self.registry().subsets.get(base_target).cloned();
        if let Some(subset) = subset
            && let Some(ref pred) = subset.predicate
        {
            let pred_clone = pred.clone();
            if !self.check_where_constraint(&pred_clone, &coerced) {
                return Err(self.throw_coerce_impossible(spec, &value));
            }
        }

        Ok(coerced)
    }

    /// Try to coerce a value for a return type constraint.
    /// For subset types, coerces to the subset's base type first.
    fn try_coerce_value_for_return(
        &mut self,
        target: &str,
        value: Value,
    ) -> Result<Value, RuntimeError> {
        // If target is a subset, coerce to the subset's base type
        let subset = self.registry().subsets.get(target).cloned();
        if let Some(subset) = subset {
            let (base, _) = super::types::strip_type_smiley(&subset.base);
            return self.try_coerce_value_for_constraint(&format!("{}()", base), value);
        }
        self.try_coerce_value_for_constraint(&format!("{}()", target), value)
    }

    /// Check a where constraint against a value
    fn check_where_constraint(&mut self, pred: &Expr, value: &Value) -> bool {
        // Evaluate the predicate expression to get a callable value
        let pred_val = match self.eval_block_value(&[Stmt::Expr(pred.clone())]) {
            Ok(v) => v,
            Err(_) => return false,
        };
        // Call the predicate with the value
        match self.call_sub_value(pred_val, vec![value.clone()], false) {
            Ok(result) => result.truthy(),
            Err(_) => false,
        }
    }
}
