use super::*;

impl Interpreter {
    /// Dispatch Seq.from-loop / Seq.from_loop
    pub(super) fn dispatch_seq_from_loop(
        &mut self,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        let mut positional: Vec<Value> = Vec::new();
        let mut label: Option<String> = None;
        let mut repeat = false;

        for arg in &args {
            if let Value::Pair(name, value) = arg {
                if name == "label" {
                    label = Some(value.to_string_value());
                    continue;
                }
                if name == "repeat" {
                    repeat = value.truthy();
                    continue;
                }
                continue;
            }
            if let Value::ValuePair(name, value) = arg {
                if let Value::Str(key) = name.as_ref() {
                    if key.as_str() == "label" {
                        label = Some(value.to_string_value());
                        continue;
                    }
                    if key.as_str() == "repeat" {
                        repeat = value.truthy();
                        continue;
                    }
                }
                continue;
            }
            positional.push(arg.clone());
        }

        let Some(body_callable) = positional.first().cloned() else {
            return Ok(Value::Seq(std::sync::Arc::new(Vec::new())));
        };
        let cond_callable = positional.get(1).cloned();
        let step_callable = positional.get(2).cloned();

        let label_matches = |error_label: &Option<String>| {
            error_label.as_deref() == label.as_deref() || error_label.is_none()
        };

        let mut items = Vec::new();
        let mut first_iteration = true;

        'from_loop: loop {
            if (!first_iteration || !repeat)
                && let Some(cond) = cond_callable.clone()
            {
                let cond_value = self.call_sub_value(cond, vec![], true)?;
                if !cond_value.truthy() {
                    break;
                }
            }
            first_iteration = false;

            'body_redo: loop {
                match self.call_sub_value(body_callable.clone(), vec![], true) {
                    Ok(value) => {
                        if !matches!(value, Value::Nil) {
                            items.push(value);
                        }
                        break 'body_redo;
                    }
                    Err(e) if e.is_redo && label_matches(&e.label) => continue 'body_redo,
                    Err(e) if e.is_next && label_matches(&e.label) => break 'body_redo,
                    Err(e) if e.is_last && label_matches(&e.label) => break 'from_loop,
                    Err(e) => return Err(e),
                }
            }

            if let Some(step) = step_callable.clone() {
                match self.call_sub_value(step, vec![], true) {
                    Ok(_) => {}
                    Err(e) if e.is_next && label_matches(&e.label) => continue 'from_loop,
                    Err(e) if e.is_redo && label_matches(&e.label) => continue 'from_loop,
                    Err(e) if e.is_last && label_matches(&e.label) => break 'from_loop,
                    Err(e) => return Err(e),
                }
            }
        }

        Ok(Value::Seq(std::sync::Arc::new(items)))
    }
}
