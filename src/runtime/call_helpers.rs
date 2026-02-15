use super::*;

impl Interpreter {
    pub(crate) fn exec_call_values(
        &mut self,
        name: &str,
        args: Vec<Value>,
    ) -> Result<(), RuntimeError> {
        match self.call_function(name, args.clone()) {
            Ok(_) => Ok(()),
            Err(e)
                if e.message
                    .starts_with("Unknown function (call_function fallback disabled):") =>
            {
                self.exec_call(name, args)
            }
            Err(e) => Err(e),
        }
    }

    pub(crate) fn exec_call_pairs_values(
        &mut self,
        name: &str,
        args: Vec<Value>,
    ) -> Result<(), RuntimeError> {
        self.exec_call(name, args)
    }

    pub(crate) fn test_ok(
        &mut self,
        success: bool,
        desc: &str,
        todo: bool,
    ) -> Result<(), RuntimeError> {
        let state = self.test_state.get_or_insert_with(TestState::new);
        state.ran += 1;
        let forced = state
            .force_todo
            .iter()
            .any(|(start, end)| state.ran >= *start && state.ran <= *end);
        let todo = todo || forced;
        if !success && !todo {
            state.failed += 1;
        }
        let mut line = String::new();
        if success {
            line.push_str("ok ");
        } else {
            line.push_str("not ok ");
        }
        line.push_str(&state.ran.to_string());
        if !desc.is_empty() {
            line.push_str(" - ");
            line.push_str(desc);
        }
        if todo {
            line.push_str(" # TODO");
        }
        line.push('\n');
        self.output.push_str(&line);
        Ok(())
    }

    pub(super) fn positional_values(args: &[Value]) -> Vec<&Value> {
        args.iter()
            .filter(|v| !matches!(v, Value::Pair(_, _)))
            .collect()
    }

    pub(super) fn positional_value(args: &[Value], index: usize) -> Option<&Value> {
        let mut count = 0;
        for arg in args {
            if !matches!(arg, Value::Pair(_, _)) {
                if count == index {
                    return Some(arg);
                }
                count += 1;
            }
        }
        None
    }

    pub(super) fn positional_value_required<'a>(
        args: &'a [Value],
        index: usize,
        message: &str,
    ) -> Result<&'a Value, RuntimeError> {
        Self::positional_value(args, index).ok_or_else(|| RuntimeError::new(message))
    }

    pub(super) fn positional_string(args: &[Value], index: usize) -> String {
        Self::positional_value(args, index)
            .map(|v| v.to_string_value())
            .unwrap_or_default()
    }

    pub(super) fn named_bool(args: &[Value], name: &str) -> bool {
        for arg in args {
            if let Value::Pair(key, value) = arg
                && key == name
            {
                return value.truthy();
            }
        }
        false
    }

    pub(super) fn named_value(args: &[Value], name: &str) -> Option<Value> {
        for arg in args {
            if let Value::Pair(key, value) = arg
                && key == name
            {
                return Some(value.as_ref().clone());
            }
        }
        None
    }
}
