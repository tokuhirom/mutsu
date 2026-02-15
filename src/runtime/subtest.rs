use super::*;

impl Interpreter {
    pub(crate) fn begin_subtest(&mut self) -> SubtestContext {
        let parent_test_state = self.test_state.take();
        let parent_output = std::mem::take(&mut self.output);
        let parent_halted = self.halted;
        self.test_state = Some(TestState::new());
        self.halted = false;
        SubtestContext {
            parent_test_state,
            parent_output,
            parent_halted,
        }
    }

    pub(crate) fn finish_subtest(
        &mut self,
        ctx: SubtestContext,
        label: &str,
        run_result: Result<(), RuntimeError>,
    ) -> Result<(), RuntimeError> {
        let subtest_output = std::mem::take(&mut self.output);
        let subtest_state = self.test_state.take();
        let subtest_failed = subtest_state.as_ref().map(|s| s.failed).unwrap_or(0);

        self.test_state = ctx.parent_test_state;
        self.output = ctx.parent_output;
        self.halted = ctx.parent_halted;

        for line in subtest_output.lines() {
            self.output.push_str("    ");
            self.output.push_str(line);
            self.output.push('\n');
        }

        let ok = run_result.is_ok() && subtest_failed == 0;
        self.test_ok(ok, label, false)?;
        Ok(())
    }

    pub(crate) fn run_whenever_with_value(
        &mut self,
        supply_val: Value,
        target_var: Option<&str>,
        param: &Option<String>,
        body: &[Stmt],
    ) -> Result<(), RuntimeError> {
        if let Value::Instance {
            class_name,
            attributes,
            ..
        } = supply_val
            && class_name == "Supply"
        {
            let tap_sub = Value::Sub {
                package: self.current_package.clone(),
                name: String::new(),
                params: param.iter().cloned().collect(),
                body: body.to_vec(),
                env: self.env.clone(),
                id: next_instance_id(),
            };
            let mut attrs = attributes.clone();
            if let Some(Value::Array(items)) = attrs.get_mut("taps") {
                items.push(tap_sub.clone());
            } else {
                attrs.insert("taps".to_string(), Value::Array(vec![tap_sub.clone()]));
            }
            if let Some(Value::Array(values)) = attrs.get("values") {
                for v in values {
                    let _ = self.call_sub_value(tap_sub.clone(), vec![v.clone()], true);
                }
            }
            let updated = Value::make_instance(class_name, attrs);
            if let Some(name) = target_var {
                self.env.insert(name.to_string(), updated);
            }
        }
        Ok(())
    }
}
