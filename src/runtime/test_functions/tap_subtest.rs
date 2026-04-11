use super::super::*;

impl Interpreter {
    pub(crate) fn test_fn_subtest(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        // subtest 'name' => { ... } (Pair arg) or subtest 'name', { ... } (two args)
        // Pairs are treated as named args by positional_value, so check raw args first
        let (label, block) = if let Some(Value::Pair(key, val)) = args.first() {
            (key.to_string(), *val.clone())
        } else if let Some(Value::ValuePair(key, val)) = args.first() {
            (key.to_string_value(), *val.clone())
        } else if let Some(first) = args.first() {
            if matches!(
                first,
                Value::Sub(_) | Value::WeakSub(_) | Value::Routine { .. }
            ) {
                let block = first.clone();
                let label = args.get(1).map(|v| v.to_string_value()).unwrap_or_default();
                (label, block)
            } else {
                let label = Self::positional_string(args, 0);
                let block = Self::positional_value(args, 1)
                    .cloned()
                    .unwrap_or(Value::Nil);
                (label, block)
            }
        } else {
            let label = Self::positional_string(args, 0);
            let block = Self::positional_value(args, 1)
                .cloned()
                .unwrap_or(Value::Nil);
            (label, block)
        };
        // Detect whether the callable is a bare block (not a sub/method).
        // `plan skip-all` inside a Block subtest is an error in Raku because
        // `return` cannot be used from a Block.
        let callable_is_block = match &block {
            Value::Sub(data) => {
                if let Some(cc) = &data.compiled_code {
                    // Compiled path: check is_routine flag set by the compiler
                    !cc.is_routine
                } else {
                    // Interpreter path: check is_bare_block on the SubData
                    data.is_bare_block
                }
            }
            _ => false,
        };
        let ctx = self.begin_subtest();
        self.subtest_callable_is_block = callable_is_block;
        let saved_env = self.env.clone();
        let saved_functions = self.functions.clone();
        let saved_proto_functions = self.proto_functions.clone();
        let saved_token_defs = self.token_defs.clone();
        let saved_proto_subs = self.proto_subs.clone();
        let saved_proto_tokens = self.proto_tokens.clone();
        let saved_classes = self.classes.clone();
        let saved_class_trusts = self.class_trusts.clone();
        let saved_roles = self.roles.clone();
        let saved_subsets = self.subsets.clone();
        let saved_type_metadata = self.type_metadata.clone();
        let saved_var_type_constraints = self.snapshot_var_type_constraints();
        let run_result = self.call_sub_value(block, vec![], true);
        // If the error is a "skip-all inside Block" error, propagate it
        // through the subtest rather than absorbing it as a test failure.
        if let Err(ref e) = run_result
            && e.message.contains("Must give `subtest` a (Sub)")
        {
            // Restore state and propagate the error
            self.env = saved_env;
            self.functions = saved_functions;
            self.proto_functions = saved_proto_functions;
            self.token_defs = saved_token_defs;
            self.proto_subs = saved_proto_subs;
            self.proto_tokens = saved_proto_tokens;
            self.classes = saved_classes;
            self.class_trusts = saved_class_trusts;
            self.roles = saved_roles;
            self.subsets = saved_subsets;
            self.type_metadata = saved_type_metadata;
            self.restore_var_type_constraints(saved_var_type_constraints);
            self.finish_subtest(ctx, &label, run_result.map(|_| ()))?;
            // finish_subtest absorbs the error, so re-raise it
            return Err(RuntimeError::new(
                "Must give `subtest` a (Sub) or a (Method) to be able to use \
                 `skip-all` plan inside, but you gave a (Block)",
            ));
        }
        let mut merged_env = saved_env.clone();
        for (k, v) in &self.env {
            if k == "_" || k == "$_" {
                // Do not propagate topic changes from subtest to caller
                continue;
            }
            if saved_env.contains_key(k) || k.starts_with("__mutsu_var_meta::") {
                merged_env.insert(k.clone(), v.clone());
            }
        }
        self.env = merged_env;
        self.functions = saved_functions;
        self.proto_functions = saved_proto_functions;
        self.token_defs = saved_token_defs;
        self.proto_subs = saved_proto_subs;
        self.proto_tokens = saved_proto_tokens;
        self.classes = saved_classes;
        self.class_trusts = saved_class_trusts;
        self.roles = saved_roles;
        self.subsets = saved_subsets;
        self.type_metadata = saved_type_metadata;
        self.restore_var_type_constraints(saved_var_type_constraints);
        self.finish_subtest(ctx, &label, run_result.map(|_| ()))?;
        Ok(Value::Bool(true))
    }

    pub(crate) fn test_fn_group_of(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        // group-of $plan => $desc => { ... }
        // Accept both `Pair` and `ValuePair` keys for compatibility with non-string keys.
        let to_pair_parts = |value: &Value| -> Option<(Value, Value)> {
            match value {
                Value::Pair(k, v) => Some((Value::str(k.clone()), *v.clone())),
                Value::ValuePair(k, v) => Some((*k.clone(), *v.clone())),
                _ => None,
            }
        };
        let Some((plan_key, inner)) = args.first().and_then(to_pair_parts) else {
            return Err(RuntimeError::new("group-of expects a Pair argument"));
        };
        let Some((desc_key, block)) = to_pair_parts(&inner) else {
            return Err(RuntimeError::new(
                "group-of expects $plan => $desc => { ... }",
            ));
        };
        let plan: i64 = match plan_key {
            Value::Int(i) => i,
            other => other
                .to_string_value()
                .parse()
                .map_err(|_| RuntimeError::new("group-of: plan must be an integer"))?,
        };
        let desc = desc_key.to_string_value();
        let ctx = self.begin_subtest();
        let saved_env = self.env.clone();
        let saved_functions = self.functions.clone();
        let saved_proto_functions = self.proto_functions.clone();
        let saved_token_defs = self.token_defs.clone();
        let saved_proto_subs = self.proto_subs.clone();
        let saved_proto_tokens = self.proto_tokens.clone();
        let saved_classes = self.classes.clone();
        let saved_class_trusts = self.class_trusts.clone();
        let saved_roles = self.roles.clone();
        let saved_subsets = self.subsets.clone();
        let saved_type_metadata = self.type_metadata.clone();
        let saved_var_type_constraints = self.snapshot_var_type_constraints();
        self.test_fn_plan(&[Value::Int(plan)])?;
        let run_result = self.call_sub_value(block, vec![], true);
        self.env = saved_env;
        self.functions = saved_functions;
        self.proto_functions = saved_proto_functions;
        self.token_defs = saved_token_defs;
        self.proto_subs = saved_proto_subs;
        self.proto_tokens = saved_proto_tokens;
        self.classes = saved_classes;
        self.class_trusts = saved_class_trusts;
        self.roles = saved_roles;
        self.subsets = saved_subsets;
        self.type_metadata = saved_type_metadata;
        self.restore_var_type_constraints(saved_var_type_constraints);
        self.finish_subtest(ctx, &desc, run_result.map(|_| ()))?;
        Ok(Value::Bool(true))
    }
}
