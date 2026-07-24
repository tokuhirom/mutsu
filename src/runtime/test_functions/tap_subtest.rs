use std::sync::Arc;

use rustc_hash::{FxHashMap, FxHashSet};

use super::super::*;
use crate::ast::FunctionDef;

/// Declaration state a subtest body must not leak into its caller.
///
/// A subtest body is a block, so anything it declares (`class`, `role`,
/// `subset`, `sub`, `token`, ...) is lexical to it in Raku and must be rolled
/// back when the block ends. `use` is lexical too, which is why the set of
/// loaded modules belongs here: a module first loaded inside a subtest has all
/// of its declarations rolled back with everything else, so it must also stop
/// counting as loaded — otherwise a later `use` of it short-circuits as a
/// no-op and the types stay gone for the rest of the file.
pub(crate) struct SubtestDeclSnapshot {
    functions: FxHashMap<Symbol, Arc<FunctionDef>>,
    proto_functions: FxHashMap<Symbol, Arc<FunctionDef>>,
    token_defs: FxHashMap<Symbol, Vec<Arc<FunctionDef>>>,
    proto_subs: FxHashSet<String>,
    proto_tokens: FxHashSet<String>,
    classes: FxHashMap<String, ClassDef>,
    class_trusts: FxHashMap<String, FxHashSet<String>>,
    roles: FxHashMap<String, RoleDef>,
    subsets: FxHashMap<String, SubsetDef>,
    loaded_modules: HashSet<String>,
    type_metadata: HashMap<String, HashMap<String, Value>>,
    var_type_constraints: HashMap<String, String>,
}

impl Interpreter {
    pub(crate) fn snapshot_subtest_decls(&self) -> SubtestDeclSnapshot {
        let registry = self.registry();
        SubtestDeclSnapshot {
            functions: registry.functions.clone(),
            proto_functions: registry.proto_functions.clone(),
            token_defs: registry.token_defs.clone(),
            proto_subs: registry.proto_subs.clone(),
            proto_tokens: registry.proto_tokens.clone(),
            classes: registry.classes.clone(),
            class_trusts: registry.class_trusts.clone(),
            roles: registry.roles.clone(),
            subsets: registry.subsets.clone(),
            loaded_modules: self.loaded_modules.clone(),
            type_metadata: self.type_metadata.clone(),
            var_type_constraints: self.snapshot_var_type_constraints(),
        }
    }

    pub(crate) fn restore_subtest_decls(&mut self, snapshot: SubtestDeclSnapshot) {
        let SubtestDeclSnapshot {
            functions,
            proto_functions,
            token_defs,
            proto_subs,
            proto_tokens,
            classes,
            class_trusts,
            roles,
            subsets,
            loaded_modules,
            mut type_metadata,
            var_type_constraints,
        } = snapshot;
        {
            let mut registry = self.registry_mut();
            registry.functions = functions;
            registry.proto_functions = proto_functions;
            registry.token_defs = token_defs;
            registry.proto_subs = proto_subs;
            registry.proto_tokens = proto_tokens;
            registry.classes = classes;
            registry.class_trusts = class_trusts;
            registry.roles = roles;
            registry.subsets = subsets;
        }
        crate::runtime::regex_parse::TOKEN_DEFS_GEN
            .fetch_add(1, std::sync::atomic::Ordering::Relaxed);
        self.loaded_modules = loaded_modules;
        // Merge type_metadata: preserve entries added during the subtest
        for (key, val) in std::mem::take(&mut self.type_metadata) {
            type_metadata.entry(key).or_insert(val);
        }
        self.type_metadata = type_metadata;
        self.restore_var_type_constraints(var_type_constraints);
    }

    pub(crate) fn test_fn_subtest(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        // subtest 'name' => { ... } (Pair arg) or subtest 'name', { ... } (two args)
        // Pairs are treated as named args by positional_value, so check raw args first
        let (label, block) = if let Some(ValueView::Pair(key, val)) = args.first().map(Value::view)
        {
            (key.to_string(), val.clone())
        } else if let Some(ValueView::ValuePair(key, val)) = args.first().map(Value::view) {
            (key.to_string_value(), val.clone())
        } else if let Some(first) = args.first() {
            if matches!(
                first.view(),
                ValueView::Sub(_) | ValueView::WeakSub(_) | ValueView::Routine { .. }
            ) {
                let block = first.clone();
                let label = args.get(1).map(|v| v.to_string_value()).unwrap_or_default();
                (label, block)
            } else {
                let label = Self::positional_string(args, 0);
                let block = Self::positional_value(args, 1)
                    .cloned()
                    .unwrap_or(Value::NIL);
                (label, block)
            }
        } else {
            let label = Self::positional_string(args, 0);
            let block = Self::positional_value(args, 1)
                .cloned()
                .unwrap_or(Value::NIL);
            (label, block)
        };
        // Detect whether the callable is a Sub/Method (supports `return`) or a Block.
        // `plan skip-all` inside a subtest uses `return`, which only works for Subs.
        let callable_is_sub = match block.view() {
            ValueView::Sub(data) => !data.is_bare_block,
            ValueView::Routine { .. } => true,
            _ => false,
        };
        let ctx = self.begin_subtest();
        // Override the default (true) set by begin_subtest
        self.tap.set_subtest_callable_is_sub_last(callable_is_sub);
        let saved_env = self.env.clone();
        let saved_decls = self.snapshot_subtest_decls();
        let run_result = self.call_sub_value(block, vec![], true);
        // If `plan skip-all` was used inside a Block callable, the error is fatal
        // and should propagate out of the subtest entirely (matching Raku behavior).
        if let Err(ref e) = run_result
            && e.message.starts_with("Must give `subtest`")
        {
            // Restore state before propagating
            self.tap.set_state(ctx.parent_test_state);
            self.output_sink_mut().output = ctx.parent_output;
            self.halted = ctx.parent_halted;
            self.tap.end_subtest();
            self.env = saved_env;
            self.restore_subtest_decls(saved_decls);
            return Err(run_result.unwrap_err());
        }
        let mut merged_env = saved_env.clone();
        for (k, v) in &self.env {
            if k == "_" || k == "$_" {
                // Do not propagate topic changes from subtest to caller
                continue;
            }
            if saved_env.contains_key_sym(*k) || k.starts_with("__mutsu_var_meta::") {
                merged_env.insert_sym(*k, v.clone());
            }
        }
        self.env = merged_env;
        self.restore_subtest_decls(saved_decls);
        self.finish_subtest(ctx, &label, run_result.map(|_| ()))?;
        Ok(Value::TRUE)
    }

    pub(crate) fn test_fn_group_of(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        // group-of $plan => $desc => { ... }
        // Accept both `Pair` and `ValuePair` keys for compatibility with non-string keys.
        let to_pair_parts = |value: &Value| -> Option<(Value, Value)> {
            match value.view() {
                ValueView::Pair(k, v) => Some((Value::str(k.clone()), v.clone())),
                ValueView::ValuePair(k, v) => Some((k.clone(), v.clone())),
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
        let plan: i64 = match plan_key.as_int() {
            Some(i) => i,
            None => plan_key
                .to_string_value()
                .parse()
                .map_err(|_| RuntimeError::new("group-of: plan must be an integer"))?,
        };
        let desc = desc_key.to_string_value();
        let ctx = self.begin_subtest();
        let saved_env = self.env.clone();
        let saved_decls = self.snapshot_subtest_decls();
        self.test_fn_plan(&[Value::int(plan)])?;
        let run_result = self.call_sub_value(block, vec![], true);
        self.env = saved_env;
        self.restore_subtest_decls(saved_decls);
        self.finish_subtest(ctx, &desc, run_result.map(|_| ()))?;
        Ok(Value::TRUE)
    }
}
