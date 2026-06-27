use super::*;
use crate::ast::{FunctionDef, ParamDef};
use crate::opcode::CompiledFunction;
use std::collections::HashMap;

struct ParsedMainArgs {
    positional: Vec<Value>,
    named: Vec<(String, Value)>,
}

struct NamedParamInfo {
    names: Vec<String>,
    is_bool: bool,
    requires_value: bool,
    is_array: bool,
}

#[derive(Default)]
struct SubMainOpts {
    named_anywhere: bool,
    bundling: bool,
    allow_no: bool,
    coerce_allomorphs_to: Option<String>,
    numeric_suffix_as_value: bool,
}

impl Interpreter {
    fn read_sub_main_opts(&self) -> SubMainOpts {
        let mut opts = SubMainOpts::default();
        let hash = self
            .env
            .get("%*SUB-MAIN-OPTS")
            .cloned()
            .unwrap_or(Value::Nil);
        let items = match hash {
            Value::Hash(h) => h,
            _ => return opts,
        };
        for (key, value) in items.iter() {
            match key.as_str() {
                "named-anywhere" => opts.named_anywhere = value.truthy(),
                "bundling" => opts.bundling = value.truthy(),
                "allow-no" => opts.allow_no = value.truthy(),
                "coerce-allomorphs-to" => {
                    let name = match value {
                        Value::Package(n) => n.resolve().to_string(),
                        _ => value.to_string_value(),
                    };
                    opts.coerce_allomorphs_to = Some(name);
                }
                "numeric-suffix-as-value" => {
                    opts.numeric_suffix_as_value = value.truthy();
                }
                _ => {}
            }
        }
        opts
    }

    pub(super) fn dispatch_main(
        &mut self,
        _compiled_fns: &HashMap<String, CompiledFunction>,
    ) -> Result<(), RuntimeError> {
        let all_candidates = self.collect_main_candidates();
        if all_candidates.is_empty() {
            return Ok(());
        }
        let main_def = all_candidates[0].clone();
        let sub_main_opts = self.read_sub_main_opts();

        let args_val = self
            .env
            .get("@*ARGS")
            .cloned()
            .unwrap_or_else(|| Value::array(Vec::new()));
        let raw_args: Vec<String> = if let Value::Array(items, ..) = args_val {
            items.iter().map(|v| v.to_string_value()).collect()
        } else {
            Vec::new()
        };

        if raw_args.iter().any(|a| a == "--help") {
            let usage = self.generate_usage_message(&main_def);
            self.emit_output(&format!("Usage:\n{}\n", usage));
            return Ok(());
        }

        // Emit warning for 'is rw' parameters on MAIN
        'rw_check: for candidate in &all_candidates {
            for pd in &candidate.param_defs {
                if pd.traits.iter().any(|t| t == "rw") {
                    self.emit_stderr(
                        "Potential difficulties:\n    \
                         'is rw' on parameters of 'sub MAIN' usually cannot \
                         be satisfied.\n    \
                         Did you mean 'is copy'?\n",
                    );
                    break 'rw_check;
                }
            }
        }

        for candidate in &all_candidates {
            let named_info = Self::extract_named_param_info(candidate);
            let has_capture = candidate.param_defs.iter().any(|p| {
                p.slurpy
                    && !p.named
                    && !p.double_slurpy
                    && !p.name.starts_with(['$', '@', '%', '*'])
            });
            let has_slurpy_named = has_capture
                || candidate
                    .param_defs
                    .iter()
                    .any(|p| p.double_slurpy || (p.named && p.slurpy));
            let has_slurpy_positional = candidate
                .param_defs
                .iter()
                .any(|p| p.slurpy && !p.named && !p.double_slurpy);

            match Self::parse_cli_args(&raw_args, &named_info, &sub_main_opts) {
                Ok(parsed) => {
                    let positional_params: Vec<&ParamDef> = candidate
                        .param_defs
                        .iter()
                        .filter(|p| !p.named && !p.slurpy && !p.double_slurpy)
                        .collect();
                    let required_positional = positional_params
                        .iter()
                        .filter(|p| p.default.is_none() && !p.optional_marker)
                        .count();
                    let max_positional = if has_slurpy_positional {
                        usize::MAX
                    } else {
                        positional_params.len()
                    };
                    if parsed.positional.len() < required_positional
                        || parsed.positional.len() > max_positional
                    {
                        continue;
                    }
                    let mut named_ok = true;
                    for pd in &candidate.param_defs {
                        if pd.named && pd.required && pd.default.is_none() && !pd.optional_marker {
                            let param_names = Self::param_all_names(pd);
                            if !parsed.named.iter().any(|(n, _)| param_names.contains(n)) {
                                named_ok = false;
                                break;
                            }
                        }
                    }
                    if !named_ok {
                        continue;
                    }
                    if !has_slurpy_named {
                        let all_accepted: Vec<String> =
                            named_info.iter().flat_map(|ni| ni.names.clone()).collect();
                        if parsed.named.iter().any(|(n, _)| !all_accepted.contains(n)) {
                            continue;
                        }
                    }
                    match self.call_main_with_parsed_args(candidate, &parsed, &sub_main_opts) {
                        Ok(()) => return Ok(()),
                        Err(e) => {
                            let msg = e.message.to_lowercase();
                            if msg.contains("constraint")
                                || msg.contains("type check")
                                || msg.contains("where")
                            {
                                continue;
                            }
                            return Err(e);
                        }
                    }
                }
                Err(_) => continue,
            }
        }

        self.handle_main_dispatch_failure(&main_def)?;
        Ok(())
    }

    fn collect_main_candidates(&self) -> Vec<FunctionDef> {
        let mut candidates = Vec::new();
        let mut seen_keys = std::collections::HashSet::new();
        let prefixes: Vec<String> = {
            let mut p = vec!["GLOBAL::MAIN/".to_string()];
            let pkg = &self.current_package();
            if pkg != "GLOBAL" {
                p.push(format!("{pkg}::MAIN/"));
            }
            p
        };
        for (key, def) in &self.registry().functions {
            let ks = key.resolve();
            if prefixes.iter().any(|prefix| ks.starts_with(prefix)) {
                let fp =
                    crate::ast::function_body_fingerprint(&def.params, &def.param_defs, &def.body);
                if seen_keys.insert(fp) {
                    candidates.push((**def).clone());
                }
            }
        }
        if candidates.is_empty()
            && let Some(def) = self.resolve_function("MAIN")
        {
            candidates.push((*def).clone());
        }
        candidates
            .sort_by(|a, b| Self::candidate_specificity(b).cmp(&Self::candidate_specificity(a)));
        candidates
    }

    fn candidate_specificity(def: &FunctionDef) -> i32 {
        let mut score = 0i32;
        for pd in &def.param_defs {
            if let Some(tc) = &pd.type_constraint {
                match tc.as_str() {
                    "Any" | "Mu" => score += 1,
                    "Bool" => score += 2,
                    "Str" | "Int" | "Num" | "Rat" => score += 3,
                    _ => score += 4,
                }
            }
            if pd.named && pd.name.starts_with('@') {
                score -= 1;
            }
        }
        score
    }

    fn extract_named_param_info(def: &FunctionDef) -> Vec<NamedParamInfo> {
        def.param_defs
            .iter()
            .filter(|pd| pd.named)
            .map(|pd| NamedParamInfo {
                names: Self::param_all_names(pd),
                is_bool: pd.type_constraint.as_ref().is_some_and(|t| t == "Bool"),
                requires_value: pd
                    .type_constraint
                    .as_ref()
                    .is_some_and(|t| t != "Bool" && t != "Any"),
                is_array: pd.name.starts_with('@'),
            })
            .collect()
    }

    fn param_all_names(pd: &ParamDef) -> Vec<String> {
        let mut names = vec![pd.name.trim_start_matches(['$', '@', '%']).to_string()];
        if let Some(sub_params) = &pd.sub_signature {
            for sp in sub_params {
                let alias = sp.name.trim_start_matches(['$', '@', '%']);
                if !alias.is_empty() && !names.contains(&alias.to_string()) {
                    names.push(alias.to_string());
                }
            }
        }
        names
    }

    fn parse_cli_args(
        raw_args: &[String],
        named_info: &[NamedParamInfo],
        opts: &SubMainOpts,
    ) -> Result<ParsedMainArgs, String> {
        let mut positional = Vec::new();
        let mut named: Vec<(String, Value)> = Vec::new();
        let mut i = 0;
        let mut positional_started = false;

        while i < raw_args.len() {
            let arg = &raw_args[i];

            if positional_started && !opts.named_anywhere {
                positional.push(Value::str(arg.clone()));
                i += 1;
                continue;
            }
            if arg == "--" {
                positional_started = true;
                i += 1;
                continue;
            }
            if let Some(rest) = arg.strip_prefix("--/") {
                named.push((rest.to_string(), Value::Bool(false)));
                i += 1;
                continue;
            }
            if opts.allow_no
                && let Some(rest) = arg.strip_prefix("--no-")
                && !rest.is_empty()
            {
                named.push((rest.to_string(), Value::Bool(false)));
                i += 1;
                continue;
            }
            if let Some(rest) = arg.strip_prefix("--") {
                if rest.is_empty() {
                    i += 1;
                    continue;
                }
                if let Some(eq_pos) = rest.find('=') {
                    let key = &rest[..eq_pos];
                    let val = &rest[eq_pos + 1..];
                    let info = named_info
                        .iter()
                        .find(|ni| ni.names.iter().any(|n| n == key));
                    if let Some(ni) = info
                        && ni.is_array
                    {
                        if let Some(existing) = named.iter_mut().find(|(n, _)| n == key) {
                            let old = std::mem::replace(&mut existing.1, Value::Nil);
                            if let Value::Array(items, ..) = old {
                                let mut v: Vec<Value> = items.to_vec();
                                v.push(Value::str(val.to_string()));
                                existing.1 = Value::array(v);
                            }
                        } else {
                            named.push((
                                key.to_string(),
                                Value::array(vec![Value::str(val.to_string())]),
                            ));
                        }
                        i += 1;
                        continue;
                    }
                    named.push((key.to_string(), Value::str(val.to_string())));
                    i += 1;
                    continue;
                }
                let key = rest;
                let info = named_info
                    .iter()
                    .find(|ni| ni.names.iter().any(|n| n == key));
                if let Some(ni) = info {
                    if ni.is_bool {
                        named.push((key.to_string(), Value::Bool(true)));
                        i += 1;
                        continue;
                    }
                    if ni.requires_value && i + 1 < raw_args.len() {
                        named.push((key.to_string(), Value::str(raw_args[i + 1].clone())));
                        i += 2;
                        continue;
                    }
                }
                named.push((key.to_string(), Value::Bool(true)));
                i += 1;
                continue;
            }
            if arg.starts_with('-') && arg.len() > 1 && !arg.starts_with("--") {
                let rest = &arg[1..];
                if let Some(eq_pos) = rest.find('=') {
                    let key = &rest[..eq_pos];
                    let val = &rest[eq_pos + 1..];
                    named.push((key.to_string(), Value::str(val.to_string())));
                    i += 1;
                    continue;
                }
                if opts.numeric_suffix_as_value && rest.len() >= 2 {
                    let first_char = rest.chars().next().unwrap();
                    let suffix = &rest[first_char.len_utf8()..];
                    if first_char.is_ascii_alphabetic()
                        && suffix.chars().all(|c| c.is_ascii_digit())
                    {
                        let int_val: i64 = suffix.parse().unwrap_or(0);
                        let mut mixins = std::collections::HashMap::new();
                        mixins.insert("Str".to_string(), Value::str(suffix.to_string()));
                        named.push((
                            first_char.to_string(),
                            Value::mixin(Value::Int(int_val), mixins),
                        ));
                        i += 1;
                        continue;
                    }
                }
                if opts.bundling && rest.len() >= 2 && rest.chars().all(|c| c.is_ascii_alphabetic())
                {
                    for ch in rest.chars() {
                        named.push((ch.to_string(), Value::Bool(true)));
                    }
                    i += 1;
                    continue;
                }
                let key = rest;
                let info = named_info
                    .iter()
                    .find(|ni| ni.names.iter().any(|n| n == key));
                if let Some(ni) = info {
                    if ni.is_bool {
                        named.push((key.to_string(), Value::Bool(true)));
                        i += 1;
                        continue;
                    }
                    if ni.requires_value && i + 1 < raw_args.len() {
                        named.push((key.to_string(), Value::str(raw_args[i + 1].clone())));
                        i += 2;
                        continue;
                    }
                }
                named.push((key.to_string(), Value::Bool(true)));
                i += 1;
                continue;
            }
            positional.push(Value::str(arg.clone()));
            positional_started = true;
            i += 1;
        }
        Ok(ParsedMainArgs { positional, named })
    }

    fn call_main_with_parsed_args(
        &mut self,
        candidate: &FunctionDef,
        parsed: &ParsedMainArgs,
        opts: &SubMainOpts,
    ) -> Result<(), RuntimeError> {
        let positional_params: Vec<&ParamDef> = candidate
            .param_defs
            .iter()
            .filter(|p| !p.named && !p.slurpy && !p.double_slurpy)
            .collect();
        let mut args = Vec::new();
        for (i, arg) in parsed.positional.iter().enumerate() {
            let mut coerced = if let Some(pd) = positional_params.get(i) {
                self.coerce_cli_arg(arg, pd)
            } else {
                // Beyond the declared positionals (e.g. into a slurpy `*@a`):
                // untyped, so apply enum auto-conversion like an untyped param.
                self.autoconvert_cli_enum_value(arg)
            };
            if let Some(ref target_type) = opts.coerce_allomorphs_to {
                coerced = Self::coerce_to_type(&coerced, target_type);
            }
            args.push(coerced);
        }
        // Build a lookup from accepted named-arg names to their parameter so that
        // CLI string values can be coerced (e.g. to an enum) before binding.
        let named_params: Vec<&ParamDef> =
            candidate.param_defs.iter().filter(|p| p.named).collect();
        for (name, value) in &parsed.named {
            let coerced = named_params
                .iter()
                .find(|pd| Self::param_all_names(pd).iter().any(|n| n == name))
                .map(|pd| self.coerce_cli_arg(value, pd))
                .unwrap_or_else(|| value.clone());
            args.push(Value::Pair(name.clone(), Box::new(coerced)));
        }
        let all_candidates = self.collect_main_candidates();
        let usage_text = self.generate_usage_from_candidates(&all_candidates);
        self.env
            .insert("$*USAGE".to_string(), Value::str(usage_text.clone()));
        self.env
            .insert("*USAGE".to_string(), Value::str(usage_text));
        self.mark_readonly("$*USAGE");
        self.mark_readonly("*USAGE");
        match self.call_function_def(candidate, &args) {
            Ok(_) => Ok(()),
            Err(e) if e.return_value.is_some() => Ok(()),
            Err(e) if e.message.is_empty() => Ok(()),
            Err(e) => Err(e),
        }
    }

    fn handle_main_dispatch_failure(&mut self, main_def: &FunctionDef) -> Result<(), RuntimeError> {
        let usage = self.generate_usage_message(main_def);
        self.env
            .insert("$*USAGE".to_string(), Value::str(usage.clone()));
        self.env
            .insert("*USAGE".to_string(), Value::str(usage.clone()));
        self.mark_readonly("$*USAGE");
        self.mark_readonly("*USAGE");
        if self.resolve_function("GENERATE-USAGE").is_some() {
            let result = self.call_function("GENERATE-USAGE", vec![])?;
            self.emit_stderr(&format!("{}\n", result.to_string_value()));
        } else {
            self.emit_stderr(&format!("Usage:\n{}\n", usage));
        }
        self.exit_code = 2;
        Ok(())
    }

    fn generate_usage_message(&self, main_def: &FunctionDef) -> String {
        let all_candidates = self.collect_main_candidates();
        if all_candidates.len() > 1 {
            self.generate_usage_from_candidates(&all_candidates)
        } else {
            self.generate_usage_from_candidates(std::slice::from_ref(main_def))
        }
    }

    fn generate_usage_from_candidates(&self, candidates: &[FunctionDef]) -> String {
        let program = self
            .env
            .get("*PROGRAM-NAME")
            .or_else(|| self.env.get("$*PROGRAM-NAME"))
            .map(|v| v.to_string_value())
            .unwrap_or_else(|| "program".to_string());
        let mut lines = Vec::new();
        for candidate in candidates {
            let mut parts = vec![format!("  {}", program)];
            for pd in &candidate.param_defs {
                if pd.named {
                    let name = pd.name.trim_start_matches(['$', '@', '%']);
                    let is_bool = pd.type_constraint.as_ref().is_some_and(|t| t == "Bool");
                    if is_bool {
                        parts.push(format!("[--{}]", name));
                    } else {
                        let value_placeholder = self.usage_value_placeholder(pd);
                        if pd.required {
                            parts.push(format!("--{}{}", name, value_placeholder));
                        } else {
                            parts.push(format!("[--{}{}]", name, value_placeholder));
                        }
                    }
                } else if pd.slurpy {
                    let name = pd.name.trim_start_matches(['$', '@', '%', '*']);
                    parts.push(format!("[<{}>...]", name));
                } else if pd.double_slurpy {
                    // skip
                } else if let Some(ref lit) = pd.literal_value {
                    parts.push(lit.to_string_value());
                } else {
                    let name = pd.name.trim_start_matches(['$', '@', '%', '\\']);
                    if pd.default.is_some() || pd.optional_marker {
                        parts.push(format!("[<{}>]", name));
                    } else {
                        parts.push(format!("<{}>", name));
                    }
                }
            }
            lines.push(parts.join(" "));
        }
        lines.join("\n")
    }

    /// Build the `=<value>` placeholder for a named parameter in the usage
    /// message. Enum-typed params show `=<EnumName> (variants...)`, subset-typed
    /// params show `[=SubsetName]`, everything else shows `=<value>`.
    fn usage_value_placeholder(&self, pd: &ParamDef) -> String {
        if let Some(tc) = &pd.type_constraint {
            if let Some(variants) = self.registry().enum_types.get(tc.as_str()) {
                let mut names: Vec<&str> = variants.iter().map(|(k, _)| k.as_str()).collect();
                names.sort_unstable();
                return format!("=<{}> ({})", tc, names.join(" "));
            }
            if self.registry().subsets.contains_key(tc.as_str()) {
                return format!("[={}]", tc);
            }
        }
        "=<value>".to_string()
    }

    /// Auto-convert an untyped CLI argument string to an enum value when the
    /// string is an unambiguous value name of a known enum. Used for MAIN
    /// parameters without a type constraint (e.g. slurpy `*@a`).
    fn autoconvert_cli_enum_value(&mut self, arg: &Value) -> Value {
        let name = match arg {
            Value::Str(s) => s.to_string(),
            _ => return arg.clone(),
        };
        // Bool is a core enum but is represented natively rather than in
        // `enum_types`; convert its value names directly.
        match name.as_str() {
            "True" => return Value::Bool(true),
            "False" => return Value::Bool(false),
            _ => {}
        }
        let matching: Vec<String> = self
            .registry()
            .enum_types
            .iter()
            .filter(|(_, variants)| variants.iter().any(|(k, _)| k == &name))
            .map(|(enum_name, _)| enum_name.clone())
            .collect();
        if matching.len() == 1 {
            // Hoist the registry read so the guard drops before coerce_to_enum_variant
            // (which needs &mut self); never hold a guard across user-code re-entry.
            let variants = self.registry().enum_types.get(&matching[0]).cloned();
            if let Some(variants) = variants
                && let Some(enum_val) =
                    self.coerce_to_enum_variant(&matching[0], &variants, Value::str(name))
            {
                return enum_val;
            }
        }
        arg.clone()
    }

    fn coerce_cli_arg(&mut self, arg: &Value, pd: &ParamDef) -> Value {
        let tc = match &pd.type_constraint {
            Some(t) => t.as_str(),
            // Untyped parameters: Raku auto-converts a CLI argument that is an
            // unambiguous value name of a known enum (e.g. `True` -> Bool::True,
            // `Less` -> Order::Less). Leave the string unchanged otherwise.
            None => return self.autoconvert_cli_enum_value(arg),
        };
        let s = arg.to_string_value();
        match tc {
            "Int" => s
                .parse::<i64>()
                .map(Value::Int)
                .unwrap_or_else(|_| arg.clone()),
            "Num" => s
                .parse::<f64>()
                .map(Value::Num)
                .unwrap_or_else(|_| arg.clone()),
            "Rat" => s
                .parse::<f64>()
                .map(Value::Num)
                .unwrap_or_else(|_| arg.clone()),
            _ => {
                // If the parameter is typed with an enum, coerce the CLI string to
                // the matching enum variant so type-checked binding succeeds. An
                // unmatched string is left unchanged so dispatch can reject it.
                let variants = self.registry().enum_types.get(tc).cloned();
                if let Some(variants) = variants
                    && let Some(enum_val) =
                        self.coerce_to_enum_variant(tc, &variants, Value::str(s.clone()))
                {
                    return enum_val;
                }
                // Subset-typed parameters keep the raw string; the subset's `where`
                // clause is validated during signature binding.
                arg.clone()
            }
        }
    }

    fn coerce_to_type(val: &Value, target: &str) -> Value {
        let s = val.to_string_value();
        match target {
            "Str" => Value::str(s),
            "Int" => s
                .parse::<i64>()
                .map(Value::Int)
                .unwrap_or_else(|_| val.clone()),
            "Num" => s
                .parse::<f64>()
                .map(Value::Num)
                .unwrap_or_else(|_| val.clone()),
            _ => val.clone(),
        }
    }
}
