use super::*;
use crate::ast::{FunctionDef, ParamDef};
use crate::opcode::CompiledFunction;
use std::collections::HashMap;

/// Parsed CLI arguments for MAIN dispatch.
struct ParsedMainArgs {
    /// Positional arguments (strings from CLI).
    positional: Vec<Value>,
    /// Named arguments as (name, value) pairs.
    named: Vec<(String, Value)>,
}

/// Information about a named parameter from MAIN's signature.
struct NamedParamInfo {
    /// All accepted names for this parameter (including aliases).
    names: Vec<String>,
    /// Whether this parameter is typed as Bool.
    is_bool: bool,
    /// Whether this parameter requires a value (typed as Str, Int, etc. but not Any/Bool).
    requires_value: bool,
    /// Whether this is an array parameter (@foo).
    is_array: bool,
}

impl Interpreter {
    /// Entry point for MAIN dispatch after program body has executed.
    pub(super) fn dispatch_main(
        &mut self,
        _compiled_fns: &HashMap<String, CompiledFunction>,
    ) -> Result<(), RuntimeError> {
        // Check if MAIN is defined (either as plain sub or multi candidates)
        let all_candidates = self.collect_main_candidates();
        if all_candidates.is_empty() {
            return Ok(());
        }
        // Use first candidate as the default for usage generation
        let main_def = all_candidates[0].clone();

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

        // Check for --help flag
        if raw_args.iter().any(|a| a == "--help") {
            let usage = self.generate_usage_message(&main_def);
            // --help prints to stdout
            let text = format!("{}\n", usage);
            self.emit_output(&text);
            return Ok(());
        }

        // Try to parse args and dispatch against each candidate
        for candidate in &all_candidates {
            let named_info = Self::extract_named_param_info(candidate);
            let has_slurpy_named = candidate
                .param_defs
                .iter()
                .any(|p| p.double_slurpy || (p.named && p.slurpy));
            let has_slurpy_positional = candidate
                .param_defs
                .iter()
                .any(|p| p.slurpy && !p.named && !p.double_slurpy);

            match Self::parse_cli_args(&raw_args, &named_info) {
                Ok(parsed) => {
                    // Check if positional count matches
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

                    // Check that all required named params are provided
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

                    // Check for unexpected named args (unless slurpy named)
                    if !has_slurpy_named {
                        let all_accepted: Vec<String> =
                            named_info.iter().flat_map(|ni| ni.names.clone()).collect();
                        let unexpected =
                            parsed.named.iter().any(|(n, _)| !all_accepted.contains(n));
                        if unexpected {
                            continue;
                        }
                    }

                    // Build args for the call
                    match self.call_main_with_parsed_args(candidate, &parsed) {
                        Ok(()) => return Ok(()),
                        Err(e) => {
                            // If MAIN call failed due to a constraint check or
                            // similar runtime error, fall through to GENERATE-USAGE
                            // rather than leaking the internal exception.
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

        // MAIN dispatch failed - call GENERATE-USAGE or auto-generate
        self.handle_main_dispatch_failure(&main_def)?;
        Ok(())
    }

    /// Collect all MAIN candidates (including multi variants).
    fn collect_main_candidates(&self) -> Vec<FunctionDef> {
        let mut candidates = Vec::new();
        let mut seen_keys = std::collections::HashSet::new();

        // Look for multi candidates in both GLOBAL and current package
        let prefixes: Vec<String> = {
            let mut p = vec!["GLOBAL::MAIN/".to_string()];
            let pkg = &self.current_package;
            if pkg != "GLOBAL" {
                p.push(format!("{pkg}::MAIN/"));
            }
            p
        };

        for (key, def) in &self.functions {
            let ks = key.resolve();
            let matches = prefixes.iter().any(|prefix| ks.starts_with(prefix));
            if matches {
                // Deduplicate by body fingerprint
                let fp =
                    crate::ast::function_body_fingerprint(&def.params, &def.param_defs, &def.body);
                if seen_keys.insert(fp) {
                    candidates.push(def.clone());
                }
            }
        }

        // If no multi candidates found, use the plain MAIN
        if candidates.is_empty()
            && let Some(def) = self.resolve_function("MAIN")
        {
            candidates.push(def);
        }

        // Sort candidates: more specific types first
        candidates.sort_by(|a, b| {
            let a_specificity = Self::candidate_specificity(a);
            let b_specificity = Self::candidate_specificity(b);
            b_specificity.cmp(&a_specificity)
        });

        candidates
    }

    /// Compute a specificity score for a MAIN candidate (higher = more specific).
    /// Also penalizes array named params so Scalar candidates are preferred.
    fn candidate_specificity(def: &FunctionDef) -> i32 {
        let mut score = 0i32;
        for pd in &def.param_defs {
            if let Some(tc) = &pd.type_constraint {
                match tc.as_str() {
                    "Any" | "Mu" => score += 1,
                    "Bool" => score += 2,
                    "Str" | "Int" | "Num" | "Rat" => score += 3,
                    _ => score += 4, // Custom types like subsets are most specific
                }
            }
            // Penalize array named params so Scalar candidates are preferred
            if pd.named && pd.name.starts_with('@') {
                score -= 1;
            }
        }
        score
    }

    /// Extract named parameter info from a MAIN function definition.
    fn extract_named_param_info(def: &FunctionDef) -> Vec<NamedParamInfo> {
        let mut result = Vec::new();
        for pd in &def.param_defs {
            if pd.named {
                let names = Self::param_all_names(pd);
                let is_bool = pd.type_constraint.as_ref().is_some_and(|t| t == "Bool");
                let requires_value = pd
                    .type_constraint
                    .as_ref()
                    .is_some_and(|t| t != "Bool" && t != "Any");
                let is_array = pd.name.starts_with('@');
                result.push(NamedParamInfo {
                    names,
                    is_bool,
                    requires_value,
                    is_array,
                });
            }
        }
        result
    }

    /// Get all names for a named parameter (including aliases).
    fn param_all_names(pd: &ParamDef) -> Vec<String> {
        let mut names = Vec::new();
        let primary = pd.name.trim_start_matches(['$', '@', '%']);
        names.push(primary.to_string());
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

    /// Parse CLI arguments according to MAIN's named parameter info.
    /// Raku's MAIN CLI parsing requires named options before positional args.
    /// Once a positional arg is seen, everything after it is positional.
    fn parse_cli_args(
        raw_args: &[String],
        named_info: &[NamedParamInfo],
    ) -> Result<ParsedMainArgs, String> {
        let mut positional = Vec::new();
        let mut named: Vec<(String, Value)> = Vec::new();
        let mut i = 0;
        let mut positional_started = false;

        while i < raw_args.len() {
            let arg = &raw_args[i];

            if positional_started {
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

                // --flag (no =)
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
                    if ni.requires_value {
                        if i + 1 < raw_args.len() {
                            named.push((key.to_string(), Value::str(raw_args[i + 1].clone())));
                            i += 2;
                            continue;
                        } else {
                            return Err(format!("Option --{} requires a value", key));
                        }
                    }
                    // Any-typed or untyped: treat as Bool (no spacey value)
                    named.push((key.to_string(), Value::Bool(true)));
                    i += 1;
                    continue;
                }

                // Unknown named option - treat as Bool flag
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
                    if ni.requires_value {
                        if i + 1 < raw_args.len() {
                            named.push((key.to_string(), Value::str(raw_args[i + 1].clone())));
                            i += 2;
                            continue;
                        } else {
                            return Err(format!("Option -{} requires a value", key));
                        }
                    }
                    named.push((key.to_string(), Value::Bool(true)));
                    i += 1;
                    continue;
                }

                named.push((key.to_string(), Value::Bool(true)));
                i += 1;
                continue;
            }

            // Positional argument - everything after is positional
            positional.push(Value::str(arg.clone()));
            positional_started = true;
            i += 1;
        }

        Ok(ParsedMainArgs { positional, named })
    }

    /// Call MAIN with the parsed arguments.
    fn call_main_with_parsed_args(
        &mut self,
        candidate: &FunctionDef,
        parsed: &ParsedMainArgs,
    ) -> Result<(), RuntimeError> {
        let mut args = Vec::new();
        for arg in &parsed.positional {
            args.push(arg.clone());
        }
        for (name, value) in &parsed.named {
            args.push(Value::Pair(name.clone(), Box::new(value.clone())));
        }

        // Set $*USAGE (read-only)
        let all_candidates = self.collect_main_candidates();
        let usage_text = self.generate_usage_from_candidates(&all_candidates);
        self.env
            .insert("$*USAGE".to_string(), Value::str(usage_text.clone()));
        self.env
            .insert("*USAGE".to_string(), Value::str(usage_text));
        self.mark_readonly("$*USAGE");
        self.mark_readonly("*USAGE");

        // Call the specific candidate directly
        match self.call_function_def(candidate, &args) {
            Ok(_) => Ok(()),
            Err(e) if e.return_value.is_some() => Ok(()),
            Err(e) if e.message.is_empty() => Ok(()),
            Err(e) => Err(e),
        }
    }

    /// Handle MAIN dispatch failure: call GENERATE-USAGE or auto-generate usage.
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
            let msg = result.to_string_value();
            let output = format!("{}\n", msg);
            self.stderr_output.push_str(&output);
            eprint!("{}", output);
        } else {
            let output = format!("Usage:\n{}\n", usage);
            self.stderr_output.push_str(&output);
            eprint!("{}", output);
        }

        self.exit_code = 2;
        Ok(())
    }

    /// Generate usage message from all MAIN candidates.
    fn generate_usage_message(&self, main_def: &FunctionDef) -> String {
        let all_candidates = self.collect_main_candidates();
        if all_candidates.len() > 1 {
            self.generate_usage_from_candidates(&all_candidates)
        } else {
            self.generate_usage_from_candidates(std::slice::from_ref(main_def))
        }
    }

    /// Generate usage text from a list of MAIN candidates.
    fn generate_usage_from_candidates(&self, candidates: &[FunctionDef]) -> String {
        let program = self
            .env
            .get("$*PROGRAM-NAME")
            .map(|v| v.to_string_value())
            .unwrap_or_else(|| "program".to_string());

        let mut lines = Vec::new();
        for candidate in candidates {
            let mut parts = Vec::new();
            parts.push(format!("  {}", program));
            for pd in &candidate.param_defs {
                if pd.named {
                    let name = pd.name.trim_start_matches(['$', '@', '%']);
                    let is_bool = pd.type_constraint.as_ref().is_some_and(|t| t == "Bool");
                    if is_bool {
                        parts.push(format!("[--{}]", name));
                    } else if pd.required {
                        parts.push(format!("--{}", name));
                    } else {
                        parts.push(format!("[--{}=<value>]", name));
                    }
                } else if pd.slurpy {
                    let name = pd.name.trim_start_matches(['$', '@', '%', '*']);
                    parts.push(format!("[<{}>...]", name));
                } else if pd.double_slurpy {
                    // Skip slurpy hash in usage
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
}
