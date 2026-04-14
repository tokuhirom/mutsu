use super::super::*;
use super::regex_helpers::NamedRegexLookupSpec;
use crate::symbol::Symbol;

impl Interpreter {
    pub(super) fn resolve_token_defs_in_pkg(&self, name: &str, pkg: &str) -> Vec<FunctionDef> {
        let mut out = Vec::new();
        if name.contains("::") {
            if let Some(defs) = self.token_defs.get(&Symbol::intern(name)) {
                out.extend(defs.clone());
            }
            let sym_prefix_angle = format!("{name}:sym<");
            let sym_prefix_french = format!("{name}:sym\u{ab}");
            let mut sym_keys: Vec<String> = self
                .token_defs
                .keys()
                .map(|key| key.resolve())
                .filter(|key| {
                    key.starts_with(&sym_prefix_angle) || key.starts_with(&sym_prefix_french)
                })
                .collect();
            sym_keys.sort();
            for key in &sym_keys {
                if let Some(defs) = self.token_defs.get(&Symbol::intern(key)) {
                    out.extend(defs.clone());
                }
            }
            // Walk MRO for qualified names
            if out.is_empty()
                && let Some(pos) = name.rfind("::")
            {
                let qual_pkg = &name[..pos];
                let token_name = &name[pos + 2..];
                for ancestor in self.mro_readonly(qual_pkg) {
                    if ancestor == qual_pkg {
                        continue;
                    }
                    self.collect_token_defs_for_scope(&ancestor, token_name, &mut out);
                    if !out.is_empty() {
                        break;
                    }
                }
            }
            return out;
        }
        if !pkg.is_empty() {
            // Walk MRO of pkg
            for scope in self.mro_readonly(pkg) {
                self.collect_token_defs_for_scope(&scope, name, &mut out);
                if !out.is_empty() {
                    return out;
                }
            }
        }
        self.collect_token_defs_for_scope("GLOBAL", name, &mut out);
        out
    }

    pub(super) fn resolve_token_patterns_with_args_in_pkg(
        &self,
        name: &str,
        pkg: &str,
        arg_values: &[Value],
    ) -> Vec<(String, String, Option<String>)> {
        let mut out = Vec::new();
        for def in self.resolve_token_defs_in_pkg(name, pkg) {
            let mut interp = Interpreter {
                env: self.env.clone(),
                functions: self.functions.clone(),
                proto_functions: self.proto_functions.clone(),
                token_defs: self.token_defs.clone(),
                current_package: def.package.resolve(),
                ..Default::default()
            };
            let saved_env = interp.env.clone();
            if interp
                .bind_function_args_values(&def.param_defs, &def.params, arg_values)
                .is_ok()
            {
                interp
                    .routine_stack
                    .push((def.package.resolve(), def.name.resolve()));
                let result = interp.eval_block_value(&def.body);
                interp.routine_stack.pop();
                let value = match result {
                    Ok(v) => Some(v),
                    Err(e) if e.return_value.is_some() => e.return_value,
                    Err(_) => None,
                };
                if let Some(value) = value {
                    let pattern = match value {
                        Value::Regex(pat) => pat.to_string(),
                        Value::Str(s) => s.to_string(),
                        Value::Nil => String::new(),
                        other => other.to_string_value(),
                    };
                    // Bake the bound parameter values into any `{ ... }` code
                    // blocks of the pattern. This is needed because regex code
                    // blocks execute in the outer interpreter env (which does
                    // not contain the subrule's bound params).
                    let param_names: Vec<String> = def
                        .param_defs
                        .iter()
                        .filter(|pd| !pd.name.is_empty() && !pd.slurpy)
                        .map(|pd| {
                            pd.name
                                .trim_start_matches([':', '@', '%', '&', '!', '.'])
                                .to_string()
                        })
                        .collect();
                    let pattern =
                        interp.bake_bound_params_into_regex_code_blocks(&pattern, &param_names);
                    let pattern = interp.interpolate_bound_regex_scalars(&pattern);
                    if let Ok(instantiated) = interp.instantiate_named_regex_arg_calls(&pattern) {
                        let sym_val = Self::extract_sym_adverb(&def.name.resolve());
                        out.push((instantiated, def.package.resolve(), sym_val));
                    }
                }
            }
            interp.env = saved_env;
        }
        out
    }

    pub(super) fn resolve_named_regex_candidates_in_pkg(
        &self,
        spec: &NamedRegexLookupSpec,
        pkg: &str,
        arg_values: &[Value],
    ) -> Vec<(String, String, Option<String>)> {
        if arg_values.is_empty() {
            self.resolve_token_patterns_static_in_pkg(&spec.lookup_name, pkg)
        } else {
            self.resolve_token_patterns_with_args_in_pkg(&spec.lookup_name, pkg, arg_values)
        }
    }

    pub(super) fn format_named_regex_arg_value(value: &Value) -> String {
        match value {
            Value::Str(s) => {
                let escaped = s.replace('\\', "\\\\").replace('"', "\\\"");
                format!("\"{escaped}\"")
            }
            Value::Bool(true) => "True".to_string(),
            Value::Bool(false) => "False".to_string(),
            Value::Nil => "Nil".to_string(),
            _ => value.to_string_value(),
        }
    }
}
