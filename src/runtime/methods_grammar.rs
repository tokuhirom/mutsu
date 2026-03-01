use super::*;
use crate::symbol::Symbol;

impl Interpreter {
    pub(super) fn dispatch_package_parse(
        &mut self,
        package_name: &str,
        method: &str,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let mut source_arg: Option<String> = None;
        let mut start_rule = "TOP".to_string();
        let mut rule_args: Vec<Value> = Vec::new();
        for arg in args {
            if let Value::Pair(key, value) = arg {
                if key == "rule" || key == "token" {
                    start_rule = value.to_string_value();
                } else if key == "args" {
                    // :args(\(42)) passes a Capture; :args(42,) passes an Array
                    match value.as_ref() {
                        Value::Capture {
                            positional,
                            named: _,
                        } => {
                            rule_args = positional.clone();
                        }
                        Value::Array(arr, _) => {
                            rule_args = arr.as_ref().clone();
                        }
                        other => {
                            rule_args = vec![other.clone()];
                        }
                    }
                }
                continue;
            }
            if source_arg.is_none() {
                source_arg = Some(arg.to_string_value());
            }
        }
        let Some(source_text) = source_arg else {
            return Err(RuntimeError::new(
                "Too few positionals passed; expected 2 arguments but got 1",
            ));
        };
        let text = if method == "parsefile" {
            match std::fs::read_to_string(&source_text) {
                Ok(contents) => contents,
                Err(err) => return Err(RuntimeError::new(err.to_string())),
            }
        } else {
            source_text
        };

        let saved_package = self.current_package.clone();
        let saved_topic = self.env.get("_").cloned();
        let saved_made = self.env.get("made").cloned();
        self.env.remove("made");
        self.current_package = package_name.to_string();
        let has_start_rule =
            self.resolve_token_defs(&start_rule).is_some() || self.has_proto_token(&start_rule);
        if !has_start_rule {
            self.current_package = saved_package;
            if let Some(old_topic) = saved_topic {
                self.env.insert("_".to_string(), old_topic);
            } else {
                self.env.remove("_");
            }
            return Err(RuntimeError::new(format!(
                "X::Method::NotFound: Unknown method value dispatch (fallback disabled): {}",
                method
            )));
        }
        self.env.insert("_".to_string(), Value::Str(text.clone()));
        let result = (|| -> Result<Value, RuntimeError> {
            let pattern = match self.eval_token_call_values(&start_rule, &rule_args) {
                Ok(Some(pattern)) => pattern,
                Ok(None) => {
                    self.env.insert("/".to_string(), Value::Nil);
                    return Ok(self.parse_failure_for_pattern(&text, None));
                }
                Err(err)
                    if err
                        .message
                        .contains("No matching candidates for proto token") =>
                {
                    self.env.insert("/".to_string(), Value::Nil);
                    return Ok(self.parse_failure_for_pattern(&text, None));
                }
                Err(err) => return Err(err),
            };

            // Bind rule args to the env so code assertions { ... } can access them
            if !rule_args.is_empty()
                && let Some(def) = self
                    .resolve_token_defs(&start_rule)
                    .and_then(|defs| defs.into_iter().next())
            {
                let _ = self.bind_function_args_values(&def.param_defs, &def.params, &rule_args);
            }

            let captures = if method == "parse" || method == "parsefile" {
                self.regex_match_with_captures_full_from_start(&pattern, &text)
            } else {
                self.regex_match_with_captures(&pattern, &text)
            };
            let Some(captures) = captures else {
                self.env.insert("/".to_string(), Value::Nil);
                return Ok(self.parse_failure_for_pattern(&text, Some(&pattern)));
            };
            self.execute_regex_code_blocks(&captures.code_blocks);
            if captures.from != 0 {
                self.env.insert("/".to_string(), Value::Nil);
                return Ok(self.parse_failure_for_pattern(&text, Some(&pattern)));
            }
            if (method == "parse" || method == "parsefile") && captures.to != text.chars().count() {
                self.env.insert("/".to_string(), Value::Nil);
                return Ok(self.make_parse_failure_value(&text, captures.to));
            }
            for (i, v) in captures.positional.iter().enumerate() {
                self.env.insert(i.to_string(), Value::Str(v.clone()));
            }
            let match_obj = Value::make_match_object_full(
                captures.matched,
                captures.from as i64,
                captures.to as i64,
                &captures.positional,
                &captures.named,
                &captures.named_subcaps,
                Some(&text),
            );
            let match_obj = if let Value::Instance {
                class_name,
                attributes,
                ..
            } = &match_obj
            {
                let mut attrs = attributes.as_ref().clone();
                if let Some(ast) = self.env.get("made").cloned() {
                    attrs.insert("ast".to_string(), ast);
                }
                Value::make_instance(*class_name, attrs)
            } else {
                match_obj
            };
            // Set named capture env vars from match object
            if let Value::Instance { attributes, .. } = &match_obj
                && let Some(Value::Hash(named_hash)) = attributes.get("named")
            {
                for (k, v) in named_hash.iter() {
                    self.env.insert(format!("<{}>", k), v.clone());
                }
            }
            self.env.insert("/".to_string(), match_obj.clone());
            Ok(match_obj)
        })();

        self.current_package = saved_package;
        if let Some(old_topic) = saved_topic {
            self.env.insert("_".to_string(), old_topic);
        } else {
            self.env.remove("_");
        }
        if let Some(old_made) = saved_made {
            self.env.insert("made".to_string(), old_made);
        } else {
            self.env.remove("made");
        }
        result
    }

    pub(super) fn parse_failure_for_pattern(&mut self, text: &str, pattern: Option<&str>) -> Value {
        let best_end = pattern
            .map(|pat| self.longest_complete_prefix_end(pat, text))
            .unwrap_or(0);
        self.make_parse_failure_value(text, best_end)
    }

    pub(super) fn longest_complete_prefix_end(&mut self, pattern: &str, text: &str) -> usize {
        let chars: Vec<char> = text.chars().collect();
        for end in (0..=chars.len()).rev() {
            let prefix: String = chars[..end].iter().collect();
            if let Some(captures) = self.regex_match_with_captures(pattern, &prefix)
                && captures.from == 0
                && captures.to == end
            {
                return end;
            }
        }
        0
    }

    pub(super) fn make_parse_failure_value(&self, text: &str, best_end: usize) -> Value {
        let chars: Vec<char> = text.chars().collect();
        let pos = best_end.saturating_sub(1).min(chars.len());
        let line_start = chars[..pos]
            .iter()
            .rposition(|ch| *ch == '\n')
            .map(|idx| idx + 1)
            .unwrap_or(0);
        let line_end = chars[pos..]
            .iter()
            .position(|ch| *ch == '\n')
            .map(|offset| pos + offset)
            .unwrap_or(chars.len());
        let pre: String = chars[line_start..pos].iter().collect();
        let post = if pos >= chars.len() || chars[pos] == '\n' {
            "<EOL>".to_string()
        } else {
            chars[pos..line_end].iter().collect()
        };
        let line = chars[..pos].iter().filter(|ch| **ch == '\n').count() as i64 + 1;

        let mut ex_attrs = HashMap::new();
        ex_attrs.insert("reason".to_string(), Value::Str("unknown".to_string()));
        ex_attrs.insert("filename".to_string(), Value::Str("<anon>".to_string()));
        ex_attrs.insert("pos".to_string(), Value::Int(pos as i64));
        ex_attrs.insert("line".to_string(), Value::Int(line));
        ex_attrs.insert("pre".to_string(), Value::Str(pre));
        ex_attrs.insert("post".to_string(), Value::Str(post));
        ex_attrs.insert("highexpect".to_string(), Value::array(Vec::new()));
        let exception = Value::make_instance(Symbol::intern("X::Syntax::Confused"), ex_attrs);

        let mut failure_attrs = HashMap::new();
        failure_attrs.insert("exception".to_string(), exception);
        failure_attrs.insert("handled".to_string(), Value::Bool(false));
        Value::make_instance(Symbol::intern("Failure"), failure_attrs)
    }
}
