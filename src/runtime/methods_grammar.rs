use super::*;
use crate::symbol::Symbol;

impl Interpreter {
    fn first_goal_name(pattern: &RegexPattern) -> Option<String> {
        for token in &pattern.tokens {
            match &token.atom {
                RegexAtom::GoalMatch { goal_text, .. } => return Some(goal_text.clone()),
                RegexAtom::Group(inner) | RegexAtom::CaptureGroup(inner) => {
                    if let Some(goal) = Self::first_goal_name(inner) {
                        return Some(goal);
                    }
                }
                RegexAtom::Alternation(alts) | RegexAtom::SequentialAlternation(alts) => {
                    for alt in alts {
                        if let Some(goal) = Self::first_goal_name(alt) {
                            return Some(goal);
                        }
                    }
                }
                _ => {}
            }
        }
        None
    }

    fn extract_tilde_goal_from_source(pattern: &str) -> Option<String> {
        let mut chars = pattern.chars().peekable();
        while let Some(ch) = chars.next() {
            if ch != '~' {
                continue;
            }
            while chars.peek().is_some_and(|c| c.is_whitespace()) {
                chars.next();
            }
            let open = chars.next()?;
            let close = match open {
                '\'' => '\'',
                '"' => '"',
                '\u{2018}' | '\u{201A}' => '\u{2019}',
                '\u{201C}' | '\u{201E}' => '\u{201D}',
                '\u{FF62}' => '\u{FF63}',
                other => return Some(other.to_string()),
            };
            let mut body = String::new();
            for ch in chars.by_ref() {
                if ch == close {
                    break;
                }
                body.push(ch);
            }
            return Some(format!("{body:?}"));
        }
        None
    }

    fn make_goal_failure_value(&self, goal: &str, pos: usize) -> Value {
        let msg = format!("Cannot find {goal} near position {pos}");
        let mut ex_attrs = HashMap::new();
        ex_attrs.insert("message".to_string(), Value::str(msg));
        let exception = Value::make_instance(Symbol::intern("X::AdHoc"), ex_attrs);

        let mut failure_attrs = HashMap::new();
        failure_attrs.insert("exception".to_string(), exception);
        failure_attrs.insert("handled".to_string(), Value::Bool(true));
        Value::make_instance(Symbol::intern("Failure"), failure_attrs)
    }

    pub(super) fn dispatch_package_parse(
        &mut self,
        package_name: &str,
        method: &str,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let mut source_arg: Option<String> = None;
        let mut start_rule = "TOP".to_string();
        let mut rule_args: Vec<Value> = Vec::new();
        let mut actions_obj: Option<Value> = None;
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
                } else if key == "actions" {
                    actions_obj = Some(value.as_ref().clone());
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
        self.env.insert("_".to_string(), Value::str(text.clone()));
        Self::clear_pending_goal_failure();
        let is_full_parse = method == "parse" || method == "parsefile";
        let result = (|| -> Result<Value, RuntimeError> {
            let pattern = match self.eval_token_call_values(&start_rule, &rule_args) {
                Ok(Some(pattern)) => pattern,
                Ok(None) => {
                    // Check for pending regex error (e.g., <sym> used outside proto regex)
                    if let Some(err) = Self::take_pending_regex_error() {
                        return Err(err);
                    }
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
            // Check for pending regex error (e.g., <sym> used outside proto regex)
            if let Some(err) = Self::take_pending_regex_error() {
                return Err(err);
            }
            let Some(captures) = captures else {
                let goal = Self::take_pending_goal_failure().or_else(|| {
                    self.parse_regex(&pattern)
                        .and_then(|p| Self::first_goal_name(&p))
                        .map(|goal| (goal, text.chars().count()))
                });
                if let Some((goal, pos)) = goal {
                    match self.call_method_with_values(
                        Value::Package(Symbol::intern(package_name)),
                        "FAILGOAL",
                        vec![Value::str(goal.clone())],
                    ) {
                        Ok(_) => {
                            self.env.insert("/".to_string(), Value::Nil);
                            return Ok(self.make_goal_failure_value(&goal, pos));
                        }
                        Err(err) if err.message.contains("X::Method::NotFound") => {}
                        Err(err) => return Err(err),
                    }
                    self.env.insert("/".to_string(), Value::Nil);
                    return Ok(self.make_goal_failure_value(&goal, pos));
                }
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
                self.env.insert(i.to_string(), Value::str(v.clone()));
            }
            let match_obj = Value::make_match_object_full(
                captures.matched,
                captures.from as i64,
                captures.to as i64,
                &captures.positional,
                &captures.named,
                &captures.named_subcaps,
                &captures.positional_subcaps,
                &captures.positional_quantified,
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
                if let Some(ref act) = actions_obj {
                    attrs.insert("actions".to_string(), act.clone());
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

            // Invoke action methods if :actions was provided
            let match_obj = if let Some(ref mut actions) = actions_obj {
                let result = self.invoke_grammar_actions(match_obj, actions, &start_rule)?;
                // Update the actions attribute on the final Match to reflect
                // any mutations that occurred during action method dispatch.
                if let Value::Instance {
                    class_name,
                    attributes,
                    id,
                    ..
                } = &result
                {
                    let mut attrs = attributes.as_ref().clone();
                    attrs.insert("actions".to_string(), actions.clone());
                    Value::make_instance_with_id(*class_name, attrs, *id)
                } else {
                    result
                }
            } else {
                match_obj
            };

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
        // In Raku 6.c, Grammar.parse/parsefile returns Nil on failure.
        // In 6.e+, it returns a Failure object.
        if is_full_parse
            && !crate::parser::current_language_version().starts_with("6.e")
            && let Ok(Value::Instance { class_name, .. }) = &result
            && class_name == "Failure"
        {
            return Ok(Value::Nil);
        }
        result
    }

    /// Walk the match tree bottom-up and invoke action methods on the actions object.
    fn invoke_grammar_actions(
        &mut self,
        match_obj: Value,
        actions: &mut Value,
        rule_name: &str,
    ) -> Result<Value, RuntimeError> {
        let (class_name, attributes) = if let Value::Instance {
            class_name,
            attributes,
            ..
        } = &match_obj
        {
            (*class_name, attributes.as_ref().clone())
        } else {
            return Ok(match_obj);
        };

        // First, recursively process child named captures (bottom-up order)
        let mut updated_attrs = attributes.clone();
        if let Some(Value::Hash(named_hash)) = attributes.get("named") {
            let mut updated_named = named_hash.as_ref().clone();
            // Sort children by their match position (from attribute) to preserve grammar order
            let mut children: Vec<(String, Value)> = named_hash
                .iter()
                .map(|(k, v)| (k.clone(), v.clone()))
                .collect();
            children.sort_by_key(|(_, v)| {
                if let Value::Instance { attributes, .. } = v
                    && let Some(Value::Int(from)) = attributes.get("from")
                {
                    return *from;
                }
                0
            });
            for (child_name, child_match) in children {
                let updated_child =
                    self.invoke_grammar_actions(child_match, actions, &child_name)?;
                updated_named.insert(child_name, updated_child);
            }
            updated_attrs.insert("named".to_string(), Value::hash(updated_named));
        }

        // Rebuild match_obj with updated children
        let match_obj = Value::make_instance(class_name, updated_attrs.clone());

        // Set $/ to this match and try calling actions.{rule_name}(match)
        self.env.insert("/".to_string(), match_obj.clone());
        self.env.remove("made");
        self.action_made = None;
        // Set named capture env vars (<a>, <b>, etc.) so $<a> works inside action methods
        if let Some(Value::Hash(named_hash)) = updated_attrs.get("named") {
            for (k, v) in named_hash.iter() {
                self.env.insert(format!("<{}>", k), v.clone());
            }
        }
        // Also set $_ to the match (for `.make:` syntax)
        let saved_topic = self.env.get("_").cloned();
        self.env.insert("_".to_string(), match_obj.clone());

        // For protoregex :sym<> variants, try dispatching to the specific
        // action method (e.g., alt:sym<baz>) first.
        let sym_method_name = if let Some(Value::Str(sym_val)) = updated_attrs.get("sym_variant") {
            // Use «» delimiters when the sym value contains '<' or '>'
            // to match method names stored with French-quote delimiters
            if sym_val.contains('<') || sym_val.contains('>') {
                Some(format!("{rule_name}:sym\u{ab}{sym_val}\u{bb}"))
            } else {
                Some(format!("{rule_name}:sym<{sym_val}>"))
            }
        } else {
            None
        };
        let method_result = if let Some(ref sym_name) = sym_method_name {
            let result =
                self.call_method_with_values(actions.clone(), sym_name, vec![match_obj.clone()]);
            match result {
                Err(e) if e.message.contains("X::Method::NotFound") => {
                    // Fall back to plain rule name
                    self.call_method_with_values(
                        actions.clone(),
                        rule_name,
                        vec![match_obj.clone()],
                    )
                }
                other => other,
            }
        } else {
            self.call_method_with_values(actions.clone(), rule_name, vec![match_obj.clone()])
        };

        // After the method call, if actions is an Instance, its attributes
        // may have been mutated.  Retrieve the updated version from env so
        // subsequent child/rule calls see the latest state.
        if let Value::Instance {
            class_name: act_cn,
            id: act_id,
            ..
        } = actions
        {
            for v in self.env.values() {
                if let Value::Instance {
                    class_name: cn,
                    id,
                    attributes,
                    ..
                } = v
                    && cn == act_cn
                    && *id == *act_id
                {
                    *actions = Value::Instance {
                        class_name: *cn,
                        attributes: attributes.clone(),
                        id: *id,
                    };
                    break;
                }
            }
        }

        // Restore $_
        if let Some(old_topic) = saved_topic {
            self.env.insert("_".to_string(), old_topic);
        } else {
            self.env.remove("_");
        }

        match method_result {
            Ok(_) => {}
            Err(e) if e.message.contains("X::Method::NotFound") => {
                // No action method for this rule — silently skip
            }
            Err(e) => return Err(e),
        }

        // If make() was called (via action_made which persists across env restore),
        // update .ast on match
        let final_obj = if let Some(ast) = self.action_made.take() {
            let mut attrs = updated_attrs;
            attrs.insert("ast".to_string(), ast);
            // Preserve actions attribute if present
            if let Some(act_val) = attributes.get("actions") {
                attrs.insert("actions".to_string(), act_val.clone());
            }
            Value::make_instance(class_name, attrs)
        } else {
            match_obj
        };

        Ok(final_obj)
    }

    pub(super) fn parse_failure_for_pattern(&mut self, text: &str, pattern: Option<&str>) -> Value {
        if let Some(goal) = pattern.and_then(Self::extract_tilde_goal_from_source) {
            return self.make_goal_failure_value(&goal, text.chars().count());
        }
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
        ex_attrs.insert("reason".to_string(), Value::str_from("unknown"));
        ex_attrs.insert("filename".to_string(), Value::str_from("<anon>"));
        ex_attrs.insert("pos".to_string(), Value::Int(pos as i64));
        ex_attrs.insert("line".to_string(), Value::Int(line));
        ex_attrs.insert("pre".to_string(), Value::str(pre));
        ex_attrs.insert("post".to_string(), Value::str(post));
        ex_attrs.insert("highexpect".to_string(), Value::array(Vec::new()));
        let exception = Value::make_instance(Symbol::intern("X::Syntax::Confused"), ex_attrs);

        let mut failure_attrs = HashMap::new();
        failure_attrs.insert("exception".to_string(), exception);
        // TODO: Grammar.parse/subparse should return a failed Match, not a Failure.
        // Mark as handled so that stringifying doesn't throw prematurely.
        failure_attrs.insert("handled".to_string(), Value::Bool(true));
        Value::make_instance(Symbol::intern("Failure"), failure_attrs)
    }
}
