use super::super::*;
use super::regex_helpers::{CaseFoldIter, matches_named_builtin};

impl Interpreter {
    /// Evaluate a closure interpolation `<{ code }>` inside a regex.
    /// Returns the regex pattern string to match against.
    pub(super) fn eval_regex_closure_interpolation(
        &self,
        code: &str,
        caps: &RegexCaptures,
        target: &str,
    ) -> Option<String> {
        let mut env = self.make_regex_eval_env(caps);
        // Set $_ to the match target string
        env.insert("_".to_string(), Value::str(target.to_string()));
        // Add variables declared via :my inside the regex
        for (k, v) in &caps.regex_vars {
            env.insert(k.clone(), v.clone());
        }
        let (stmts, _) = crate::parse_dispatch::parse_source(code).ok()?;
        let mut interp = Interpreter {
            env,
            functions: self.functions.clone(),
            current_package: self.current_package.clone(),
            ..Default::default()
        };
        match interp.eval_block_value(&stmts) {
            Ok(val) => match val {
                Value::Regex(pat) => Some(pat.to_string()),
                other => Some(Self::regex_escape_literal(&other.to_string_value())),
            },
            Err(e) => e
                .return_value
                .map(|val| Self::regex_escape_literal(&val.to_string_value())),
        }
    }

    /// Evaluate a code assertion inside a regex.
    /// Sets up $0, $1, etc. from current captures, evaluates the code,
    /// and returns whether the result is truthy.
    pub(super) fn eval_regex_code_assertion(&self, code: &str, caps: &RegexCaptures) -> bool {
        // We need to evaluate in a mutable context but `self` is &self.
        // Use unsafe interior mutability pattern via a clone-and-eval approach.
        // Parse the code first.
        let (stmts, _) = match crate::parse_dispatch::parse_source(code) {
            Ok(result) => result,
            Err(_) => return false,
        };
        // Create a minimal interpreter with env set up
        let mut env = self.env.clone();
        // Set positional capture variables ($0, $1, etc.)
        for (i, val) in caps.positional.iter().enumerate() {
            env.insert(i.to_string(), Value::str(val.clone()));
        }
        // Build $/ as an array for $/[n] access
        let match_list: Vec<Value> = caps
            .positional
            .iter()
            .map(|s| Value::str(s.clone()))
            .collect();
        env.insert("/".to_string(), Value::array(match_list));
        // Set named captures
        for (k, v) in &caps.named {
            let value = if v.len() == 1 {
                Value::str(v[0].clone())
            } else {
                Value::array(v.iter().cloned().map(Value::str).collect())
            };
            env.insert(format!("<{}>", k), value);
        }
        // Evaluate the code in a fresh interpreter with this env
        let mut interp = Interpreter {
            env,
            functions: self.functions.clone(),
            current_package: self.current_package.clone(),
            ..Default::default()
        };
        match interp.eval_block_value(&stmts) {
            Ok(val) => val.truthy(),
            Err(_) => false,
        }
    }

    /// Execute code blocks collected during regex matching for side effects.
    pub(in crate::runtime) fn execute_regex_code_blocks(
        &mut self,
        code_blocks: &[CodeBlockContext],
    ) {
        for ctx in code_blocks {
            let Ok((stmts, _)) = crate::parse_dispatch::parse_source(&ctx.code) else {
                continue;
            };
            // Set up $/ as a match object for the matched-so-far text
            let match_obj = Value::make_match_object_with_captures(
                ctx.matched_so_far.clone(),
                0,
                ctx.matched_so_far.chars().count() as i64,
                &[],
                &HashMap::new(),
            );
            self.env.insert("/".to_string(), match_obj.clone());
            // Set up $¢ (current match cursor) — same as $/ for in-progress match
            self.env.insert("\u{00A2}".to_string(), match_obj);
            // Set up positional captures ($0, $1, ...)
            for (i, val) in ctx.positional.iter().enumerate() {
                let pos_match = Value::make_match_object_with_captures(
                    val.clone(),
                    0,
                    val.chars().count() as i64,
                    &[],
                    &HashMap::new(),
                );
                self.env.insert(i.to_string(), pos_match);
            }
            // Set up named captures as $<name> variables
            let ast_hint = self.env.get("made").cloned().unwrap_or(Value::Nil);
            for (k, v) in &ctx.named {
                let to_match_with_ast = |text: &str, ast: &Value| -> Value {
                    let match_obj = Value::make_match_object_with_captures(
                        text.to_string(),
                        0,
                        text.chars().count() as i64,
                        &[],
                        &HashMap::new(),
                    );
                    if let Value::Instance {
                        class_name,
                        attributes,
                        ..
                    } = match_obj
                    {
                        let mut attrs = attributes.as_ref().clone();
                        attrs.insert("ast".to_string(), ast.clone());
                        Value::make_instance(class_name, attrs)
                    } else {
                        match_obj
                    }
                };
                let value = if v.len() == 1 {
                    to_match_with_ast(&v[0], &ast_hint)
                } else {
                    Value::array(
                        v.iter()
                            .map(|s| to_match_with_ast(s, &ast_hint))
                            .collect::<Vec<_>>(),
                    )
                };
                self.env.insert(format!("<{}>", k), value);
            }
            // Snapshot env keys and values before execution
            let snapshot: HashMap<String, String> = self
                .env
                .iter()
                .map(|(k, v)| (k.clone(), format!("{:?}", v)))
                .collect();
            let _ = self.eval_block_value(&stmts);
            // Record changed env variables as pending local updates for the outer VM
            for (k, v) in &self.env {
                let old_repr = snapshot.get(k).map(|s| s.as_str()).unwrap_or("");
                let new_repr = format!("{:?}", v);
                if old_repr != new_repr {
                    self.pending_local_updates.push((k.clone(), v.clone()));
                }
            }
        }
    }

    pub(super) fn regex_match_class_ignorecase(
        &self,
        class: &CharClass,
        c: char,
        ignore_case: bool,
    ) -> bool {
        if ignore_case {
            // When ignore_case is set, check if any case variant of c matches the class
            for variant in CaseFoldIter::new(c) {
                if self.regex_match_class(class, variant) {
                    return true;
                }
            }
            return false;
        }
        self.regex_match_class(class, c)
    }

    pub(super) fn regex_match_class(&self, class: &CharClass, c: char) -> bool {
        let mut matched = false;
        for item in &class.items {
            match item {
                ClassItem::Range(a, b) => {
                    if *a <= c && c <= *b {
                        matched = true;
                        break;
                    }
                }
                ClassItem::Char(ch) => {
                    if *ch == c {
                        matched = true;
                        break;
                    }
                }
                ClassItem::Digit => {
                    if c.is_ascii_digit() {
                        matched = true;
                        break;
                    }
                }
                ClassItem::Word => {
                    if c.is_alphanumeric() || c == '_' {
                        matched = true;
                        break;
                    }
                }
                ClassItem::Space => {
                    if c.is_whitespace() {
                        matched = true;
                        break;
                    }
                }
                ClassItem::NamedBuiltin(name) => {
                    if matches_named_builtin(name, c) {
                        matched = true;
                        break;
                    }
                }
                ClassItem::UnicodePropItem { name, negated } => {
                    let prop_match = check_unicode_property(name, c);
                    if if *negated { !prop_match } else { prop_match } {
                        matched = true;
                        break;
                    }
                }
            }
        }
        if class.negated { !matched } else { matched }
    }

    /// Find all non-overlapping regex matches, returning (start, end) char-index pairs.
    pub(crate) fn regex_find_all(&self, pattern: &str, text: &str) -> Vec<(usize, usize)> {
        let parsed = match self.parse_regex(pattern) {
            Some(p) => p,
            None => return Vec::new(),
        };
        let pkg = self.current_package.clone();
        let chars: Vec<char> = text.chars().collect();
        let mut results = Vec::new();
        let mut pos = 0;
        while pos <= chars.len() {
            let search_start = if parsed.anchor_start { 0 } else { pos };
            let mut found = None;
            if parsed.anchor_start {
                if pos == 0
                    && let Some(end) = self.regex_match_end_from_in_pkg(&parsed, &chars, 0, &pkg)
                {
                    found = Some((0, end));
                }
            } else {
                for start in search_start..=chars.len() {
                    if let Some(end) =
                        self.regex_match_end_from_in_pkg(&parsed, &chars, start, &pkg)
                    {
                        found = Some((start, end));
                        break;
                    }
                }
            }
            match found {
                Some((start, end)) => {
                    results.push((start, end));
                    // Advance past the match (at least 1 to avoid infinite loop)
                    pos = if end > start { end } else { start + 1 };
                }
                None => break,
            }
            if parsed.anchor_start {
                break;
            }
        }
        results
    }
}
