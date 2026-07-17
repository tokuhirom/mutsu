use super::*;
use crate::symbol::Symbol;
use crate::value::ValueView;

impl Interpreter {
    /// A rule/token body may declare a *dynamic* variable inline
    /// (`rule deal { :my %*PLAYED = (); … }`). Such a variable lives in the
    /// dynamic scope of the whole parse, so an action method invoked while the
    /// grammar matches (`method card($/) { … %*PLAYED{$card}++ … }`) must see and
    /// mutate the same one. Scan every rule of the grammar being parsed (its
    /// package, MRO-walked) for `:my $*/%*/@*NAME = INIT;` declarations, evaluate
    /// them into `self.env` (where dynamic-var lookup finds them), and return the
    /// prior values so the caller can restore them when the parse ends.
    fn establish_grammar_dynamic_vars(&mut self, package: &str) -> Vec<(String, Option<Value>)> {
        // Collect the grammar's rule patterns (this package + ancestors).
        let mut patterns: Vec<String> = Vec::new();
        {
            let mut packages = vec![package.to_string()];
            packages.extend(
                self.mro_readonly(package)
                    .into_iter()
                    .filter(|p| p != package),
            );
            let prefixes: Vec<String> = packages.iter().map(|p| format!("{p}::")).collect();
            let defs: Vec<std::sync::Arc<FunctionDef>> = self
                .registry()
                .token_defs
                .iter()
                .filter(|(k, _)| {
                    let ks = k.resolve();
                    prefixes.iter().any(|pre| ks.starts_with(pre.as_str()))
                })
                .flat_map(|(_, v)| v.iter().cloned())
                .collect();
            for def in &defs {
                if let Some(pat) = Self::token_pattern_from_def(def) {
                    patterns.push(pat);
                }
            }
        }
        // Extract `:my $*/%*/@*NAME = INIT;` declarations from the patterns.
        let mut decls: Vec<String> = Vec::new();
        for pat in &patterns {
            Self::collect_dynamic_var_decls(pat, &mut decls);
        }
        // Evaluate each declaration in `self.env`, saving the prior value.
        let mut saved: Vec<(String, Option<Value>)> = Vec::new();
        for (code, var_key) in decls
            .iter()
            .filter_map(|d| Self::dynamic_decl_var_key(d).map(|k| (d, k)))
        {
            if saved.iter().any(|(k, _)| k == &var_key) {
                continue;
            }
            saved.push((var_key.clone(), self.env.get(&var_key).cloned()));
            let source = format!("{code};");
            if let Ok((stmts, _)) = crate::parse_dispatch::parse_source(&source) {
                let _ = self.eval_block_value(&stmts);
            }
        }
        saved
    }

    /// Collect `:my $*/%*/@*… = …;` declaration substrings (main-slang code
    /// between `:my ` and the terminating `;`) from a rule pattern.
    fn collect_dynamic_var_decls(pattern: &str, out: &mut Vec<String>) {
        let bytes: Vec<char> = pattern.chars().collect();
        let mut i = 0;
        while i < bytes.len() {
            let rest: String = bytes[i..].iter().collect();
            if let Some(after) = rest
                .strip_prefix(":my ")
                .or_else(|| rest.strip_prefix(":our "))
            {
                // Only dynamic (`*`-twigil) declarations concern us.
                let trimmed = after.trim_start();
                if matches!(trimmed.chars().next(), Some('$' | '@' | '%'))
                    && trimmed.chars().nth(1) == Some('*')
                {
                    // Collect `my … ` up to the `;`.
                    let decl: String = rest
                        .strip_prefix(':')
                        .unwrap_or(&rest)
                        .chars()
                        .take_while(|&c| c != ';')
                        .collect();
                    out.push(decl.trim().to_string());
                }
                // Skip past this `:my`.
                i += 4;
                continue;
            }
            i += 1;
        }
    }

    /// The env key (`%*PLAYED` → `%*PLAYED`) declared by a `my $*/%*/@*NAME …`
    /// declaration string, or `None` if it is not a simple dynamic declaration.
    fn dynamic_decl_var_key(decl: &str) -> Option<String> {
        let rest = decl
            .strip_prefix("my ")
            .or_else(|| decl.strip_prefix("our "))?;
        let rest = rest.trim_start();
        let mut chars = rest.chars();
        let sigil = chars.next()?;
        if !matches!(sigil, '$' | '@' | '%') || chars.next()? != '*' {
            return None;
        }
        let name: String = rest
            .chars()
            .skip(2)
            .take_while(|c| c.is_alphanumeric() || *c == '_' || *c == '-')
            .collect();
        if name.is_empty() {
            return None;
        }
        Some(format!("{sigil}*{name}"))
    }

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
        failure_attrs.insert("handled".to_string(), Value::TRUE);
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
            if let ValueView::Pair(key, value) = arg.view() {
                if key == "rule" || key == "token" {
                    start_rule = value.to_string_value();
                } else if key == "args" {
                    // :args(\(42)) passes a Capture; :args(42,) passes an Array
                    match value.view() {
                        ValueView::Capture {
                            positional,
                            named: _,
                        } => {
                            rule_args = positional.clone();
                        }
                        ValueView::Array(arr, _) => {
                            rule_args = arr.as_ref().clone().items;
                        }
                        _ => {
                            rule_args = vec![value.clone()];
                        }
                    }
                } else if key == "actions" {
                    actions_obj = Some(value.clone());
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

        let saved_package = self.current_package();
        let saved_topic = self.env.get("_").cloned();
        let saved_made = self.env.get("made").cloned();
        self.env.remove("made");
        self.set_current_package(package_name.to_string());
        let has_start_rule =
            self.resolve_token_defs(&start_rule).is_some() || self.has_proto_token(&start_rule);
        if !has_start_rule {
            self.set_current_package(saved_package);
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
        // Expose the `:actions` object for the whole parse (candidate selection in
        // `eval_token_call_values` already runs the pattern, so the assertions can
        // fire before the main match) so that `<?{ ... }>` code assertions
        // referencing `$<x>.made` can run the relevant action method on a
        // just-matched named capture during parsing. raku runs actions
        // incrementally at reduce time; mutsu otherwise only runs them post-parse.
        // See `eval_regex_code_assertion`. Saved/restored so nested/re-entrant
        // parses stay balanced.
        let saved_grammar_actions =
            std::mem::replace(&mut self.current_grammar_actions, actions_obj.clone());
        // Activate the reduce-time dyn-var overlay for action-driven parses so an
        // action that writes a `$*` dynamic var mid-parse (e.g. a delimiter
        // finalizer) affects subsequent subrule matching. No-op overlay cost
        // until a pattern actually interpolates a `$*` var (see the SEEN gate in
        // `interpolate_regex_scalars`). Dropped at scope end, restoring any outer
        // parse's overlay.
        let _dynvar_overlay_guard = actions_obj
            .is_some()
            .then(super::regex::regex_helpers::RegexDynvarOverlayGuard::activate);
        // Establish any `:my $*/%*/@*… = …;` dynamic variables the grammar's rules
        // declare, so action methods run during the match share them (`%*PLAYED`).
        let saved_grammar_dynvars = self.establish_grammar_dynamic_vars(package_name);
        let result = (|| -> Result<Value, RuntimeError> {
            let pattern = match self.eval_token_call_values(&start_rule, &rule_args) {
                Ok(Some(pattern)) => pattern,
                Ok(None) => {
                    // Check for pending regex error (e.g., <sym> used outside proto regex)
                    if let Some(err) = Self::take_pending_regex_error() {
                        return Err(err);
                    }
                    self.env.insert("/".to_string(), Value::NIL);
                    return Ok(self.parse_failure_for_pattern(&text, None));
                }
                Err(err)
                    if err
                        .message
                        .contains("No matching candidates for proto token") =>
                {
                    self.env.insert("/".to_string(), Value::NIL);
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

            // Candidate selection above (`eval_token_call_values`) may have run a
            // preliminary match that evolved the reduce-time dyn-var overlay
            // (e.g. fired a delimiter finalizer). Reset it so the real match
            // begins from the initial dynamic-var state in `self.env`.
            super::regex::regex_helpers::dynvar_overlay_reset_scan();
            // Collect embedded `{ ... }` code blocks eagerly so their side effects
            // (e.g. `regex TOP { x { $x = 42 } }` mutating an outer lexical) persist
            // even when the OVERALL parse fails because the rule matched a prefix but
            // not the whole string (`.parse('xxx')` on a rule matching one `x`).
            // raku runs such blocks as the cursor reaches them, regardless of the
            // final parse verdict.
            let want_eager = (method == "parse" || method == "parsefile")
                && self.has_code_block_in_prefix(&pattern);
            if want_eager {
                self.enable_eager_code_blocks();
            }
            let captures = if method == "parse" || method == "parsefile" {
                self.regex_match_with_captures_full_from_start(&pattern, &text)
            } else {
                self.regex_match_with_captures(&pattern, &text)
            };
            // Always drain the eager buffer (resets the thread-local). Only run it
            // on the FAILURE path; on success the winning `captures.code_blocks`
            // (executed below) are authoritative — the eager buffer over-collects
            // backtracked branches.
            let eager_blocks = if want_eager {
                self.drain_eager_code_blocks()
            } else {
                Vec::new()
            };
            // Check for pending regex error (e.g., <sym> used outside proto regex)
            if let Some(err) = Self::take_pending_regex_error() {
                return Err(err);
            }
            let Some(mut captures) = captures else {
                if !eager_blocks.is_empty() {
                    self.execute_regex_code_blocks(&eager_blocks);
                }
                let goal = Self::take_pending_goal_failure().or_else(|| {
                    self.parse_regex(&pattern)
                        .and_then(|p| Self::first_goal_name(&p))
                        .map(|goal| (goal, text.chars().count()))
                });
                if let Some((goal, pos)) = goal {
                    match self.call_method_with_values(
                        Value::package(Symbol::intern(package_name)),
                        "FAILGOAL",
                        vec![Value::str(goal.clone())],
                    ) {
                        Ok(_) => {
                            self.env.insert("/".to_string(), Value::NIL);
                            return Ok(self.make_goal_failure_value(&goal, pos));
                        }
                        Err(err) if err.is_method_not_found() => {}
                        Err(err) => return Err(err),
                    }
                    self.env.insert("/".to_string(), Value::NIL);
                    return Ok(self.make_goal_failure_value(&goal, pos));
                }
                self.env.insert("/".to_string(), Value::NIL);
                return Ok(self.parse_failure_for_pattern(&text, Some(&pattern)));
            };
            self.execute_regex_code_blocks(&captures.code_blocks);
            if captures.from != 0 {
                self.env.insert("/".to_string(), Value::NIL);
                return Ok(self.parse_failure_for_pattern(&text, Some(&pattern)));
            }
            if (method == "parse" || method == "parsefile") && captures.to != text.chars().count() {
                self.env.insert("/".to_string(), Value::NIL);
                return Ok(self.make_parse_failure_value(&text, captures.to));
            }
            for (i, v) in captures.positional.iter().enumerate() {
                self.env.insert(i.to_string(), Value::str(v.clone()));
            }
            let alias_map = std::mem::take(&mut captures.capture_alias_map);
            let match_obj = Value::make_match_object_full_q(
                captures.matched,
                captures.from as i64,
                captures.to as i64,
                &captures.positional,
                &captures.named,
                &captures.named_subcaps,
                &captures.positional_subcaps,
                &captures.positional_quantified,
                &captures.positional_nil,
                Some(&text),
                &captures.named_quantified,
            );
            let match_obj = if let ValueView::Instance {
                class_name,
                attributes,
                ..
            } = match_obj.view()
            {
                let attrs = attributes.as_ref().clone();
                if let Some(ast) = self.env.get("made").cloned() {
                    attrs.insert("ast".to_string(), ast);
                }
                if let Some(ref act) = actions_obj {
                    attrs.insert("actions".to_string(), act.clone());
                }
                if !alias_map.is_empty() {
                    let alias_hash: HashMap<String, Value> = alias_map
                        .iter()
                        .map(|(k, v)| (k.clone(), Value::str(v.clone())))
                        .collect();
                    attrs.insert("capture_alias_map".to_string(), Value::hash(alias_hash));
                }
                Value::make_instance(class_name, (attrs).to_map())
            } else {
                match_obj
            };
            // Set named capture env vars from match object
            if let ValueView::Instance { attributes, .. } = match_obj.view()
                && let Some(ValueView::Hash(named_hash)) =
                    attributes.as_map().get("named").map(Value::view)
            {
                for (k, v) in named_hash.iter() {
                    self.env.insert(format!("<{}>", k), v.clone());
                }
            }
            self.env.insert("/".to_string(), match_obj.clone());

            // Invoke action methods if :actions was provided
            let match_obj = if let Some(ref mut actions) = actions_obj {
                // Action methods run with `self` bound to the actions object.
                // Restore the caller's `self` afterwards so a nested sub in the
                // caller (which resolves `self` from env via `GetSelfOrNoSelf`)
                // doesn't see the actions object leaked in. (Same hazard the
                // stringify path fixes for `try_compiled_method_or_interpret`.)
                let saved_self = self.env.get("self").cloned();
                let result = self.invoke_grammar_actions(match_obj, actions, &start_rule);
                match saved_self {
                    Some(s) => {
                        self.env.insert("self".to_string(), s);
                    }
                    None => {
                        self.env.remove("self");
                    }
                }
                let result = result?;
                // Update the actions attribute on the final Match to reflect
                // any mutations that occurred during action method dispatch.
                if let ValueView::Instance {
                    class_name,
                    attributes,
                    id,
                    ..
                } = result.view()
                {
                    let mut attrs = attributes.to_map();
                    attrs.insert("actions".to_string(), actions.clone());
                    Value::write_back_sharing(&attributes, class_name, attrs, id)
                } else {
                    result
                }
            } else {
                match_obj
            };

            self.env.insert("/".to_string(), match_obj.clone());
            Ok(match_obj)
        })();

        // Restore any dynamic vars the grammar's rules established for this parse.
        for (key, prev) in saved_grammar_dynvars {
            match prev {
                Some(v) => {
                    self.env.insert(key, v);
                }
                None => {
                    self.env.remove(&key);
                }
            }
        }
        self.set_current_package(saved_package);
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
        self.current_grammar_actions = saved_grammar_actions;
        // In Raku 6.c/6.d, Grammar.parse/parsefile returns Nil on failure.
        // In 6.e+, it returns a Failure object. The decision is keyed on the
        // grammar's *declaration* revision (captured as type metadata), not the
        // globally-current language version: the latter is a mutable thread-local
        // that gets reset to the default whenever the parser re-enters (e.g. while
        // compiling a regex during the parse), so reading it here is unreliable.
        let grammar_is_6e = match self
            .type_metadata
            .get(package_name)
            .and_then(|meta| meta.get("language-revision"))
            .map(Value::view)
        {
            Some(ValueView::Str(rev)) => rev.as_str() >= "e",
            _ => crate::parser::current_language_version().starts_with("6.e"),
        };
        if is_full_parse
            && !grammar_is_6e
            && let Ok(v) = &result
            && let ValueView::Instance { class_name, .. } = v.view()
            && class_name == "Failure"
        {
            return Ok(Value::NIL);
        }
        result
    }

    /// Extract `action_name` from a Match object (set for aliased captures).
    pub(crate) fn get_action_name(match_obj: &Value) -> Option<String> {
        if let ValueView::Instance { attributes, .. } = match_obj.view()
            && let Some(ValueView::Str(action_name)) =
                attributes.as_map().get("action_name").map(Value::view)
        {
            return Some(action_name.to_string());
        }
        None
    }

    /// Dispatch the silent-action captures reachable from a match's attribute
    /// map: this level's own `silent_caps` (hidden `<.foo>` subrule matches that
    /// carry nested captures), then — descending through positional `( )` groups
    /// — each group's `silent_caps`. Only `silent_caps` are fired, never a group's
    /// named children: those are already dispatched by the named-children walk in
    /// `invoke_grammar_actions`, so re-firing them would double-dispatch (see
    /// t/grammar-reduce-time-dynvar.t). Captures are dispatched in source order.
    fn dispatch_silent_action_caps(
        &mut self,
        attrs: &crate::value::AttrReadGuard<'_>,
        actions: &mut Value,
    ) -> Result<(), RuntimeError> {
        if let Some(ValueView::Array(silent_arr, _)) = attrs.get("silent_caps").map(Value::view) {
            let mut items: Vec<Value> = silent_arr.iter().cloned().collect();
            items.sort_by_key(|m| {
                if let ValueView::Instance { attributes, .. } = m.view()
                    && let Some(ValueView::Int(from)) =
                        attributes.as_map().get("from").map(Value::view)
                {
                    from
                } else {
                    0
                }
            });
            for item in items {
                let dispatch_name = Self::get_action_name(&item).unwrap_or_default();
                self.invoke_grammar_actions(item, actions, &dispatch_name)?;
            }
        }
        let positionals: Vec<Value> = match attrs.get("list").map(Value::view) {
            Some(ValueView::Array(pos_arr, _)) => pos_arr.iter().cloned().collect(),
            _ => Vec::new(),
        };
        for p in &positionals {
            if let ValueView::Instance { attributes, .. } = p.view() {
                self.dispatch_silent_action_caps(&attributes.as_map(), actions)?;
            }
        }
        Ok(())
    }

    /// Walk the match tree bottom-up and invoke action methods on the actions object.
    pub(crate) fn invoke_grammar_actions(
        &mut self,
        match_obj: Value,
        actions: &mut Value,
        rule_name: &str,
    ) -> Result<Value, RuntimeError> {
        let (class_name, attributes) = if let ValueView::Instance {
            class_name,
            attributes,
            ..
        } = match_obj.view()
        {
            (class_name, attributes.as_ref().clone())
        } else {
            return Ok(match_obj);
        };

        // First, recursively process child named captures (bottom-up order)
        let updated_attrs = attributes.clone();
        if let Some(ValueView::Hash(named_hash)) = attributes.as_map().get("named").map(Value::view)
        {
            let mut updated_named = named_hash.as_ref().clone();
            let mut children: Vec<(String, Value)> = named_hash
                .iter()
                .map(|(k, v)| (k.clone(), v.clone()))
                .collect();
            children.sort_by_key(|(_, v)| {
                if let ValueView::Instance { attributes, .. } = v.view()
                    && let Some(ValueView::Int(from)) =
                        attributes.as_map().get("from").map(Value::view)
                {
                    return from;
                }
                0
            });
            for (child_name, child_match) in children {
                if let ValueView::Array(items, meta) = child_match.view() {
                    let mut updated_items = Vec::with_capacity(items.len());
                    for item in items.as_ref() {
                        let dispatch_name =
                            Self::get_action_name(item).unwrap_or_else(|| child_name.clone());
                        let updated_item =
                            self.invoke_grammar_actions(item.clone(), actions, &dispatch_name)?;
                        updated_items.push(updated_item);
                    }
                    updated_named.insert(
                        child_name,
                        Value::array_with_kind(
                            crate::gc::Gc::new(crate::value::ArrayData::new(updated_items)),
                            meta,
                        ),
                    );
                } else {
                    let dispatch_name =
                        Self::get_action_name(&child_match).unwrap_or_else(|| child_name.clone());
                    let updated_child =
                        self.invoke_grammar_actions(child_match, actions, &dispatch_name)?;
                    updated_named.insert(child_name, updated_child);
                }
            }
            updated_attrs.insert("named".to_string(), Value::hash(updated_named));
        }

        // Dispatch actions for silent-action captures: hidden `<.foo>` subrule
        // matches that carry nested captures. The subrule is absent from `.hash`,
        // but Rakudo fires its action method (and its descendants') at reduce time
        // regardless of capture — so recurse into each (bottom-up, in source order)
        // to dispatch them. Each subcap's `action_name` names the method to call.
        // This also descends through positional `( )` groups, since a silent
        // subrule matched inside a group has its marker stored on the GROUP's
        // match. Only `silent_caps` are dispatched — never the groups' named
        // children (the named-children walk above already handles those; firing
        // them again double-dispatches, see t/grammar-reduce-time-dynvar.t).
        // Results are not stored back: silent captures are never read by user code.
        self.dispatch_silent_action_caps(&attributes.as_map(), actions)?;

        // Rebuild match_obj with updated children
        let match_obj = Value::make_instance(class_name, (updated_attrs.clone()).to_map());

        // Set $/ to this match and try calling actions.{rule_name}(match)
        self.env.insert("/".to_string(), match_obj.clone());
        self.env.remove("made");
        self.action_made = None;
        // Save old named capture env vars so parent captures don't leak into child actions
        let saved_named_captures: Vec<(Symbol, Value)> = self
            .env
            .iter()
            .filter(|(k, _)| k.starts_with("<") && k.ends_with(">"))
            .map(|(k, v)| (*k, v.clone()))
            .collect();
        for (k, _) in &saved_named_captures {
            self.env.remove_sym(*k);
        }
        // Set named capture env vars (<a>, <b>, etc.) so $<a> works inside action methods
        if let Some(ValueView::Hash(named_hash)) =
            updated_attrs.as_map().get("named").map(Value::view)
        {
            for (k, v) in named_hash.iter() {
                self.env.insert(format!("<{}>", k), v.clone());
            }
        }
        // Set positional capture env vars ($0, $1, ...) so they work inside action methods.
        // First, save and clear any existing positional captures from parent/sibling action
        // calls so they don't leak into this action method's scope.
        let mut saved_positional: Vec<(usize, Option<Value>)> = Vec::new();
        for i in 0..10 {
            let key = i.to_string();
            saved_positional.push((i, self.env.remove(&key)));
        }
        if let Some(ValueView::Array(pos_arr, _)) =
            updated_attrs.as_map().get("list").map(Value::view)
        {
            for (i, v) in pos_arr.iter().enumerate() {
                self.env.insert(i.to_string(), v.clone());
            }
        }
        // Also set $_ to the match (for `.make:` syntax)
        let saved_topic = self.env.get("_").cloned();
        self.env.insert("_".to_string(), match_obj.clone());

        // For protoregex :sym<> variants, try dispatching to the specific
        // action method (e.g., alt:sym<baz>) first.
        let sym_method_name = if let Some(ValueView::Str(sym_val)) =
            updated_attrs.as_map().get("sym_variant").map(Value::view)
        {
            // Use «» delimiters when the sym value contains '<' or '>'
            // to match method names stored with French-quote delimiters
            if sym_val.contains('<') || sym_val.contains('>') {
                Some(format!("{rule_name}:sym\u{ab}{}\u{bb}", *sym_val))
            } else {
                Some(format!("{rule_name}:sym<{}>", *sym_val))
            }
        } else {
            None
        };
        let method_result = if let Some(ref sym_name) = sym_method_name {
            let result =
                self.call_method_with_values(actions.clone(), sym_name, vec![match_obj.clone()]);
            match result {
                Err(e) if e.is_method_not_found() => {
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
        if let ValueView::Instance {
            class_name: act_cn,
            id: act_id,
            ..
        } = actions.view()
        {
            for v in self.env.values() {
                if let ValueView::Instance {
                    class_name: cn,
                    id,
                    attributes,
                    ..
                } = v.view()
                    && cn == act_cn
                    && id == act_id
                {
                    *actions = Value::instance_parts(cn, attributes.clone(), id);
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
        // Restore named capture env vars from parent scope
        {
            let current_angle_keys: Vec<Symbol> = self
                .env
                .keys()
                .filter(|k| k.starts_with("<") && k.ends_with(">"))
                .copied()
                .collect();
            for k in current_angle_keys {
                self.env.remove_sym(k);
            }
            for (k, v) in saved_named_captures {
                self.env.insert_sym(k, v);
            }
        }

        // Restore positional capture env vars from parent scope
        for i in 0..10 {
            self.env.remove(&i.to_string());
        }
        for (i, val) in saved_positional {
            if let Some(v) = val {
                self.env.insert(i.to_string(), v);
            }
        }

        match method_result {
            Ok(_) => {}
            Err(e) if e.is_method_not_found() => {
                // No action method for this rule — silently skip
            }
            Err(e) => return Err(e),
        }

        // If make() was called (via action_made which persists across env restore),
        // update .ast on match
        let final_obj = if let Some(ast) = self.action_made.take() {
            let attrs = updated_attrs;
            attrs.insert("ast".to_string(), ast);
            // Preserve actions attribute if present
            if let Some(act_val) = attributes.as_map().get("actions") {
                attrs.insert("actions".to_string(), act_val.clone());
            }
            Value::make_instance(class_name, (attrs).to_map())
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

    /// How far a failed `.parse` got, for the failure message: the longest prefix of
    /// the input that the pattern matches in full.
    ///
    /// This re-matches the pattern once per prefix, so it must not execute the
    /// grammar's code atoms — otherwise a `<?{ … }>` runs once per prefix, i.e.
    /// O(input length) times, purely to build a diagnostic (ADR-0009). `advent2013-day18`
    /// showed this exactly: its card assertion ran 15 times on a 14-char input and 31
    /// times on a 31-char one. Under `CODE_ATOMS_INERT` the probe measures the
    /// pattern's declarative skeleton instead, executing nothing.
    ///
    /// (The search is still O(input length) full matches on the failure path, and the
    /// common case — no prefix matches — is the worst case. Pre-existing; not addressed
    /// here.)
    pub(super) fn longest_complete_prefix_end(&mut self, pattern: &str, text: &str) -> usize {
        let saved = super::regex::regex_helpers::CODE_ATOMS_INERT.replace(true);
        let chars: Vec<char> = text.chars().collect();
        let mut best = 0;
        for end in (0..=chars.len()).rev() {
            let prefix: String = chars[..end].iter().collect();
            if let Some(captures) = self.regex_match_with_captures(pattern, &prefix)
                && captures.from == 0
                && captures.to == end
            {
                best = end;
                break;
            }
        }
        super::regex::regex_helpers::CODE_ATOMS_INERT.set(saved);
        best
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
        ex_attrs.insert("pos".to_string(), Value::int(pos as i64));
        ex_attrs.insert("line".to_string(), Value::int(line));
        ex_attrs.insert("pre".to_string(), Value::str(pre));
        ex_attrs.insert("post".to_string(), Value::str(post));
        ex_attrs.insert("highexpect".to_string(), Value::array(Vec::new()));
        let exception = Value::make_instance(Symbol::intern("X::Syntax::Confused"), ex_attrs);

        let mut failure_attrs = HashMap::new();
        failure_attrs.insert("exception".to_string(), exception);
        // TODO: Grammar.parse/subparse should return a failed Match, not a Failure.
        // Mark as handled so that stringifying doesn't throw prematurely.
        failure_attrs.insert("handled".to_string(), Value::TRUE);
        Value::make_instance(Symbol::intern("Failure"), failure_attrs)
    }
}
