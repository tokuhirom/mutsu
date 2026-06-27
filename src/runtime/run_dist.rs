use super::*;

impl Interpreter {
    /// Build the %?RESOURCES hash for the current package/distribution context.
    /// Looks up the distribution for the current package (or falls back to current_distribution)
    /// and returns a Hash mapping resource names to their absolute paths on disk.
    pub(crate) fn build_resources_for_package(&self) -> Value {
        use std::collections::HashMap;

        // `%?RESOURCES` is lexically tied to the compilation unit whose source
        // contains the token, i.e. the module of the *currently executing*
        // routine — not whichever module is currently being loaded. So the
        // innermost routine-stack frame whose package owns a distribution wins.
        // This matters when one module's load-time code (e.g. a `constant`
        // initializer) calls into another module whose method reads
        // `%?RESOURCES` — MIME::Types.new being the canonical case: it runs
        // while the *outer* module is still loading, so `current_distribution`
        // would otherwise (incorrectly) point at the outer module's dist.
        //
        // Priority 1: the executing routine's defining package (innermost first).
        for frame in self.routine_stack.iter().rev() {
            if let Some(dist) = self.package_distributions.get(&frame.package) {
                return Self::build_resources_from_dist(dist);
            }
        }
        // Priority 2: current_distribution (set during module loading) — the
        // right answer for top-level module code that is not inside any routine.
        if let Some(dist) = &self.current_distribution {
            return Self::build_resources_from_dist(dist);
        }
        // Priority 3: Look up by current package
        if let Some(dist) = self.package_distributions.get(&self.current_package()) {
            return Self::build_resources_from_dist(dist);
        }
        Value::Hash(Value::hash_arc(HashMap::new()))
    }

    fn build_resources_from_dist(dist: &Value) -> Value {
        use std::collections::HashMap;

        let meta = match dist {
            Value::Instance { attributes, .. } => {
                // Try "meta" first (Distribution::Installation from detect_inst_distribution),
                // then fall back to "$!meta" (plain Distribution from detect_distribution).
                let map = attributes.as_map();
                map.get("meta")
                    .or_else(|| map.get("$!meta"))
                    .cloned()
                    .unwrap_or(Value::Nil)
            }
            _ => Value::Nil,
        };
        let prefix = match &meta {
            Value::Hash(map) => map
                .get("prefix")
                .map(|v| v.to_string_value())
                .unwrap_or_default(),
            _ => String::new(),
        };
        // If the dist has a "files" hash (installation repo), use it to resolve resource paths.
        // The files hash maps "resources/config.txt" -> "/inst-prefix/resources/HASH.txt"
        let files_val = match &meta {
            Value::Hash(map) => map.get("files").cloned(),
            _ => None,
        };
        let resources_val = match &meta {
            Value::Hash(map) => map.get("resources").cloned().unwrap_or(Value::Nil),
            _ => Value::Nil,
        };
        let mut result: HashMap<String, Value> = HashMap::new();
        match &resources_val {
            Value::Array(arr, _) => {
                for item in arr.iter() {
                    let key = item.to_string_value();
                    // Check files hash first (installation repo)
                    let files_key = format!("resources/{key}");
                    if let Some(ref fv) = files_val
                        && let Some(path_val) = fv.hash_get_str(&files_key)
                    {
                        result.insert(key, path_val);
                        continue;
                    }
                    let actual_path = if key.starts_with("libraries/") {
                        let lib_name = key.strip_prefix("libraries/").unwrap_or(&key);
                        let platform_name = if cfg!(target_os = "macos") {
                            format!("lib{lib_name}.dylib")
                        } else if cfg!(target_os = "windows") {
                            format!("{lib_name}.dll")
                        } else {
                            format!("lib{lib_name}.so")
                        };
                        if prefix.is_empty() {
                            format!("resources/libraries/{platform_name}")
                        } else {
                            format!("{prefix}/resources/libraries/{platform_name}")
                        }
                    } else if prefix.is_empty() {
                        format!("resources/{key}")
                    } else {
                        format!("{prefix}/resources/{key}")
                    };
                    result.insert(key, Value::str(actual_path));
                }
            }
            Value::Hash(map) => {
                for (k, v) in map.iter() {
                    result.insert(k.clone(), v.clone());
                }
            }
            _ => {}
        }
        Value::Hash(Value::hash_arc(result))
    }

    /// Detect a distribution (META6.json) for the given module source path.
    pub(super) fn detect_distribution(source_path: &Path) -> Option<Value> {
        let mut dir = source_path.parent()?;
        for _ in 0..4 {
            let meta_path = dir.join("META6.json");
            if meta_path.exists()
                && let Ok(content) = fs::read_to_string(&meta_path)
            {
                // Pass the distribution prefix (dir containing META6.json) so
                // %?RESOURCES can resolve absolute paths.
                let dist_prefix = dir.to_string_lossy().to_string();
                return Self::build_distribution_from_meta(&content, &dist_prefix);
            }
            dir = dir.parent()?;
        }
        // No META6.json found; build a minimal auto-generated distribution
        let lib_dir = source_path.parent()?;
        Some(Self::build_distribution_from_lib_dir(lib_dir))
    }

    fn build_distribution_from_meta(json_content: &str, dist_prefix: &str) -> Option<Value> {
        let mut meta_hash = Self::parse_meta6_json(json_content)?;
        // Store the distribution prefix in meta so %?RESOURCES can build absolute paths.
        meta_hash.insert("prefix".to_string(), Value::str(dist_prefix.to_string()));
        let mut attrs = HashMap::new();
        attrs.insert(
            "$!meta".to_string(),
            Value::Hash(Value::hash_arc(meta_hash)),
        );
        Some(Value::make_instance_without_destroy(
            crate::symbol::Symbol::intern("Distribution"),
            attrs,
        ))
    }

    fn build_distribution_from_lib_dir(lib_dir: &Path) -> Value {
        let mut meta = HashMap::new();
        let lib_path = lib_dir.to_string_lossy().to_string();
        meta.insert("name".to_string(), Value::str(lib_path));
        meta.insert("ver".to_string(), Value::str("*".to_string()));
        meta.insert("api".to_string(), Value::str("*".to_string()));
        meta.insert("auth".to_string(), Value::str(String::new()));
        let provides = Self::scan_lib_provides(lib_dir);
        meta.insert("provides".to_string(), provides);
        let resources_dir = lib_dir.parent().map(|p| p.join("resources"));
        let resources = if let Some(ref rd) = resources_dir
            && rd.exists()
        {
            Self::scan_resources(rd)
        } else {
            Value::Hash(Value::hash_arc(HashMap::new()))
        };
        meta.insert("resources".to_string(), resources);
        let mut attrs = HashMap::new();
        attrs.insert("$!meta".to_string(), Value::Hash(Value::hash_arc(meta)));
        Value::make_instance_without_destroy(crate::symbol::Symbol::intern("Distribution"), attrs)
    }

    fn scan_lib_provides(lib_dir: &Path) -> Value {
        let mut provides = HashMap::new();
        let extensions = [".rakumod", ".pm6", ".pm"];
        if let Ok(entries) = std::fs::read_dir(lib_dir) {
            for entry in entries.flatten() {
                let path = entry.path();
                if path.is_file() {
                    for ext in &extensions {
                        if let Some(name) = path.file_name().and_then(|n| n.to_str())
                            && let Some(stem) = name.strip_suffix(ext)
                        {
                            let module_name = stem.replace('/', "::");
                            let relative = format!("lib/{}", name);
                            provides.insert(module_name, Value::str(relative));
                        }
                    }
                }
            }
        }
        Value::Hash(Value::hash_arc(provides))
    }

    fn scan_resources(resources_dir: &Path) -> Value {
        let mut files = HashMap::new();
        if let Ok(entries) = std::fs::read_dir(resources_dir) {
            for entry in entries.flatten() {
                let path = entry.path();
                if path.is_file()
                    && let Some(name) = path.file_name().and_then(|n| n.to_str())
                {
                    let key = format!("resources/{}", name);
                    files.insert(key.clone(), Value::str(key));
                }
            }
        }
        Value::Hash(Value::hash_arc(files))
    }

    fn parse_meta6_json(content: &str) -> Option<HashMap<String, Value>> {
        let json: serde_json::Value = serde_json::from_str(content).ok()?;
        let obj = json.as_object()?;
        let mut meta = HashMap::new();
        for (key, val) in obj {
            meta.insert(key.clone(), Self::json_to_value(val));
        }
        Some(meta)
    }

    fn json_to_value(val: &serde_json::Value) -> Value {
        match val {
            serde_json::Value::Null => Value::Nil,
            serde_json::Value::Bool(b) => Value::Bool(*b),
            serde_json::Value::Number(n) => {
                if let Some(i) = n.as_i64() {
                    Value::Int(i)
                } else if let Some(f) = n.as_f64() {
                    Value::Num(f)
                } else {
                    Value::str(n.to_string())
                }
            }
            serde_json::Value::String(s) => Value::str(s.clone()),
            serde_json::Value::Array(arr) => {
                Value::array(arr.iter().map(Self::json_to_value).collect())
            }
            serde_json::Value::Object(obj) => {
                let mut map = HashMap::new();
                for (k, v) in obj {
                    map.insert(k.clone(), Self::json_to_value(v));
                }
                Value::Hash(Value::hash_arc(map))
            }
        }
    }

    /// Check for unresolved package/class stubs at program end.
    /// Throws X::Package::Stubbed if any stubs remain.
    pub(crate) fn check_unresolved_stubs(&self) -> Result<(), RuntimeError> {
        let mut unresolved: Vec<String> = Vec::new();
        for name in &self.registry().class_stubs {
            unresolved.push(name.clone());
        }
        for name in &self.registry().package_stubs {
            unresolved.push(name.clone());
        }
        if unresolved.is_empty() {
            return Ok(());
        }
        unresolved.sort();
        let names_list = unresolved
            .iter()
            .map(|n| format!("    {}", n))
            .collect::<Vec<_>>()
            .join("\n");
        let message = format!(
            "The following packages were stubbed but not defined:\n{}",
            names_list
        );
        let mut attrs = std::collections::HashMap::new();
        attrs.insert(
            "packages".to_string(),
            Value::array(unresolved.iter().map(|n| Value::str(n.clone())).collect()),
        );
        attrs.insert("message".to_string(), Value::str(message.clone()));
        let ex = Value::make_instance(crate::symbol::Symbol::intern("X::Package::Stubbed"), attrs);
        let mut err = RuntimeError::new(&message);
        err.exception = Some(Box::new(ex));
        Err(err)
    }
}

#[cfg(test)]
mod tests {
    use super::Interpreter;

    #[test]
    fn preprocess_count_skip_skips_only_next_test() {
        let src = "#?rakudo 1 skip 'reason'\n{\n    is EVAL('$bar'), Any, 'x'\n    is 42, 42, 'still runs';\n}\nsay 42;\n";
        let out = Interpreter::preprocess_roast_directives(src);
        assert!(out.contains("skip 'reason', 1;"));
        assert!(!out.contains("is EVAL('$bar'), Any, 'x'"));
        assert!(out.contains("is 42, 42, 'still runs';"));
        assert!(out.contains("say 42;"));
    }

    #[test]
    fn preprocess_block_skip_consumes_entire_block() {
        let src = "#?rakudo skip 'reason'\n{\n    is EVAL('$bar'), Any, 'x'\n}\nsay 42;\n";
        let out = Interpreter::preprocess_roast_directives(src);
        assert!(out.contains("skip 'reason', 1;"));
        assert!(!out.contains("is EVAL('$bar'), Any, 'x'"));
        assert!(out.contains("say 42;"));
    }

    #[test]
    fn preprocess_block_skip_counts_tap_ok_as_test_assertion() {
        let src = "#?rakudo skip 'reason'\n{\n    tap-ok $s, [1], 'tap';\n    ok True, 'ok';\n}\n";
        let out = Interpreter::preprocess_roast_directives(src);
        assert_eq!(out.matches("skip 'reason', 1;").count(), 2);
        assert!(!out.contains("tap-ok $s, [1], 'tap';"));
        assert!(!out.contains("ok True, 'ok';"));
    }

    #[test]
    fn preprocess_rakudo_todo_marks_backend_specific_todo() {
        let src = "#?rakudo todo 'NYI'\nok True, 'still runs';\n#?rakudo 2 todo 'later'\nis 42, 42, 'also runs';\n";
        let out = Interpreter::preprocess_roast_directives(src);
        assert!(out.contains("todo '__mutsu_backend_todo__:NYI', 1;"));
        assert!(out.contains("todo '__mutsu_backend_todo__:later', 2;"));
    }

    #[test]
    fn eval_q_bracket_statement_list_runs_declaration_then_assertion() {
        let mut interp = Interpreter::new();
        interp.set_immediate_stdout(false);
        let result = interp.run(
            "use Test; plan 1; EVAL q[[my $sub = sub () { 42 }; is [$sub()], [42], 'q-bracket eval';]];",
        );
        assert!(result.is_ok(), "run failed: {:?}", result.err());
        assert!(
            interp.output_sink().output.contains("1..1"),
            "output: {}",
            interp.output_sink().output
        );
        assert!(
            interp
                .output_sink()
                .output
                .contains("ok 1 - q-bracket eval"),
            "output: {}",
            interp.output_sink().output
        );
    }

    #[test]
    fn rakudo_todo_passes_without_todo_annotation() {
        let mut interp = Interpreter::new();
        interp.set_immediate_stdout(false);
        // run() only applies fudge when MUTSU_FUDGE is set; preprocess explicitly
        // so the test exercises the fudge pipeline regardless of the env gate.
        let src = Interpreter::preprocess_roast_directives(
            "use Test; plan 1;\n#?rakudo todo 'NYI'\nok True, 'pass';",
        );
        let result = interp.run(&src);
        assert!(result.is_ok(), "run failed: {:?}", result.err());
        assert_eq!(interp.output_sink().output, "1..1\nok 1 - pass\n");
    }

    #[test]
    fn rakudo_todo_failures_keep_todo_annotation() {
        let mut interp = Interpreter::new();
        interp.set_immediate_stdout(false);
        // run() only applies fudge when MUTSU_FUDGE is set; preprocess explicitly
        // so the test exercises the fudge pipeline regardless of the env gate.
        let src = Interpreter::preprocess_roast_directives(
            "use Test; plan 1;\n#?rakudo todo 'NYI'\nok False, 'fail';",
        );
        let result = interp.run(&src);
        assert!(result.is_ok(), "run failed: {:?}", result.err());
        assert!(
            interp
                .output_sink()
                .output
                .starts_with("1..1\nnot ok 1 - fail # TODO NYI\n")
        );
    }

    // END phasers must run even after die() or exit().
    // Spec: raku-doc/doc/Language/phasers.rakudoc (END runs "at runtime, ALAP, only ever runs once")
    // Roast: roast/integration/error-reporting.t line 99 ("END phasers are run after die()")
    // Roast: roast/S04-phasers/end.t line 53 ("exit does not prevent running of END blocks")

    #[test]
    fn end_phaser_runs_after_die() {
        let mut interp = Interpreter::new();
        interp.set_immediate_stdout(false);
        let result = interp.run("END { say 'end-ran' }; die 'boom';");
        assert!(result.is_err(), "die should propagate as error");
        assert_eq!(interp.output_sink().output, "end-ran\n");
    }

    #[test]
    fn end_phaser_runs_after_exit() {
        let mut interp = Interpreter::new();
        interp.set_immediate_stdout(false);
        let result = interp.run("END { say 'end-ran' }; exit;");
        assert!(result.is_ok());
        assert_eq!(interp.output_sink().output, "end-ran\n");
    }

    #[test]
    fn end_phaser_runs_after_exit_with_code() {
        let mut interp = Interpreter::new();
        interp.set_immediate_stdout(false);
        let result = interp.run("END { say 'end-ran' }; exit(5);");
        assert!(result.is_ok());
        assert_eq!(interp.exit_code(), 5);
        assert_eq!(interp.output_sink().output, "end-ran\n");
    }

    #[test]
    fn end_phaser_runs_in_reverse_order() {
        let mut interp = Interpreter::new();
        interp.set_immediate_stdout(false);
        let result = interp.run("END { print 'A' }; END { print 'B' }; END { print 'C' };");
        assert!(result.is_ok());
        assert_eq!(interp.output_sink().output, "CBA");
    }

    #[test]
    fn end_phaser_reverse_order_with_die() {
        let mut interp = Interpreter::new();
        interp.set_immediate_stdout(false);
        let result = interp.run("END { print 'A' }; END { print 'B' }; die 'x';");
        assert!(result.is_err());
        assert_eq!(interp.output_sink().output, "BA");
    }

    #[test]
    fn end_phaser_runs_on_normal_completion() {
        let mut interp = Interpreter::new();
        interp.set_immediate_stdout(false);
        let result = interp.run("say 'hello'; END { say 'end' };");
        assert!(result.is_ok());
        assert_eq!(interp.output_sink().output, "hello\nend\n");
    }

    #[test]
    fn die_preserves_exit_code_with_end_phaser() {
        let mut interp = Interpreter::new();
        interp.set_immediate_stdout(false);
        let result = interp.run("END { say 'end' }; die 'boom';");
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert!(
            err.message.contains("boom"),
            "error message should contain 'boom'"
        );
    }
}
