use super::*;
use std::fs;
use std::sync::atomic::{AtomicU64, Ordering};

static TEMP_COUNTER: AtomicU64 = AtomicU64::new(0);

impl Interpreter {
    /// Dispatch Test::Util module functions. Returns `Ok(Some(value))` if the
    /// name matched a Test::Util function, `Ok(None)` if it did not.
    pub(crate) fn call_test_util_function(
        &mut self,
        name: &str,
        args: &[Value],
    ) -> Result<Option<Value>, RuntimeError> {
        match name {
            "make-temp-file" | "make-temp-path" => self.test_util_make_temp_file(args).map(Some),
            "make-temp-dir" => self.test_util_make_temp_dir(args).map(Some),
            "is-eqv" => self.test_util_is_eqv(args).map(Some),
            "group-of" => self.test_util_group_of(args).map(Some),
            _ => Ok(None),
        }
    }

    /// Generate a unique temp path under the system temp directory.
    fn make_temp_path_buf(&self) -> std::path::PathBuf {
        let tmp_dir = std::env::temp_dir();
        let counter = TEMP_COUNTER.fetch_add(1, Ordering::Relaxed);
        let pid = std::process::id();
        let name = format!("mutsu_test_{}_{}", pid, counter);
        tmp_dir.join(name)
    }

    /// Build an IO::Path Value from a PathBuf.
    fn io_path_value(path: &std::path::Path) -> Value {
        Value::Instance {
            class_name: "IO::Path".to_string(),
            attributes: {
                let mut attrs = HashMap::new();
                attrs.insert(
                    "path".to_string(),
                    Value::Str(path.to_string_lossy().to_string()),
                );
                attrs
            },
            id: next_instance_id(),
        }
    }

    /// `make-temp-file(:$content, Int :$chmod --> IO::Path:D)`
    ///
    /// Creates a temp file, optionally writing content and setting permissions.
    fn test_util_make_temp_file(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let path = self.make_temp_path_buf();
        let content: Option<Value> = Self::named_value(args, "content");
        let chmod_val: Option<Value> = Self::named_value(args, "chmod");

        // If chmod is set but content is not, spurt an empty string
        if chmod_val.is_some() && content.is_none() {
            fs::write(&path, "")
                .map_err(|e| RuntimeError::new(format!("make-temp-file: cannot write: {}", e)))?;
        } else if let Some(ref c) = content {
            let text = c.to_string_value();
            fs::write(&path, &text)
                .map_err(|e| RuntimeError::new(format!("make-temp-file: cannot write: {}", e)))?;
        }

        #[cfg(unix)]
        if let Some(ref mode) = chmod_val {
            use std::os::unix::fs::PermissionsExt;
            let mode_int = match mode {
                Value::Int(i) => *i as u32,
                other => other.to_string_value().parse::<u32>().unwrap_or(0o644),
            };
            fs::set_permissions(&path, fs::Permissions::from_mode(mode_int))
                .map_err(|e| RuntimeError::new(format!("make-temp-file: cannot chmod: {}", e)))?;
        }
        let _ = &chmod_val; // suppress unused warning on non-unix

        // Register for cleanup at END
        self.temp_files.push(path.clone());

        Ok(Self::io_path_value(&path))
    }

    /// `make-temp-dir(Int $chmod? --> IO::Path:D)`
    ///
    /// Creates a temp directory, optionally setting permissions.
    fn test_util_make_temp_dir(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let path = self.make_temp_path_buf();
        fs::create_dir_all(&path)
            .map_err(|e| RuntimeError::new(format!("make-temp-dir: cannot mkdir: {}", e)))?;

        let chmod_val: Option<Value> = Self::positional_value(args, 0).cloned();
        #[cfg(unix)]
        if let Some(ref mode) = chmod_val {
            use std::os::unix::fs::PermissionsExt;
            let mode_int = match mode {
                Value::Int(i) => *i as u32,
                other => other.to_string_value().parse::<u32>().unwrap_or(0o755),
            };
            fs::set_permissions(&path, fs::Permissions::from_mode(mode_int))
                .map_err(|e| RuntimeError::new(format!("make-temp-dir: cannot chmod: {}", e)))?;
        }
        let _ = &chmod_val; // suppress unused warning on non-unix

        // Register for cleanup at END
        self.temp_dirs.push(path.clone());

        Ok(Self::io_path_value(&path))
    }

    /// `is-eqv(Mu $got, Mu $expected, Str:D $desc)`
    ///
    /// Tests equivalence using the `eqv` operator.
    fn test_util_is_eqv(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let got = Self::positional_value(args, 0)
            .cloned()
            .unwrap_or(Value::Nil);
        let expected = Self::positional_value(args, 1)
            .cloned()
            .unwrap_or(Value::Nil);
        let desc = Self::positional_string(args, 2);

        let ok = got.eqv(&expected);

        if !ok {
            self.test_ok(false, &desc, false)?;
            let diag = format!(
                "# expected: {}\n#      got: {}",
                expected.to_string_value(),
                got.to_string_value()
            );
            self.output.push_str(&diag);
            self.output.push('\n');
        } else {
            self.test_ok(true, &desc, false)?;
        }

        Ok(Value::Bool(ok))
    }

    /// `group-of(Pair)` — syntactic sugar for subtest with plan.
    ///
    /// Usage: `group-of 3 => 'description' => { tests }`
    fn test_util_group_of(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        // Expects a nested Pair: Int => (Str => Block)
        let pair = Self::positional_value(args, 0)
            .cloned()
            .unwrap_or(Value::Nil);

        if let Value::Pair(plan_str, inner) = pair {
            let plan: i64 = plan_str.parse().unwrap_or(0);
            if let Value::Pair(desc, block) = *inner {
                let ctx = self.begin_subtest();
                self.test_ok_plan(plan as usize);
                let run_result = self.call_sub_value(*block, vec![], true);
                self.finish_subtest(ctx, &desc, run_result.map(|_| ()))?;
                return Ok(Value::Bool(true));
            }
        }

        Err(RuntimeError::new(
            "group-of expects a nested Pair: plan => desc => block",
        ))
    }

    /// Set plan count directly (for group-of).
    fn test_ok_plan(&mut self, count: usize) {
        let state = self.test_state.get_or_insert_with(TestState::new);
        state.planned = Some(count);
        self.output.push_str(&format!("1..{}\n", count));
    }

    /// Clean up temp files and directories created by make-temp-file/make-temp-dir.
    pub(crate) fn cleanup_temp_files(&mut self) {
        for path in self.temp_files.drain(..) {
            let _ = fs::remove_file(&path);
        }
        for path in self.temp_dirs.drain(..) {
            let _ = fs::remove_dir_all(&path);
        }
    }
}
