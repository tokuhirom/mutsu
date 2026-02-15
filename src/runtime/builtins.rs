use super::*;

impl Interpreter {
    pub(crate) fn eval_call_on_value(
        &mut self,
        target_val: Value,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        if let Value::Sub {
            package,
            name,
            params,
            body,
            env,
            ..
        } = target_val
        {
            let saved_env = self.env.clone();
            let mut new_env = saved_env.clone();
            for (k, v) in env {
                if matches!(new_env.get(&k), Some(Value::Array(_))) && matches!(v, Value::Array(_))
                {
                    continue;
                }
                new_env.insert(k, v);
            }
            // Bind named params
            for (i, pname) in params.iter().enumerate() {
                if let Some(value) = args.get(i) {
                    new_env.insert(pname.clone(), value.clone());
                }
            }
            // Bind placeholder variables ($^a, $^b, ...)
            let placeholders = collect_placeholders(&body);
            if !placeholders.is_empty() {
                for (i, ph) in placeholders.iter().enumerate() {
                    if let Some(val) = args.get(i) {
                        new_env.insert(ph.clone(), val.clone());
                    }
                }
            }
            let block_sub = Value::Sub {
                package: package.clone(),
                name: name.clone(),
                params: params.clone(),
                body: body.clone(),
                env: new_env.clone(),
                id: next_instance_id(),
            };
            self.env = new_env;
            self.routine_stack.push((package.clone(), name.clone()));
            self.block_stack.push(block_sub);
            let result = self.eval_block_value(&body);
            self.block_stack.pop();
            self.routine_stack.pop();
            let mut merged = saved_env;
            for (k, v) in self.env.iter() {
                if matches!(v, Value::Array(_)) {
                    merged.insert(k.clone(), v.clone());
                }
            }
            self.env = merged;
            return match result {
                Err(e) if e.return_value.is_some() => Ok(e.return_value.unwrap()),
                other => other,
            };
        }
        if matches!(target_val, Value::Routine { .. }) {
            return self.call_sub_value(target_val, args, false);
        }
        Ok(Value::Nil)
    }

    pub(crate) fn call_function(
        &mut self,
        name: &str,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        crate::trace::trace_log!("call", "call_function: {} ({} args)", name, args.len());
        if name == "die" || name == "fail" {
            let msg = args
                .first()
                .map(|v| v.to_string_value())
                .unwrap_or_else(|| "Died".to_string());
            return Err(RuntimeError::new(&msg));
        }
        if matches!(name, "Int" | "Num" | "Str" | "Bool")
            && let Some(value) = args.first().cloned()
        {
            let coerced = match name {
                "Int" => match value {
                    Value::Int(i) => Value::Int(i),
                    Value::Num(f) => Value::Int(f as i64),
                    Value::Rat(n, d) => {
                        if d == 0 {
                            Value::Int(0)
                        } else {
                            Value::Int(n / d)
                        }
                    }
                    Value::Complex(r, _) => Value::Int(r as i64),
                    Value::Str(s) => Value::Int(s.trim().parse::<i64>().unwrap_or(0)),
                    Value::Bool(b) => Value::Int(if b { 1 } else { 0 }),
                    _ => Value::Int(0),
                },
                "Num" => match value {
                    Value::Int(i) => Value::Num(i as f64),
                    Value::Num(f) => Value::Num(f),
                    Value::Rat(n, d) => {
                        if d == 0 {
                            Value::Num(if n == 0 {
                                f64::NAN
                            } else if n > 0 {
                                f64::INFINITY
                            } else {
                                f64::NEG_INFINITY
                            })
                        } else {
                            Value::Num(n as f64 / d as f64)
                        }
                    }
                    Value::Complex(r, _) => Value::Num(r),
                    Value::Str(s) => {
                        if let Ok(i) = s.trim().parse::<i64>() {
                            Value::Num(i as f64)
                        } else if let Ok(f) = s.trim().parse::<f64>() {
                            Value::Num(f)
                        } else {
                            Value::Num(0.0)
                        }
                    }
                    Value::Bool(b) => Value::Num(if b { 1.0 } else { 0.0 }),
                    _ => Value::Num(0.0),
                },
                "Str" => Value::Str(value.to_string_value()),
                "Bool" => Value::Bool(value.truthy()),
                _ => Value::Nil,
            };
            return Ok(coerced);
        }
        if name == "make" {
            let value = args.first().cloned().unwrap_or(Value::Nil);
            self.env.insert("made".to_string(), value.clone());
            return Ok(value);
        }
        if name == "made" {
            return Ok(self.env.get("made").cloned().unwrap_or(Value::Nil));
        }
        if name == "undefine" {
            return Ok(Value::Nil);
        }
        if name == "VAR" {
            return Ok(args.first().cloned().unwrap_or(Value::Nil));
        }
        if name == "shift" || name == "pop" {
            return Ok(match args.first().cloned() {
                Some(Value::Array(mut items)) => {
                    if items.is_empty() {
                        Value::Nil
                    } else if name == "shift" {
                        items.remove(0)
                    } else {
                        items.pop().unwrap_or(Value::Nil)
                    }
                }
                _ => Value::Nil,
            });
        }
        if matches!(name, "push" | "unshift" | "append" | "prepend") {
            return Ok(Value::Nil);
        }
        if (self.loaded_modules.contains("Test")
            || self.loaded_modules.iter().any(|m| m.starts_with("Test::")))
            && let Some(result) = self.call_test_function(name, &args)?
        {
            return Ok(result);
        }
        if name == "callframe" || name == "caller" {
            let default_depth = if name == "caller" { 1 } else { 0 };
            let depth = args
                .first()
                .and_then(|v| match v {
                    Value::Int(i) if *i >= 0 => Some(*i as usize),
                    Value::Num(f) if *f >= 0.0 => Some(*f as usize),
                    _ => None,
                })
                .unwrap_or(default_depth);
            if let Some(frame) = self.callframe_value(depth) {
                return Ok(frame);
            }
            return Ok(Value::Nil);
        }
        if name == "EVALFILE" {
            let path = args
                .first()
                .map(|v| v.to_string_value())
                .ok_or_else(|| RuntimeError::new("EVALFILE requires a filename"))?;
            let code = fs::read_to_string(&path)
                .map_err(|err| RuntimeError::new(format!("Failed to read {}: {}", path, err)))?;
            return self.eval_eval_string(&code);
        }
        if name == "EVAL" {
            let code = args
                .first()
                .map(|v| v.to_string_value())
                .unwrap_or_default();
            if code.contains("&?ROUTINE") && self.routine_stack.is_empty() {
                return Err(RuntimeError::new("X::Undeclared::Symbols"));
            }
            return self.eval_eval_string(&code);
        }
        if name == "exit" {
            let _code = match args.first() {
                Some(Value::Int(i)) => *i,
                _ => 0,
            };
            self.halted = true;
            return Ok(Value::Nil);
        }
        if name == "dd" {
            let val = args.first().cloned().unwrap_or(Value::Nil);
            self.output.push_str(&format!("{:?}\n", val));
            return Ok(val);
        }
        if name == "elems" {
            let val = args.first().cloned();
            return Ok(match val {
                Some(Value::Array(items)) => Value::Int(items.len() as i64),
                Some(Value::LazyList(list)) => {
                    Value::Int(self.force_lazy_list(&list)?.len() as i64)
                }
                Some(Value::Hash(items)) => Value::Int(items.len() as i64),
                Some(Value::Str(s)) => Value::Int(s.chars().count() as i64),
                _ => Value::Int(0),
            });
        }
        if name == "set" {
            let mut elems = HashSet::new();
            for arg in &args {
                match arg {
                    Value::Array(items) => {
                        for item in items {
                            elems.insert(item.to_string_value());
                        }
                    }
                    other => {
                        elems.insert(other.to_string_value());
                    }
                }
            }
            return Ok(Value::Set(elems));
        }
        if name == "bag" {
            let mut counts: HashMap<String, i64> = HashMap::new();
            for arg in &args {
                match arg {
                    Value::Array(items) => {
                        for item in items {
                            *counts.entry(item.to_string_value()).or_insert(0) += 1;
                        }
                    }
                    other => {
                        *counts.entry(other.to_string_value()).or_insert(0) += 1;
                    }
                }
            }
            return Ok(Value::Bag(counts));
        }
        if name == "mix" {
            let mut weights: HashMap<String, f64> = HashMap::new();
            for arg in &args {
                match arg {
                    Value::Array(items) => {
                        for item in items {
                            *weights.entry(item.to_string_value()).or_insert(0.0) += 1.0;
                        }
                    }
                    other => {
                        *weights.entry(other.to_string_value()).or_insert(0.0) += 1.0;
                    }
                }
            }
            return Ok(Value::Mix(weights));
        }
        if name == "hash" {
            let mut flat_values = Vec::new();
            for arg in &args {
                flat_values.extend(Self::value_to_list(arg));
            }
            let mut map = HashMap::new();
            let mut iter = flat_values.into_iter();
            while let Some(item) = iter.next() {
                match item {
                    Value::Pair(key, boxed_val) => {
                        map.insert(key, *boxed_val);
                    }
                    other => {
                        let key = other.to_string_value();
                        let value = iter.next().unwrap_or(Value::Nil);
                        map.insert(key, value);
                    }
                }
            }
            return Ok(Value::Hash(map));
        }
        if matches!(name, "any" | "all" | "one" | "none") {
            let kind = match name {
                "any" => JunctionKind::Any,
                "all" => JunctionKind::All,
                "one" => JunctionKind::One,
                _ => JunctionKind::None,
            };
            let mut elems = Vec::new();
            for arg in args {
                match arg {
                    Value::Array(items) => elems.extend(items),
                    other => elems.push(other),
                }
            }
            return Ok(Value::Junction {
                kind,
                values: elems,
            });
        }
        if name == "pair" {
            let key = args
                .first()
                .map(|v| v.to_string_value())
                .unwrap_or_default();
            let val = args.get(1).cloned().unwrap_or(Value::Nil);
            return Ok(Value::Pair(key, Box::new(val)));
        }
        if name == "slurp" {
            let path = args
                .first()
                .map(|v| v.to_string_value())
                .ok_or_else(|| RuntimeError::new("slurp requires a path argument"))?;
            let content = fs::read_to_string(&path)
                .map_err(|err| RuntimeError::new(format!("Failed to slurp '{}': {}", path, err)))?;
            return Ok(Value::Str(content));
        }
        if name == "spurt" {
            let path = args
                .first()
                .map(|v| v.to_string_value())
                .ok_or_else(|| RuntimeError::new("spurt requires a path argument"))?;
            let content = args
                .get(1)
                .map(|v| v.to_string_value())
                .ok_or_else(|| RuntimeError::new("spurt requires a content argument"))?;
            fs::write(&path, &content)
                .map_err(|err| RuntimeError::new(format!("Failed to spurt '{}': {}", path, err)))?;
            return Ok(Value::Bool(true));
        }
        if name == "getlogin" {
            let login = Self::get_login_name().unwrap_or_default();
            return Ok(Value::Str(login));
        }
        if name == "gethost" {
            let host_str = args.first().map(|v| v.to_string_value());
            let hostname = host_str.unwrap_or_else(Self::hostname);
            let addrs = Self::resolve_host(&hostname);
            return Ok(Self::make_os_name_value(hostname, addrs));
        }
        if name == "chroot" {
            let path_str = args
                .first()
                .map(|v| v.to_string_value())
                .or_else(|| {
                    self.get_dynamic_string("$*CWD")
                        .or_else(|| self.get_dynamic_string("$*CHROOT"))
                })
                .unwrap_or_else(|| ".".to_string());
            let path_buf = PathBuf::from(path_str.clone());
            if !path_buf.is_dir() {
                return Ok(Value::Bool(false));
            }
            let canonical = path_buf.canonicalize().unwrap_or_else(|_| path_buf.clone());
            if std::env::set_current_dir(&canonical).is_err() {
                return Ok(Value::Bool(false));
            }
            self.chroot_root = Some(canonical.clone());
            let repr = Self::stringify_path(&canonical);
            self.env
                .insert("$*CHROOT".to_string(), Value::Str(repr.clone()));
            self.env.insert("$*CWD".to_string(), Value::Str(repr));
            return Ok(Value::Bool(true));
        }
        if name == "shell" {
            let command_str = args
                .first()
                .map(|v| v.to_string_value())
                .unwrap_or_default();
            if command_str.is_empty() {
                return Ok(Value::Bool(false));
            }
            let mut opts = HashMap::new();
            let mut cwd_opt: Option<String> = None;
            for value in args.iter().skip(1) {
                if let Value::Hash(map) = value {
                    for (key, inner) in map {
                        match key.as_str() {
                            "cwd" => {
                                cwd_opt = Some(inner.to_string_value());
                            }
                            "env" => {
                                if let Value::Hash(env_map) = inner {
                                    for (ek, ev) in env_map {
                                        opts.insert(ek.clone(), ev.to_string_value());
                                    }
                                }
                            }
                            _ => {}
                        }
                    }
                }
            }
            let mut command = if cfg!(windows) {
                let mut cmd = Command::new("cmd");
                cmd.arg("/C").arg(&command_str);
                cmd
            } else {
                let mut cmd = Command::new("sh");
                cmd.arg("-c").arg(&command_str);
                cmd
            };
            if let Some(cwd) = cwd_opt {
                command.current_dir(cwd);
            }
            for (k, v) in opts {
                command.env(k, v);
            }
            let status = command
                .status()
                .map(|status| status.success())
                .unwrap_or(false);
            return Ok(Value::Bool(status));
        }
        if name == "kill" {
            let signal = args.first().map(super::to_int).unwrap_or(15);
            let mut success = true;
            let mut had_pid = false;
            for val in args.iter().skip(1) {
                had_pid = true;
                let pid = super::to_int(val);
                success &= Self::send_signal(pid, signal);
            }
            if !had_pid {
                success = false;
            }
            return Ok(Value::Bool(success));
        }
        if name == "syscall" {
            let num_val = args.first();
            if let Some(val) = num_val {
                let num = super::to_int(val);
                if num == 0 {
                    let pid = self
                        .env
                        .get("*PID")
                        .and_then(|v| match v {
                            Value::Int(i) => Some(*i),
                            _ => None,
                        })
                        .unwrap_or_else(|| std::process::id() as i64);
                    return Ok(Value::Int(pid));
                }
                return Ok(Value::Int(-1));
            }
            return Ok(Value::Nil);
        }
        if name == "sleep" {
            let duration =
                Self::duration_from_seconds(Self::seconds_from_value(args.first().cloned()));
            thread::sleep(duration);
            return Ok(Value::Nil);
        }
        if name == "sleep-timer" {
            let duration =
                Self::duration_from_seconds(Self::seconds_from_value(args.first().cloned()));
            let start = Instant::now();
            thread::sleep(duration);
            let elapsed = start.elapsed();
            let remaining = duration.checked_sub(elapsed).unwrap_or_default();
            return Ok(Value::Num(remaining.as_secs_f64()));
        }
        if name == "sleep-till" {
            if let Some(target_time) = Self::system_time_from_value(args.first().cloned()) {
                let now = SystemTime::now();
                if target_time <= now {
                    return Ok(Value::Bool(false));
                }
                if let Ok(diff) = target_time.duration_since(now) {
                    thread::sleep(diff);
                    return Ok(Value::Bool(true));
                }
            }
            return Ok(Value::Bool(false));
        }
        if name == "chrs" {
            let mut result = String::new();
            for arg in &args {
                for item in Self::value_to_list(arg) {
                    if let Value::Int(i) = item
                        && i >= 0
                        && (i as u64) <= 0x10ffff
                        && let Some(ch) = std::char::from_u32(i as u32)
                    {
                        result.push(ch);
                        continue;
                    }
                    result.push_str(&item.to_string_value());
                }
            }
            return Ok(Value::Str(result));
        }
        if name == "chr" {
            if let Some(Value::Int(i)) = args.first()
                && *i >= 0
                && let Some(ch) = std::char::from_u32(*i as u32)
            {
                return Ok(Value::Str(ch.to_string()));
            }
            return Ok(Value::Str(String::new()));
        }
        if name == "ord" {
            if let Some(val) = args.first()
                && let Some(ch) = val.to_string_value().chars().next()
            {
                return Ok(Value::Int(ch as u32 as i64));
            }
            return Ok(Value::Nil);
        }
        if name == "keys" {
            let val = args.first().cloned();
            return Ok(match val {
                Some(Value::Hash(items)) => {
                    Value::Array(items.keys().map(|k| Value::Str(k.clone())).collect())
                }
                _ => Value::Array(Vec::new()),
            });
        }
        if name == "values" {
            let val = args.first().cloned();
            return Ok(match val {
                Some(Value::Hash(items)) => Value::Array(items.values().cloned().collect()),
                _ => Value::Array(Vec::new()),
            });
        }
        if name == "abs" {
            let val = args.first().cloned();
            return Ok(match val {
                Some(Value::Int(i)) => Value::Int(i.abs()),
                Some(Value::Num(f)) => Value::Num(f.abs()),
                _ => Value::Int(0),
            });
        }
        if name == "chars" {
            use unicode_segmentation::UnicodeSegmentation;
            let val = args.first().cloned();
            return Ok(match val {
                Some(Value::Str(s)) => Value::Int(s.graphemes(true).count() as i64),
                Some(v) => Value::Int(v.to_string_value().graphemes(true).count() as i64),
                _ => Value::Int(0),
            });
        }
        if name == "join" {
            let sep = args
                .first()
                .map(|v| v.to_string_value())
                .unwrap_or_default();
            let list = args.get(1).cloned();
            return Ok(match list {
                Some(Value::Array(items)) => {
                    let joined = items
                        .iter()
                        .map(|v| v.to_string_value())
                        .collect::<Vec<_>>()
                        .join(&sep);
                    Value::Str(joined)
                }
                _ => Value::Str(String::new()),
            });
        }
        if name == "item" {
            return Ok(args.first().cloned().unwrap_or(Value::Nil));
        }
        if name == "list" {
            let mut result = Vec::new();
            for arg in &args {
                result.extend(Self::value_to_list(arg));
            }
            return Ok(Value::Array(result));
        }
        if name == "lol" {
            return Ok(Value::Array(args.clone()));
        }
        if name == "flat" {
            let mut result = Vec::new();
            for arg in &args {
                match arg {
                    Value::Array(items) => {
                        for item in items {
                            if let Value::Array(sub) = item {
                                result.extend(sub.clone());
                            } else {
                                result.push(item.clone());
                            }
                        }
                    }
                    other => result.push(other.clone()),
                }
            }
            return Ok(Value::Array(result));
        }
        if name == "slip" {
            let mut items = Vec::new();
            for arg in &args {
                match arg {
                    Value::Array(elems) => items.extend(elems.clone()),
                    Value::Slip(elems) => items.extend(elems.clone()),
                    other => items.push(other.clone()),
                }
            }
            return Ok(Value::Slip(items));
        }
        if name == "reverse" {
            let val = args.first().cloned();
            return Ok(match val {
                Some(Value::Array(mut items)) => {
                    items.reverse();
                    Value::Array(items)
                }
                Some(Value::Str(s)) => Value::Str(s.chars().rev().collect()),
                _ => Value::Nil,
            });
        }
        if name == "sort" {
            let val = args.first().cloned();
            return Ok(match val {
                Some(Value::Array(mut items)) => {
                    items.sort_by_key(|a| a.to_string_value());
                    Value::Array(items)
                }
                _ => Value::Nil,
            });
        }
        if name == "ords" {
            if let Some(val) = args.first() {
                let mut codes = Vec::new();
                for ch in val.to_string_value().chars() {
                    codes.push(Value::Int(ch as u32 as i64));
                }
                return Ok(Value::Array(codes));
            }
            return Ok(Value::Array(Vec::new()));
        }
        if name == "flip" {
            let val = args
                .first()
                .map(|v| v.to_string_value())
                .unwrap_or_default();
            return Ok(Value::Str(val.chars().rev().collect()));
        }
        if name == "lc" {
            let val = args.first().cloned().unwrap_or(Value::Nil);
            return Ok(Value::Str(val.to_string_value().to_lowercase()));
        }
        if name == "uc" {
            let val = args.first().cloned().unwrap_or(Value::Nil);
            return Ok(Value::Str(val.to_string_value().to_uppercase()));
        }
        if name == "tc" {
            let val = args
                .first()
                .map(|v| v.to_string_value())
                .unwrap_or_default();
            let mut result = String::new();
            let mut capitalize = true;
            for ch in val.chars() {
                if capitalize {
                    for c in ch.to_uppercase() {
                        result.push(c);
                    }
                    capitalize = false;
                } else {
                    result.push(ch);
                }
            }
            return Ok(Value::Str(result));
        }
        if name == "trim" {
            let val = args
                .first()
                .map(|v| v.to_string_value())
                .unwrap_or_default();
            return Ok(Value::Str(val.trim().to_string()));
        }
        if name == "sprintf" {
            let fmt = match args.first() {
                Some(Value::Str(s)) => s.clone(),
                _ => String::new(),
            };
            let rendered = super::format_sprintf(&fmt, args.get(1));
            return Ok(Value::Str(rendered));
        }
        if name == "map" {
            let func = args.first().cloned();
            let mut list_items = Vec::new();
            for arg in args.iter().skip(1) {
                match arg {
                    Value::Array(items) => list_items.extend(items.clone()),
                    Value::Range(a, b) => list_items.extend((*a..=*b).map(Value::Int)),
                    Value::RangeExcl(a, b) => list_items.extend((*a..*b).map(Value::Int)),
                    Value::RangeExclStart(a, b) => list_items.extend((*a + 1..=*b).map(Value::Int)),
                    Value::RangeExclBoth(a, b) => list_items.extend((*a + 1..*b).map(Value::Int)),
                    other => list_items.push(other.clone()),
                }
            }
            return self.eval_map_over_items(func, list_items);
        }
        if name == "grep" {
            let func = args.first().cloned();
            let mut list_items = Vec::new();
            for arg in args.iter().skip(1) {
                match arg {
                    Value::Array(items) => list_items.extend(items.clone()),
                    Value::Range(a, b) => list_items.extend((*a..=*b).map(Value::Int)),
                    Value::RangeExcl(a, b) => list_items.extend((*a..*b).map(Value::Int)),
                    other => list_items.push(other.clone()),
                }
            }
            return self.eval_grep_over_items(func, list_items);
        }
        if name == "classify" || name == "categorize" {
            let func = match args.first() {
                Some(value) => value.clone(),
                None => return Ok(Value::Hash(HashMap::new())),
            };
            let mut buckets: HashMap<String, Vec<Value>> = HashMap::new();
            for item in args.iter().skip(1) {
                let keys = match self.call_lambda_with_arg(&func, item.clone()) {
                    Ok(Value::Array(values)) => values,
                    Ok(value) => vec![value],
                    Err(_) => vec![Value::Nil],
                };
                let target_keys = if name == "classify" {
                    if keys.is_empty() {
                        vec![Value::Nil]
                    } else {
                        vec![keys[0].clone()]
                    }
                } else {
                    keys
                };
                for key in target_keys {
                    let bucket_key = key.to_string_value();
                    buckets.entry(bucket_key).or_default().push(item.clone());
                }
            }
            let hash_map = buckets
                .into_iter()
                .map(|(k, v)| (k, Value::Array(v)))
                .collect();
            return Ok(Value::Hash(hash_map));
        }
        if name == "min" {
            let mut vals = Vec::new();
            for arg in &args {
                vals.push(arg.clone());
            }
            return Ok(vals
                .into_iter()
                .min_by(|a, b| match (a, b) {
                    (Value::Int(x), Value::Int(y)) => x.cmp(y),
                    _ => a.to_string_value().cmp(&b.to_string_value()),
                })
                .unwrap_or(Value::Nil));
        }
        if name == "max" {
            let mut vals = Vec::new();
            for arg in &args {
                vals.push(arg.clone());
            }
            return Ok(vals
                .into_iter()
                .max_by(|a, b| match (a, b) {
                    (Value::Int(x), Value::Int(y)) => x.cmp(y),
                    _ => a.to_string_value().cmp(&b.to_string_value()),
                })
                .unwrap_or(Value::Nil));
        }
        if name == "unlink" {
            let path = args
                .first()
                .map(|v| v.to_string_value())
                .ok_or_else(|| RuntimeError::new("unlink requires a path argument"))?;
            fs::remove_file(&path).map_err(|err| {
                RuntimeError::new(format!("Failed to unlink '{}': {}", path, err))
            })?;
            return Ok(Value::Bool(true));
        }
        if name == "print" || name == "say" || name == "note" {
            let mut content = String::new();
            for arg in &args {
                content.push_str(&arg.to_string_value());
            }
            let (handle, newline) = match name {
                "print" => ("$*OUT", false),
                "say" => ("$*OUT", true),
                _ => ("$*ERR", true),
            };
            self.write_to_named_handle(handle, &content, newline)?;
            return Ok(Value::Nil);
        }
        if name == "prompt" {
            let msg = args
                .first()
                .map(|v| v.to_string_value())
                .unwrap_or_default();
            self.write_to_named_handle("$*OUT", &msg, false)?;
            if let Some(handle) = self.default_input_handle() {
                let line = self.read_line_from_handle_value(&handle)?;
                return Ok(Value::Str(line));
            }
            return Ok(Value::Str(String::new()));
        }
        if name == "get" {
            let handle = args
                .first()
                .cloned()
                .or_else(|| self.default_input_handle());
            if let Some(handle) = handle {
                let line = self.read_line_from_handle_value(&handle)?;
                return Ok(Value::Str(line));
            }
            return Ok(Value::Str(String::new()));
        }
        if name == "lines" {
            let handle = args
                .first()
                .cloned()
                .or_else(|| self.default_input_handle());
            if let Some(handle) = handle {
                let mut lines = Vec::new();
                loop {
                    let line = self.read_line_from_handle_value(&handle)?;
                    if line.is_empty() {
                        break;
                    }
                    lines.push(Value::Str(line));
                }
                return Ok(Value::Array(lines));
            }
            return Ok(Value::Array(Vec::new()));
        }
        if name == "words" {
            let handle = if args.is_empty() {
                self.default_input_handle()
            } else if args.first().and_then(Self::handle_id_from_value).is_some() {
                args.first().cloned()
            } else {
                None
            };
            if let Some(handle) = handle {
                let mut words = Vec::new();
                loop {
                    let line = self.read_line_from_handle_value(&handle)?;
                    if line.is_empty() {
                        break;
                    }
                    for token in line.split_whitespace() {
                        words.push(Value::Str(token.to_string()));
                    }
                }
                return Ok(Value::Array(words));
            }
        }
        if name == "open" {
            let path = args
                .first()
                .map(|v| v.to_string_value())
                .ok_or_else(|| RuntimeError::new("open requires a path argument"))?;
            let (read, write, append) = Self::parse_io_flags_values(&args[1..]);
            let path_buf = self.resolve_path(&path);
            return self.open_file_handle(&path_buf, read, write, append);
        }
        if name == "close" {
            let handle = args
                .first()
                .ok_or_else(|| RuntimeError::new("close requires a handle"))?;
            return Ok(Value::Bool(self.close_handle_value(handle)?));
        }
        if name == "dir" {
            let requested = args
                .first()
                .map(|v| v.to_string_value())
                .unwrap_or_else(|| {
                    self.get_dynamic_string("$*CWD")
                        .unwrap_or_else(|| ".".to_string())
                });
            let path_buf = self.resolve_path(&requested);
            let mut entries = Vec::new();
            for entry in fs::read_dir(&path_buf).map_err(|err| {
                RuntimeError::new(format!("Failed to read dir '{}': {}", requested, err))
            })? {
                let entry = entry.map_err(|err| {
                    RuntimeError::new(format!("Failed to read dir entry '{}': {}", requested, err))
                })?;
                entries.push(Value::Str(entry.path().to_string_lossy().to_string()));
            }
            return Ok(Value::Array(entries));
        }
        if name == "copy" {
            let source = args
                .first()
                .map(|v| v.to_string_value())
                .ok_or_else(|| RuntimeError::new("copy requires a source path"))?;
            let dest = args
                .get(1)
                .map(|v| v.to_string_value())
                .ok_or_else(|| RuntimeError::new("copy requires a destination path"))?;
            let src_buf = self.resolve_path(&source);
            let dest_buf = self.resolve_path(&dest);
            fs::copy(&src_buf, &dest_buf).map_err(|err| {
                RuntimeError::new(format!("Failed to copy '{}': {}", source, err))
            })?;
            return Ok(Value::Bool(true));
        }
        if name == "rename" || name == "move" {
            let source = args
                .first()
                .map(|v| v.to_string_value())
                .ok_or_else(|| RuntimeError::new("rename requires a source path"))?;
            let dest = args
                .get(1)
                .map(|v| v.to_string_value())
                .ok_or_else(|| RuntimeError::new("rename requires a destination path"))?;
            let src_buf = self.resolve_path(&source);
            let dest_buf = self.resolve_path(&dest);
            fs::rename(&src_buf, &dest_buf).map_err(|err| {
                RuntimeError::new(format!("Failed to rename '{}': {}", source, err))
            })?;
            return Ok(Value::Bool(true));
        }
        if name == "chmod" {
            let path = args
                .first()
                .map(|v| v.to_string_value())
                .ok_or_else(|| RuntimeError::new("chmod requires a path"))?;
            let mode_value = args
                .get(1)
                .cloned()
                .ok_or_else(|| RuntimeError::new("chmod requires a mode"))?;
            let mode_int = match mode_value {
                Value::Int(i) => i as u32,
                Value::Str(s) => u32::from_str_radix(&s, 8).unwrap_or(0),
                other => {
                    return Err(RuntimeError::new(format!(
                        "Invalid mode: {}",
                        other.to_string_value()
                    )));
                }
            };
            let path_buf = self.resolve_path(&path);
            #[cfg(unix)]
            {
                let perms = PermissionsExt::from_mode(mode_int);
                fs::set_permissions(&path_buf, perms).map_err(|err| {
                    RuntimeError::new(format!("Failed to chmod '{}': {}", path, err))
                })?;
            }
            #[cfg(not(unix))]
            {
                return Err(RuntimeError::new("chmod not supported on this platform"));
            }
            return Ok(Value::Bool(true));
        }
        if name == "mkdir" {
            let path = args
                .first()
                .map(|v| v.to_string_value())
                .unwrap_or_else(|| {
                    self.get_dynamic_string("$*CWD")
                        .unwrap_or_else(|| ".".to_string())
                });
            let path_buf = self.resolve_path(&path);
            fs::create_dir_all(&path_buf)
                .map_err(|err| RuntimeError::new(format!("Failed to mkdir '{}': {}", path, err)))?;
            return Ok(Value::Bool(true));
        }
        if name == "rmdir" {
            let path = args
                .first()
                .map(|v| v.to_string_value())
                .ok_or_else(|| RuntimeError::new("rmdir requires a path"))?;
            let path_buf = self.resolve_path(&path);
            fs::remove_dir(&path_buf)
                .map_err(|err| RuntimeError::new(format!("Failed to rmdir '{}': {}", path, err)))?;
            return Ok(Value::Bool(true));
        }
        if name == "chdir" {
            let path = args
                .first()
                .map(|v| v.to_string_value())
                .ok_or_else(|| RuntimeError::new("chdir requires a path"))?;
            let path_buf = self.resolve_path(&path);
            if !path_buf.is_dir() {
                return Err(RuntimeError::new(format!(
                    "chdir path is not a directory: {}",
                    path
                )));
            }
            self.env.insert(
                "$*CWD".to_string(),
                Value::Str(Self::stringify_path(&path_buf)),
            );
            return Ok(Value::Bool(true));
        }
        if name == "indir" {
            let path = args
                .first()
                .map(|v| v.to_string_value())
                .ok_or_else(|| RuntimeError::new("indir requires a path"))?;
            let path_buf = self.resolve_path(&path);
            if !path_buf.is_dir() {
                return Err(RuntimeError::new(format!(
                    "indir path is not a directory: {}",
                    path
                )));
            }
            let saved = self.env.get("$*CWD").cloned();
            self.env.insert(
                "$*CWD".to_string(),
                Value::Str(Self::stringify_path(&path_buf)),
            );
            let result = if let Some(body) = args.get(1) {
                if matches!(body, Value::Sub { .. }) {
                    self.call_sub_value(body.clone(), vec![], false)
                } else {
                    Ok(body.clone())
                }
            } else {
                Ok(Value::Nil)
            };
            if let Some(prev) = saved {
                self.env.insert("$*CWD".to_string(), prev);
            } else {
                self.env.remove("$*CWD");
            }
            return result;
        }
        if name == "tmpdir" {
            if let Some(path_value) = args.first() {
                let path = path_value.to_string_value();
                let path_buf = self.resolve_path(&path);
                if !path_buf.is_dir() {
                    return Err(RuntimeError::new("tmpdir path must be a directory"));
                }
                let repr = Self::stringify_path(&path_buf);
                self.env
                    .insert("$*TMPDIR".to_string(), Value::Str(repr.clone()));
                return Ok(Value::Str(repr));
            }
            return Ok(Value::Str(
                self.get_dynamic_string("$*TMPDIR").unwrap_or_default(),
            ));
        }
        if name == "homedir" {
            if let Some(path_value) = args.first() {
                let path = path_value.to_string_value();
                let path_buf = self.resolve_path(&path);
                if !path_buf.is_dir() {
                    return Err(RuntimeError::new("homedir path must be a directory"));
                }
                let repr = Self::stringify_path(&path_buf);
                self.env
                    .insert("$*HOME".to_string(), Value::Str(repr.clone()));
                return Ok(Value::Str(repr));
            }
            return Ok(Value::Str(
                self.get_dynamic_string("$*HOME").unwrap_or_default(),
            ));
        }
        if name == "link" {
            let target = args
                .first()
                .map(|v| v.to_string_value())
                .ok_or_else(|| RuntimeError::new("link requires a target"))?;
            let link = args
                .get(1)
                .map(|v| v.to_string_value())
                .ok_or_else(|| RuntimeError::new("link requires a link name"))?;
            let target_buf = self.resolve_path(&target);
            let link_buf = self.resolve_path(&link);
            fs::hard_link(&target_buf, &link_buf).map_err(|err| {
                RuntimeError::new(format!("Failed to create link '{}': {}", target, err))
            })?;
            return Ok(Value::Bool(true));
        }
        if name == "symlink" {
            let target = args
                .first()
                .map(|v| v.to_string_value())
                .ok_or_else(|| RuntimeError::new("symlink requires a target"))?;
            let link = args
                .get(1)
                .map(|v| v.to_string_value())
                .ok_or_else(|| RuntimeError::new("symlink requires a link name"))?;
            let target_buf = self.resolve_path(&target);
            let link_buf = self.resolve_path(&link);
            #[cfg(unix)]
            {
                unix_fs::symlink(&target_buf, &link_buf).map_err(|err| {
                    RuntimeError::new(format!("Failed to symlink '{}': {}", target, err))
                })?;
            }
            #[cfg(windows)]
            {
                let metadata = fs::metadata(&target_buf);
                if metadata.map(|meta| meta.is_dir()).unwrap_or(false) {
                    windows_fs::symlink_dir(&target_buf, &link_buf).map_err(|err| {
                        RuntimeError::new(format!("Failed to symlink '{}': {}", target, err))
                    })?;
                } else {
                    windows_fs::symlink_file(&target_buf, &link_buf).map_err(|err| {
                        RuntimeError::new(format!("Failed to symlink '{}': {}", target, err))
                    })?;
                }
            }
            return Ok(Value::Bool(true));
        }
        if let Some(pattern) = self.eval_token_call_values(name, &args)? {
            return Ok(Value::Regex(pattern));
        }
        if let Some(native_result) = crate::builtins::native_function(name, &args) {
            return native_result;
        }
        if let Some(def) = self.resolve_function_with_types(name, &args) {
            let saved_env = self.env.clone();
            self.bind_function_args_values(&def.param_defs, &def.params, &args)?;
            self.routine_stack
                .push((def.package.clone(), def.name.clone()));
            let result = self.eval_block_value(&def.body);
            self.routine_stack.pop();
            self.env = saved_env;
            return match result {
                Err(e) if e.return_value.is_some() => Ok(e.return_value.unwrap()),
                other => other,
            };
        }
        if self.has_proto(name) {
            return Err(RuntimeError::new(format!(
                "No matching candidates for proto sub: {}",
                name
            )));
        }

        Err(RuntimeError::new(format!(
            "Unknown function (call_function fallback disabled): {}",
            name
        )))
    }

    pub(super) fn is_builtin_function(name: &str) -> bool {
        matches!(
            name,
            "defined"
                | "undefine"
                | "say"
                | "print"
                | "put"
                | "note"
                | "die"
                | "warn"
                | "exit"
                | "abs"
                | "sqrt"
                | "floor"
                | "ceiling"
                | "ceil"
                | "round"
                | "exp"
                | "log"
                | "sin"
                | "cos"
                | "tan"
                | "asin"
                | "acos"
                | "atan"
                | "chr"
                | "ord"
                | "chars"
                | "chomp"
                | "chop"
                | "flip"
                | "lc"
                | "uc"
                | "tc"
                | "trim"
                | "elems"
                | "keys"
                | "values"
                | "pairs"
                | "sort"
                | "reverse"
                | "join"
                | "map"
                | "grep"
                | "push"
                | "pop"
                | "shift"
                | "unshift"
                | "indir"
                | "splice"
                | "flat"
                | "unique"
                | "squish"
                | "min"
                | "max"
                | "sum"
                | "any"
                | "all"
                | "none"
                | "one"
                | "so"
                | "not"
                | "truncate"
        )
    }

    pub(super) fn call_lambda_with_arg(
        &mut self,
        func: &Value,
        item: Value,
    ) -> Result<Value, RuntimeError> {
        if let Value::Sub {
            params, body, env, ..
        } = func
        {
            let saved_env = self.env.clone();
            for (k, v) in env {
                self.env.insert(k.clone(), v.clone());
            }
            if let Some(p) = params.first() {
                self.env.insert(p.clone(), item.clone());
            }
            let placeholders = collect_placeholders(body);
            if let Some(ph) = placeholders.first() {
                self.env.insert(ph.clone(), item.clone());
            }
            self.env.insert("_".to_string(), item.clone());
            let result = self.eval_block_value(body);
            self.env = saved_env;
            return result;
        }
        Err(RuntimeError::new("Expected callable"))
    }
}
