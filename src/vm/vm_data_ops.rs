use super::*;

impl VM {
    pub(super) fn exec_make_array_op(&mut self, n: u32, is_real_array: bool) {
        let n = n as usize;
        let start = self.stack.len() - n;
        let raw: Vec<Value> = self.stack.drain(start..).collect();
        let mut elems = Vec::with_capacity(raw.len());
        for val in raw {
            match val {
                Value::Slip(items) => elems.extend(items.iter().cloned()),
                // In bracket-array literals (`[...]`), a single element is in
                // list context and should flatten one level (e.g. `[2..6]`,
                // `[@a]`, `[(1,2,3)]`), while multi-element forms keep each
                // element itemized (e.g. `[(1,2),(3,4)]`).
                other if is_real_array && n == 1 => elems.extend(runtime::value_to_list(&other)),
                other => elems.push(other),
            }
        }
        if is_real_array {
            self.stack.push(Value::real_array(elems));
        } else {
            self.stack.push(Value::array(elems));
        }
    }

    pub(super) fn exec_make_hash_op(&mut self, n: u32) {
        let n = n as usize;
        let start = self.stack.len() - n * 2;
        let items: Vec<Value> = self.stack.drain(start..).collect();
        let mut map = HashMap::new();
        for pair in items.chunks(2) {
            let key = pair[0].to_string_value();
            let val = pair[1].clone();
            map.insert(key, val);
        }
        self.stack.push(Value::hash(map));
    }

    pub(super) fn exec_make_capture_op(&mut self, n: u32) {
        let n = n as usize;
        let start = self.stack.len() - n;
        let raw: Vec<Value> = self.stack.drain(start..).collect();
        let mut positional = Vec::new();
        let mut named = HashMap::new();
        for val in raw {
            match val {
                Value::Pair(k, v) => {
                    named.insert(k, *v);
                }
                Value::Capture {
                    positional: p,
                    named: n,
                } => {
                    // Flatten inner capture (from |capture slip)
                    positional.extend(p);
                    named.extend(n);
                }
                Value::Slip(items) => {
                    for item in items.iter() {
                        match item {
                            Value::Pair(k, v) => {
                                named.insert(k.clone(), *v.clone());
                            }
                            other => positional.push(other.clone()),
                        }
                    }
                }
                other => positional.push(other),
            }
        }
        self.stack.push(Value::Capture { positional, named });
    }

    pub(super) fn exec_say_op(&mut self, n: u32) -> Result<(), RuntimeError> {
        let n = n as usize;
        let start = self.stack.len() - n;
        let values: Vec<Value> = self.stack.drain(start..).collect();
        let mut parts = Vec::new();
        for v in &values {
            parts.push(runtime::gist_value(v));
        }
        let line = parts.join("");
        self.interpreter
            .write_to_named_handle("$*OUT", &line, true)?;
        Ok(())
    }

    pub(super) fn exec_note_op(&mut self, n: u32) -> Result<(), RuntimeError> {
        let n = n as usize;
        let content = if n == 0 {
            "Noted".to_string()
        } else {
            let start = self.stack.len() - n;
            let values: Vec<Value> = self.stack.drain(start..).collect();
            let mut parts = Vec::new();
            for v in &values {
                parts.push(self.interpreter.render_gist_value(v));
            }
            parts.join("")
        };
        self.interpreter
            .write_to_named_handle("$*ERR", &content, true)?;
        Ok(())
    }

    pub(super) fn exec_print_op(&mut self, n: u32) -> Result<(), RuntimeError> {
        let n = n as usize;
        let start = self.stack.len() - n;
        let values: Vec<Value> = self.stack.drain(start..).collect();
        let mut content = String::new();
        for v in &values {
            content.push_str(&v.to_string_value());
        }
        self.interpreter
            .write_to_named_handle("$*OUT", &content, false)?;
        Ok(())
    }
}
