use super::*;

impl Interpreter {
    /// Slice 2a: clear the aggregate held inside a shared `ContainerRef` cell
    /// (`undefine @ary` where `my $r = @ary` promoted `@ary` to a cell). Uses
    /// `Arc::make_mut` so a copy taken out of the cell (`my @copy = @ary`) is
    /// detached rather than emptied; every alias of the cell observes the clear.
    pub(super) fn clear_aggregate_cell(cell: &std::sync::Arc<std::sync::Mutex<Value>>) {
        let mut guard = cell.lock().unwrap();
        match &mut *guard {
            Value::Array(arc, _) => std::sync::Arc::make_mut(arc).items.clear(),
            Value::Hash(arc) => std::sync::Arc::make_mut(arc).map.clear(),
            other => *other = Value::Nil,
        }
    }

    /// Build a backtrace string from the interpreter's routine stack.
    /// Each frame is formatted as "  in sub <name> at <file> line <N>".
    ///
    /// Each pushed frame stores the call-site (the line/file in the *caller*
    /// where this function was invoked).  To display "where each frame was
    /// executing when it called the next", we shift by one:
    ///   - innermost frame (i=0): use current ?LINE/?FILE (the die/error line)
    ///   - frame i>0: use the *next inner* frame's stored call-site
    ///     (i.e. frame[i]'s displayed line = the line where frame[i] called
    ///     frame[i-1])
    ///   - <unit> (outermost): use the outermost routine frame's stored
    ///     call-site (where <unit> called the first function)
    pub(super) fn build_backtrace_string(&self) -> String {
        let stack = self.routine_stack();
        let current_line = self.current_source_line();
        let current_file = self.current_source_file();
        // Build reversed list: stack[last] is innermost, stack[0] is outermost
        let reversed: Vec<_> = stack.iter().rev().collect();
        let mut lines = Vec::new();
        for (i, frame) in reversed.iter().enumerate() {
            // A genuine bare-block callframe (empty-named `is_block`) is omitted
            // from this concise rendering — the enclosing `<unit>` line covers it
            // (matching Raku's `.nice`). It still appears in the structured
            // `.list` built by `build_backtrace_value`.
            if frame.is_block && frame.name.is_empty() {
                continue;
            }
            let (line, file) = if i == 0 {
                // Innermost frame: use current ?LINE/?FILE
                (current_line, current_file.clone())
            } else {
                // Outer frame: the line where this frame called the next inner frame.
                // That info is stored in the next-inner frame's call-site.
                let inner_frame = reversed[i - 1];
                (inner_frame.line, inner_frame.file.clone())
            };
            let location = Self::format_location(file.as_deref(), line);
            if frame.name.is_empty() || frame.name == "<unit>" || frame.name == "<pointy-block>" {
                lines.push(format!("  in block <unit>{}", location));
            } else {
                lines.push(format!("  in sub {}{}", frame.name, location));
            }
        }
        // Add the <unit> frame at the bottom
        if stack.is_empty() {
            let location = Self::format_location(current_file.as_deref(), current_line);
            lines.push(format!("  in block <unit>{}", location));
        } else if !stack.first().is_some_and(|f| {
            // A genuine bare-block frame is empty-named but `is_block`; the
            // mainline `<unit>` still sits below it (see `build_backtrace_value`).
            f.name == "<unit>" || (f.name.is_empty() && !f.is_block)
        }) {
            // The outermost routine frame's stored call-site is where
            // <unit> called it.
            let outermost = &stack[0];
            let location = Self::format_location(outermost.file.as_deref(), outermost.line);
            lines.push(format!("  in block <unit>{}", location));
        }
        lines.join("\n")
    }

    /// Build a structured Backtrace Value from the interpreter's routine stack.
    /// Returns a `Backtrace` instance whose `frames` attribute is a list of
    /// `Backtrace::Frame` instances (each with `.subname`, `.file`, `.line`)
    /// and whose `text` attribute is the formatted backtrace string.
    pub(super) fn build_backtrace_value(&self) -> Value {
        use crate::symbol::Symbol;
        use std::collections::HashMap;

        let stack = self.routine_stack();
        let current_line = self.current_source_line();
        let current_file = self.current_source_file();
        let reversed: Vec<_> = stack.iter().rev().collect();

        let mut frames = Vec::new();
        let mut text_lines = Vec::new();

        for (i, frame) in reversed.iter().enumerate() {
            let (line, file) = if i == 0 {
                (current_line, current_file.clone())
            } else {
                let inner_frame = reversed[i - 1];
                (inner_frame.line, inner_frame.file.clone())
            };
            // A genuine bare-block callframe (is_block + empty name) is an
            // anonymous block in Raku: its `.subname` is the empty string (so
            // `.is-routine` is False and `.code.name` is empty), distinct from
            // the synthetic `<unit>` bottom frame.
            let is_anon_block = frame.is_block && frame.name.is_empty();
            let subname = if is_anon_block {
                String::new()
            } else if frame.name.is_empty()
                || frame.name == "<unit>"
                || frame.name == "<pointy-block>"
            {
                "<unit>".to_string()
            } else {
                frame.name.clone()
            };

            let location = Self::format_location(file.as_deref(), line);
            // The rendered text (`.Str`/gist) is a concise view: like Raku's
            // `.nice`, it omits the anonymous bare-block line (the enclosing
            // `<unit>` line already covers it). The block still appears in the
            // structured `frames` below (so `.list`/`.elems` count it).
            if !is_anon_block {
                if subname == "<unit>" {
                    text_lines.push(format!("  in block <unit>{}", location));
                } else {
                    text_lines.push(format!("  in sub {}{}", subname, location));
                }
            }

            let mut frame_attrs = HashMap::new();
            frame_attrs.insert("subname".to_string(), Value::str(subname));
            frame_attrs.insert(
                "file".to_string(),
                file.map(Value::str).unwrap_or(Value::str(String::new())),
            );
            frame_attrs.insert(
                "line".to_string(),
                line.map(|l| Value::Int(l as i64)).unwrap_or(Value::Int(0)),
            );
            frames.push(Value::make_instance(
                Symbol::intern("Backtrace::Frame"),
                frame_attrs,
            ));
        }

        // Add <unit> frame at bottom if needed
        if stack.is_empty() {
            let location = Self::format_location(current_file.as_deref(), current_line);
            text_lines.push(format!("  in block <unit>{}", location));

            let mut frame_attrs = HashMap::new();
            frame_attrs.insert("subname".to_string(), Value::str("<unit>".to_string()));
            frame_attrs.insert(
                "file".to_string(),
                current_file
                    .clone()
                    .map(Value::str)
                    .unwrap_or(Value::str(String::new())),
            );
            frame_attrs.insert(
                "line".to_string(),
                current_line
                    .map(|l| Value::Int(l as i64))
                    .unwrap_or(Value::Int(0)),
            );
            frames.push(Value::make_instance(
                Symbol::intern("Backtrace::Frame"),
                frame_attrs,
            ));
        } else if !stack.first().is_some_and(|f| {
            // The outermost frame already *is* the mainline boundary (so no extra
            // `<unit>` is appended) only when it is the synthetic `<unit>`/pointy
            // frame, or an empty-named *non-block* frame. A genuine bare-block
            // callframe is empty-named but `is_block`, and the mainline `<unit>`
            // still sits below it.
            f.name == "<unit>" || (f.name.is_empty() && !f.is_block)
        }) {
            let outermost = &stack[0];
            let location = Self::format_location(outermost.file.as_deref(), outermost.line);
            text_lines.push(format!("  in block <unit>{}", location));

            let mut frame_attrs = HashMap::new();
            frame_attrs.insert("subname".to_string(), Value::str("<unit>".to_string()));
            frame_attrs.insert(
                "file".to_string(),
                outermost
                    .file
                    .clone()
                    .map(Value::str)
                    .unwrap_or(Value::str(String::new())),
            );
            frame_attrs.insert(
                "line".to_string(),
                outermost
                    .line
                    .map(|l| Value::Int(l as i64))
                    .unwrap_or(Value::Int(0)),
            );
            frames.push(Value::make_instance(
                Symbol::intern("Backtrace::Frame"),
                frame_attrs,
            ));
        }

        let text = text_lines.join("\n");
        let mut bt_attrs = HashMap::new();
        bt_attrs.insert("frames".to_string(), Value::array(frames));
        bt_attrs.insert("text".to_string(), Value::str(text));
        Value::make_instance(Symbol::intern("Backtrace"), bt_attrs)
    }

    /// Format a " at <file> line <N>" suffix for backtrace entries.
    fn format_location(file: Option<&str>, line: Option<u32>) -> String {
        match (file, line) {
            (Some(f), Some(l)) => format!(" at {} line {}", f, l),
            (Some(f), None) => format!(" at {}", f),
            (None, Some(l)) => format!(" at line {}", l),
            (None, None) => String::new(),
        }
    }

    /// Build a Backtrace Value from a pre-formatted backtrace string.
    /// Parses the string lines to extract frame info (best-effort).
    pub(super) fn backtrace_value_from_string(bt_str: &str) -> Value {
        use crate::symbol::Symbol;
        use std::collections::HashMap;

        let mut frames = Vec::new();
        for line in bt_str.lines() {
            let trimmed = line.trim();
            // Parse lines like "  in sub foo at file.raku line 5"
            // or "  in block <unit> at -e line 1"
            let subname;
            let rest;
            if let Some(after_sub) = trimmed.strip_prefix("in sub ") {
                if let Some(at_pos) = after_sub.find(" at ") {
                    subname = after_sub[..at_pos].to_string();
                    rest = &after_sub[at_pos..];
                } else {
                    subname = after_sub.to_string();
                    rest = "";
                }
            } else if let Some(after_block) = trimmed.strip_prefix("in block ") {
                if let Some(at_pos) = after_block.find(" at ") {
                    subname = after_block[..at_pos].to_string();
                    rest = &after_block[at_pos..];
                } else {
                    subname = after_block.to_string();
                    rest = "";
                }
            } else {
                continue;
            }

            let mut file = String::new();
            let mut line_no: i64 = 0;
            if let Some(at_rest) = rest.strip_prefix(" at ") {
                if let Some(line_pos) = at_rest.rfind(" line ") {
                    file = at_rest[..line_pos].to_string();
                    if let Ok(n) = at_rest[line_pos + 6..].parse::<i64>() {
                        line_no = n;
                    }
                } else {
                    file = at_rest.to_string();
                }
            }

            let mut frame_attrs = HashMap::new();
            frame_attrs.insert("subname".to_string(), Value::str(subname));
            frame_attrs.insert("file".to_string(), Value::str(file));
            frame_attrs.insert("line".to_string(), Value::Int(line_no));
            frames.push(Value::make_instance(
                Symbol::intern("Backtrace::Frame"),
                frame_attrs,
            ));
        }

        let mut bt_attrs = HashMap::new();
        bt_attrs.insert("frames".to_string(), Value::array(frames));
        bt_attrs.insert("text".to_string(), Value::str(bt_str.to_string()));
        Value::make_instance(Symbol::intern("Backtrace"), bt_attrs)
    }
}
