use super::*;

impl Interpreter {
    /// Enter `cf`'s compilation unit: record it as the unit currently
    /// executing and hand back the caller's, which every exit path restores.
    ///
    /// This is what makes user-declared operator scoping lexical. `?FILE` is
    /// NOT usable for the same purpose: the env entry tracks the unit being
    /// *loaded*, so inside a module routine called at runtime it still names
    /// the main script. `EVAL` is the exception — it sets both, because an
    /// EVAL unit exists only at runtime (see `Interpreter::user_infix_override`
    /// and `runtime::note_eval_unit_parent`).
    ///
    /// `None` (an AOT-compiled body) means the main script.
    #[inline]
    pub(super) fn enter_compilation_unit(&mut self, cf: &CompiledFunction) -> Symbol {
        let unit = self.unit_of_source(cf.source_file.as_deref());
        std::mem::replace(&mut self.current_unit, unit)
    }

    /// The compilation-unit key for a routine/closure's recorded source file.
    /// `None` (AOT-compiled) and `Some(program_path)` (compiled on the fly from
    /// the running script) are the same unit: the main script.
    #[inline]
    pub(crate) fn unit_of_source(&self, source_file: Option<&str>) -> Symbol {
        match (source_file, self.program_path.as_deref()) {
            (None, _) => crate::runtime::main_unit(),
            (Some(file), Some(prog)) if file == prog => crate::runtime::main_unit(),
            (Some(file), _) => Symbol::intern(file),
        }
    }

    /// Itemize an argument bound to a plain `$`-sigiled parameter. Raku's
    /// signature binder puts the value in a Scalar container, so `f([1,2])`
    /// binds `$v` as `$[1, 2]` — ONE element in list context, `.raku` shows
    /// the `$`. Sigilless (`\v`), `is raw`, and `is rw` parameters bind the
    /// raw value; `@`/`%`/`&` parameters bind the container itself. The
    /// backing Gc is shared — only the container kind flips — so in-place
    /// mutation through the param still reaches the caller's data.
    ///
    /// An *invocant* parameter is exempt: `self` is bound raw, so
    /// `<a b c>.&(method (List:D:) { self.raku })` reports `("a", "b", "c")`,
    /// not the itemized `$("a", "b", "c")`.
    #[inline]
    pub(crate) fn itemize_plain_scalar_param(pd: &crate::ast::ParamDef, val: Value) -> Value {
        if !pd.sigilless
            && !pd.is_invocant
            && !pd.traits.iter().any(|t| t == "invocant")
            && !pd.name.starts_with(['@', '%', '&'])
            && !pd.traits.iter().any(|t| t == "raw" || t == "rw")
        {
            Self::itemize_scalar_store(&pd.name, val)
        } else {
            val
        }
    }

    /// Snapshot the lexical pragma state (`use fatal`, `use strict`,
    /// `use newline`, `use MONKEY-TYPING`) before entering a function body.
    /// Must be paired with `restore_pragma_state` on every exit path to prevent
    /// the callee's pragmas from leaking into the caller's scope.
    ///
    /// In real Raku, pragmas are compile-time and lexically scoped per
    /// compilation unit.  mutsu approximates this at runtime: save on entry,
    /// restore on exit, so `use fatal` inside a sub never outlives that sub.
    #[inline]
    pub(super) fn save_pragma_state(&self) -> (bool, bool, crate::runtime::NewlineMode, bool) {
        (
            self.fatal_mode,
            self.strict_mode,
            self.newline_mode,
            self.monkey_typing,
        )
    }

    /// Restore pragma state saved by `save_pragma_state`.
    #[inline]
    pub(super) fn restore_pragma_state(
        &mut self,
        state: (bool, bool, crate::runtime::NewlineMode, bool),
    ) {
        self.fatal_mode = state.0;
        self.strict_mode = state.1;
        self.newline_mode = state.2;
        self.monkey_typing = state.3;
    }

    /// Enforce a `ContainerRef` cell's registered `of`-type constraint before a
    /// write-through (`$ref = v` on a `:=`-bound typed slot — a typed rw
    /// attribute accessor bind, or a `my T $` anonymous typed scalar). Mirrors
    /// the Pair.value enforcement in `methods_mut_method_lvalue.rs`.
    pub(crate) fn check_container_cell_constraint(
        &mut self,
        cell: &crate::gc::Gc<crate::value::ContainerCell>,
        val: &Value,
    ) -> Result<(), RuntimeError> {
        if let Some(c) = crate::value::lookup_cell_constraint(cell)
            && !matches!(c.ty.as_str(), "Any" | "Mu")
            && !val.is_nil()
            && !self.type_matches_value(&c.ty, val)
        {
            // An ELEMENT's cell blames the container, exactly as a direct
            // `@a[0] = v` store does ("Type check failed for an element of
            // @a"); a plain typed scalar's cell keeps the assignment wording.
            return Err(match c.element_of {
                Some(owner) => {
                    crate::runtime::utils::type_check_element_typed_error(&owner, &c.ty, val)
                }
                // `assign_to` is the name the cell was promoted from, so a write
                // arriving through an alias or from another frame still reads
                // "in assignment to $a" like rakudo's descriptor-carried wording.
                None => RuntimeError::typecheck_assignment(&c.ty, val, c.assign_to.as_deref()),
            });
        }
        Ok(())
    }

    /// Materialize a deferred vivification token's terminal slot into a fresh
    /// shared `ContainerCell` holding `val`, and install it at the slot.
    ///
    /// The cell carries the container's element constraint (ADR-0036 slice 4)
    /// and this first write is checked against it, so a `:=` bind to a slot
    /// that did not exist yet (`my Str @a; my $r := @a[5]; $r = 42`) is
    /// refused exactly like the in-range bind and the direct store are. The
    /// terminal is only written when the check passes.
    pub(crate) fn materialize_entry_cell(
        &mut self,
        terminal: &crate::value::EntryTerminal,
        val: Value,
    ) -> Result<crate::gc::Gc<crate::value::ContainerCell>, RuntimeError> {
        let cell = crate::gc::Gc::new(crate::value::ContainerCell::new(val.clone()));
        if let Some((ty, owner)) = terminal.element_constraint() {
            crate::value::register_element_constraint(&cell, &ty, owner);
            self.check_container_cell_constraint(&cell, &val)?;
        }
        terminal.insert(Value::container_ref(cell.clone()));
        Ok(cell)
    }

    /// Enforce a NAME-keyed scalar's registered `var_type_constraint` before a
    /// write reaches it through the `__mutsu_sigilless_alias::` forward chain
    /// (a sigilless `\x := $a` bind, a sigilless routine parameter aliasing a
    /// caller variable, or a `for LIST -> \x, $value {}` loop-param bind).
    ///
    /// A DIRECT write to a typed scalar's own slot is already checked at its
    /// `SetLocal`/expression-assignment chokepoint (which consults
    /// `var_type_constraint(name)` for that variable's OWN name). But when the
    /// write instead reaches the typed variable *through* a sigilless alias —
    /// `x`'s own name carries no constraint, so that chokepoint sees nothing —
    /// the forward chain walk mirrors the raw value into the alias target's
    /// storage with no check at all
    /// (see `todo/deep/sigilless-alias-assignment-skips-type-constraint.md`).
    /// This closes that gap by re-running the SAME name-keyed constraint
    /// lookup against the alias TARGET's name at the point the value is
    /// mirrored into its storage, so it fires uniformly regardless of which of
    /// mutsu's several alias-propagation call sites performs the write.
    ///
    /// Cheap on the untyped-variable common case: `var_type_constraint` is a
    /// plain name-keyed map lookup, and every caller of this function is
    /// already gated behind a "has any sigilless alias ever been created"
    /// fast-path check, so a program with no sigilless binds pays nothing.
    pub(super) fn check_sigilless_alias_target_constraint(
        &mut self,
        target_name: &str,
        val: &Value,
    ) -> Result<(), RuntimeError> {
        if target_name.starts_with(['@', '%', '&']) {
            return Ok(());
        }
        let Some(constraint) = loan_env!(self, var_type_constraint(target_name)) else {
            return Ok(());
        };
        if matches!(constraint.as_str(), "Any" | "Mu") {
            return Ok(());
        }
        // A bound alias may itself hold a `ContainerRef` cell (a further bind
        // layered on top); the constraint applies to the CONTAINED value.
        let check_val = val.deref_container();
        if check_val.is_nil() {
            return Ok(());
        }
        if !self.type_matches_value(&constraint, &check_val) {
            return Err(crate::runtime::utils::type_check_assignment_typed_error(
                target_name,
                &constraint,
                &check_val,
            ));
        }
        Ok(())
    }

    /// Slice 2a: clear the aggregate held inside a shared `ContainerRef` cell
    /// (`undefine @ary` where `my $r = @ary` promoted `@ary` to a cell). Uses
    /// `Arc::make_mut` so a copy taken out of the cell (`my @copy = @ary`) is
    /// detached rather than emptied; every alias of the cell observes the clear.
    pub(super) fn clear_aggregate_cell(cell: &crate::gc::Gc<crate::value::ContainerCell>) {
        let mut guard = cell.lock().unwrap();
        if (*guard)
            .with_array_mut(|arc, _| crate::gc::Gc::make_mut(arc).items_mut().clear())
            .is_none()
            && (*guard)
                .with_hash_mut(|arc| crate::gc::Gc::make_mut(arc).map.clear())
                .is_none()
        {
            *guard = Value::NIL;
        }
    }

    /// Does the bottom of `stack` already account for the mainline `<unit>`,
    /// so that no synthetic `<unit>` frame should be appended beneath it?
    ///
    /// Two independent reasons it can:
    ///
    /// 1. The outermost frame *is* the mainline boundary — the synthetic
    ///    `<unit>` frame, or an empty-named non-block frame. (A genuine
    ///    bare-block callframe is empty-named but `is_block`, and the
    ///    mainline `<unit>` really does sit below it.)
    /// 2. This interpreter is a **thread clone**. `clone_for_thread` starts
    ///    the worker with an empty `routine_stack`, so the bottom frame is
    ///    the thread's entry block and there is no mainline `<unit>` under
    ///    it at all. Appending one there duplicated the entry block's own
    ///    line: `Promise.start({ die ... }).cause` rendered
    ///    `in block <unit> at f line 1` twice, once for the `<pointy-block>`
    ///    frame and once for the phantom unit frame synthesized beneath it.
    fn stack_bottom_is_mainline_unit(&self, stack: &[crate::runtime::RoutineFrame]) -> bool {
        if self.is_thread_clone() {
            return true;
        }
        stack
            .first()
            .is_some_and(|f| f.name == "<unit>" || (f.name.is_empty() && !f.is_block))
    }

    /// A callback run by a thread clone can leave only an anonymous block at
    /// the bottom of its stack. That frame is deliberately omitted from concise
    /// backtraces, so retain the source location of the spawn as one synthetic
    /// bottom frame. A named entry block already renders its own location and
    /// must not receive another frame (the Promise.start duplicate-frame case).
    fn thread_origin_frame(&self, stack: &[crate::runtime::RoutineFrame]) -> Option<(String, u32)> {
        let bottom_is_anon_block = stack
            .first()
            .is_some_and(|frame| frame.is_block && frame.name.is_empty());
        (self.is_thread_clone() && bottom_is_anon_block)
            .then_some(self.thread_spawn_origin.as_ref())
            .flatten()
            .map(|(file, line)| (file.resolve(), *line))
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
    pub(crate) fn build_backtrace_string(&self) -> String {
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
                (inner_frame.line, inner_frame.file.map(|s| s.resolve()))
            };
            // A routine defined in another file (a `use`d module) displays at
            // its defining file; the call-site line is within that file.
            let file = frame.def_file.map(|s| s.resolve()).or(file);
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
        } else if let Some((file, line)) = self.thread_origin_frame(stack) {
            let location = Self::format_location(Some(&file), Some(line));
            lines.push(format!("  in block <unit>{}", location));
        } else if !self.stack_bottom_is_mainline_unit(stack) {
            // The outermost routine frame's stored call-site is where
            // <unit> called it.
            let outermost = &stack[0];
            let location =
                Self::format_location(outermost.file.map(|s| s.as_str()), outermost.line);
            lines.push(format!("  in block <unit>{}", location));
        }
        lines.join("\n")
    }

    /// Build a structured Backtrace Value from the interpreter's routine stack.
    /// Returns a `Backtrace` instance whose `frames` attribute is a list of
    /// `Backtrace::Frame` instances (each with `.subname`, `.file`, `.line`)
    /// and whose `text` attribute is the formatted backtrace string.
    pub(crate) fn build_backtrace_value(&self) -> Value {
        self.build_backtrace_value_with_leading(&[])
    }

    /// [`Self::build_backtrace_value`] with an explicit `is-runtime` stamp.
    /// Only a *compile-time* diagnosis passes `false`: rakudo's
    /// `Backtrace.is-runtime` distinguishes a backtrace captured while the
    /// program was running from one describing a compilation failure, and the
    /// live routine stack of the code that triggered the compilation is the
    /// best frame set mutsu can offer for the latter.
    pub(crate) fn build_backtrace_value_with_runtime(&self, is_runtime: bool) -> Value {
        let bt = self.build_backtrace_value_with_leading(&[]);
        Self::stamp_backtrace_runtime(bt, is_runtime)
    }

    /// Overwrite a freshly built `Backtrace`'s `is-runtime` attribute.
    fn stamp_backtrace_runtime(bt: Value, is_runtime: bool) -> Value {
        if let ValueView::Instance { attributes, .. } = bt.view() {
            attributes.insert("is-runtime".to_string(), Value::truth(is_runtime));
        }
        bt
    }

    /// Build a `Backtrace` value from the current routine stack, optionally
    /// prepending synthetic leading routine frames (e.g. `throw` and `die`).
    ///
    /// An explicit `ExceptionObject.throw` is dispatched natively, so the
    /// `throw` invocation never appears as its own callframe on the routine
    /// stack. Raku, by contrast, includes the `Exception.throw` setting frame at
    /// the top of `.backtrace().list` (it is hidden from the rendered gist as a
    /// setting frame, but still counts toward `.list.elems`). Passing the method
    /// names here reproduces those extra structured-only frames.
    pub(super) fn build_backtrace_value_with_leading(&self, leading: &[&str]) -> Value {
        use crate::symbol::Symbol;
        use std::collections::HashMap;

        let stack = self.routine_stack();
        let current_line = self.current_source_line();
        let current_file = self.current_source_file();
        let reversed: Vec<_> = stack.iter().rev().collect();

        let mut frames = Vec::new();
        let mut text_lines = Vec::new();

        // Synthetic leading frame (setting `throw`/`rethrow`): structured-only,
        // omitted from the rendered text just like Raku hides setting frames.
        for name in leading {
            let mut frame_attrs = HashMap::new();
            frame_attrs.insert("subname".to_string(), Value::str((*name).to_string()));
            frame_attrs.insert("is-setting".to_string(), Value::TRUE);
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
                    .map(|l| Value::int(l as i64))
                    .unwrap_or(Value::int(0)),
            );
            frames.push(Value::make_instance(
                Symbol::intern("Backtrace::Frame"),
                frame_attrs,
            ));
        }

        for (i, frame) in reversed.iter().enumerate() {
            let (line, file) = if i == 0 {
                (current_line, current_file.clone())
            } else {
                let inner_frame = reversed[i - 1];
                (inner_frame.line, inner_frame.file.map(|s| s.resolve()))
            };
            // Module routines display at their defining file (see
            // `build_backtrace_string`).
            let file = frame.def_file.map(|s| s.resolve()).or(file);
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
                frame.name.resolve()
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
                line.map(|l| Value::int(l as i64)).unwrap_or(Value::int(0)),
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
                    .map(|l| Value::int(l as i64))
                    .unwrap_or(Value::int(0)),
            );
            frames.push(Value::make_instance(
                Symbol::intern("Backtrace::Frame"),
                frame_attrs,
            ));
        } else if let Some((file, line)) = self.thread_origin_frame(stack) {
            let location = Self::format_location(Some(&file), Some(line));
            text_lines.push(format!("  in block <unit>{}", location));

            let mut frame_attrs = HashMap::new();
            frame_attrs.insert("subname".to_string(), Value::str("<unit>".to_string()));
            frame_attrs.insert("file".to_string(), Value::str(file));
            frame_attrs.insert("line".to_string(), Value::int(line as i64));
            frames.push(Value::make_instance(
                Symbol::intern("Backtrace::Frame"),
                frame_attrs,
            ));
        } else if !self.stack_bottom_is_mainline_unit(stack) {
            let outermost = &stack[0];
            // If every live frame is an anonymous block (not rendered above),
            // the synthetic unit line is the only visible location and must
            // retain the actual throw site rather than the block-entry line.
            let only_anonymous_blocks = stack
                .iter()
                .all(|frame| frame.is_block && frame.name.is_empty());
            let location = if only_anonymous_blocks {
                Self::format_location(current_file.as_deref(), current_line)
            } else {
                Self::format_location(outermost.file.map(|s| s.as_str()), outermost.line)
            };
            text_lines.push(format!("  in block <unit>{}", location));

            let mut frame_attrs = HashMap::new();
            frame_attrs.insert("subname".to_string(), Value::str("<unit>".to_string()));
            frame_attrs.insert(
                "file".to_string(),
                outermost
                    .file
                    .map(|s| Value::str(s.resolve()))
                    .unwrap_or(Value::str(String::new())),
            );
            frame_attrs.insert(
                "line".to_string(),
                outermost
                    .line
                    .map(|l| Value::int(l as i64))
                    .unwrap_or(Value::int(0)),
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
        // Built from the live call stack, so this is a RUNTIME backtrace --
        // what rakudo's `Backtrace.is-runtime` reports True for. A compile-time
        // diagnosis never reaches either of these builders, so its backtrace
        // (synthesized from the error's file/line metadata) answers False.
        bt_attrs.insert("is-runtime".to_string(), Value::TRUE);
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
    /// `is_runtime` stamps the result — see
    /// [`Self::build_backtrace_value_with_runtime`].
    pub(super) fn backtrace_value_from_string_with_runtime(
        bt_str: &str,
        is_runtime: bool,
    ) -> Value {
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
            frame_attrs.insert("line".to_string(), Value::int(line_no));
            frames.push(Value::make_instance(
                Symbol::intern("Backtrace::Frame"),
                frame_attrs,
            ));
        }

        let mut bt_attrs = HashMap::new();
        bt_attrs.insert("frames".to_string(), Value::array(frames));
        bt_attrs.insert("text".to_string(), Value::str(bt_str.to_string()));
        // Parsed from a captured backtrace string -- runtime unless the caller
        // says otherwise (a compile-time diagnosis; see the sibling builder).
        bt_attrs.insert("is-runtime".to_string(), Value::truth(is_runtime));
        Value::make_instance(Symbol::intern("Backtrace"), bt_attrs)
    }

    /// Attach the current call-stack backtrace (string form on the error, and
    /// structured `Backtrace` + line/file attributes on the exception instance
    /// if any) to a runtime error that does not carry one yet. `die`/`fail`
    /// build theirs at the throw site; this generalizes the same information to
    /// every other runtime error (method-not-found, type-check, ...) so CLI
    /// output and `$!.backtrace` report the failing line for all of them.
    pub(super) fn attach_backtrace_to_error(&self, err: &mut RuntimeError) {
        self.attach_backtrace_to_error_with_leading(err, &[]);
    }

    /// [`Self::attach_backtrace_to_error`] with native setting routines that
    /// participated in raising the exception but have no VM callframes.
    pub(super) fn attach_backtrace_to_error_with_leading(
        &self,
        err: &mut RuntimeError,
        leading: &[&str],
    ) {
        if err.backtrace().is_none() {
            let backtrace_str = self.build_backtrace_string();
            // An error raised by USING an unhandled Failure renders rakudo's
            // dual-backtrace form: the fail-site backtrace (carried from the
            // Failure's exception) plus where it was actually thrown.
            if let Some(orig) = err.failure_original_backtrace().map(str::to_string) {
                err.set_backtrace(Some(format!(
                    "{orig}\n\nActually thrown at:\n{backtrace_str}"
                )));
            } else if !backtrace_str.is_empty() {
                err.set_backtrace(Some(backtrace_str));
            }
        }
        if let Some(ref exc_box) = err.exception
            && let ValueView::Instance { attributes, .. } = exc_box.view()
            && !attributes.as_map().contains_key("backtrace")
        {
            attributes.insert(
                "backtrace".to_string(),
                self.build_backtrace_value_with_leading(leading),
            );
            if let Some(line) = self.current_source_line() {
                attributes.insert_if_absent("line".to_string(), Value::int(line as i64));
            }
            if let Some(file) = self.current_source_file() {
                attributes.insert_if_absent("file".to_string(), Value::str_from(&file));
            }
        }
    }
}
