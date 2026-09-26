//! Backtraces: capturing the call stack at a throw, and rendering it.
//!
//! A capture ([`BacktraceCapture`]) is an immutable snapshot of the routine
//! stack plus the few interpreter facts its rendering depends on. Taking one
//! is O(1) amortized ([`crate::runtime::routine_stack::RoutineStack::snapshot`]
//! shares the stack prefix earlier captures already copied), and rendering it
//! -- the text and the structured `Backtrace` -- is deferred until something
//! reads it. So a `die` inside a `try` that never asks for `.backtrace` no
//! longer pays for the whole stack (#9172); Rakudo's backtraces are lazy too.

use crate::runtime::routine_stack::FrameNode;
use crate::runtime::{Interpreter, RoutineFrame};
use crate::value::{Value, ValueView};
use std::sync::Arc;

/// The call stack at the moment of a throw, with everything its rendering
/// needs, so it can be rendered any time later.
#[derive(Debug)]
pub(crate) struct BacktraceCapture {
    /// The innermost live frame; `None` for an empty routine stack.
    top: Option<Arc<FrameNode>>,
    current_line: Option<u32>,
    current_file: Option<String>,
    /// See `Interpreter::executing_source_file`.
    executing_file: Option<String>,
    /// See `Interpreter::thread_origin_frame`.
    thread_origin: Option<(String, u32)>,
    /// See `Interpreter::stack_bottom_is_mainline_unit`.
    bottom_is_mainline: bool,
}

impl Interpreter {
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

    /// Capture the current call stack for a backtrace rendered later.
    // Cost: O(1) amortized (the stack snapshot shares its prefix with earlier
    // ones), plus O(b), b = anonymous block frames on top of the stack
    // (`executing_source_file`).
    pub(crate) fn capture_backtrace(&self) -> Arc<BacktraceCapture> {
        let stack = self.routine_stack();
        Arc::new(BacktraceCapture {
            top: self.routine_stack_snapshot(),
            current_line: self.current_source_line(),
            current_file: self.current_source_file(),
            executing_file: self.executing_source_file(),
            thread_origin: self.thread_origin_frame(stack),
            bottom_is_mainline: self.stack_bottom_is_mainline_unit(stack),
        })
    }

    /// Build a backtrace string from the interpreter's routine stack (see
    /// [`BacktraceCapture::render_text`]).
    // Cost: O(s), s = routine-stack depth.
    pub(crate) fn build_backtrace_string(&self) -> String {
        self.capture_backtrace().render_text()
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

    /// Build a `Backtrace` value from the current routine stack (see
    /// [`BacktraceCapture::render_value`]).
    // Cost: O(s), s = routine-stack depth.
    pub(crate) fn build_backtrace_value_with_leading(&self, leading: &[&str]) -> Value {
        self.capture_backtrace().render_value(leading)
    }
}

impl BacktraceCapture {
    /// Build a backtrace string from the interpreter's routine stack.
    /// Each frame is formatted as `  in sub <name> at <file> line <N>`.
    ///
    /// Each pushed frame stores the call-site (the line/file in the *caller*
    /// where this function was invoked).  To display "where each frame was
    /// executing when it called the next", we shift by one:
    ///   - innermost frame (i=0): use current ?LINE/?FILE (the die/error line)
    ///   - frame i>0: use the *next inner* frame's stored call-site
    ///     (i.e. `frame[i]`'s displayed line = the line where `frame[i]` called
    ///     frame[i-1])
    ///   - `<unit>` (outermost): use the outermost routine frame's stored
    ///     call-site (where `<unit>` called the first function)
    pub(crate) fn render_text(&self) -> String {
        let stack_frames = FrameNode::frames(self.top.as_ref());
        let stack: &[RoutineFrame] = &stack_frames;
        let current_line = self.current_line;
        let current_file = self.current_file.clone();
        // The innermost frame's OWN file — see the matching comment on
        // `build_backtrace_value_with_leading` (#8743): `current_file`'s
        // dynamically-scoped `?FILE` had already reverted to the importer's
        // path by the time a `use`d module's own def_file-less frame ran.
        let executing_file = self.executing_file.clone();
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
                // Innermost frame: use current ?LINE, and the frame's own
                // lexical file for ?FILE.
                (current_line, executing_file.clone())
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
        } else if let Some((file, line)) = self.thread_origin.clone() {
            let location = Self::format_location(Some(&file), Some(line));
            lines.push(format!("  in block <unit>{}", location));
        } else if !self.bottom_is_mainline {
            // The outermost routine frame's stored call-site is where
            // <unit> called it.
            let outermost = &stack[0];
            let location =
                Self::format_location(outermost.file.map(|s| s.as_str()), outermost.line);
            lines.push(format!("  in block <unit>{}", location));
        }
        lines.join("\n")
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
    pub(crate) fn render_value(&self, leading: &[&str]) -> Value {
        let mut bt_attrs = std::collections::HashMap::new();
        for (key, value) in self.render_attrs(leading) {
            bt_attrs.insert(key.to_string(), value);
        }
        Value::make_instance(crate::symbol::Symbol::intern("Backtrace"), bt_attrs)
    }

    /// The attributes of the `Backtrace` [`Self::render_value`] builds.
    // Cost: O(s), s = the captured stack's depth.
    pub(super) fn render_attrs(&self, leading: &[&str]) -> Vec<(&'static str, Value)> {
        use crate::symbol::Symbol;
        use std::collections::HashMap;

        let stack_frames = FrameNode::frames(self.top.as_ref());
        let stack: &[RoutineFrame] = &stack_frames;
        let current_line = self.current_line;
        let current_file = self.current_file.clone();
        // The innermost frame's OWN file — not `current_file`'s dynamically-
        // scoped `?FILE`, which had already reverted to the importer's path by
        // the time a `use`d module's own bare block (an inlined `{ ... }`/
        // `try { ... }`, `def_file: None`) ran (#8743). `executing_source_file`
        // resolves it the same way `def_file.or(file)` below does for every
        // OTHER frame in the walk, just starting from the frame this loop has
        // not reached yet.
        let executing_file = self.executing_file.clone();
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
                (current_line, executing_file.clone())
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
                "is-hidden".to_string(),
                Value::truth(frame.is_hidden_from_backtrace),
            );
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
        } else if let Some((file, line)) = self.thread_origin.clone() {
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
        } else if !self.bottom_is_mainline {
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
        // Built from the live call stack, so this is a RUNTIME backtrace --
        // what rakudo's `Backtrace.is-runtime` reports True for. A compile-time
        // diagnosis never reaches either of these builders, so its backtrace
        // (synthesized from the error's file/line metadata) answers False.
        vec![
            ("frames", Value::array(frames)),
            ("text", Value::str(text)),
            ("is-runtime", Value::TRUE),
        ]
    }

    /// Format a `" at <file> line <N>"` suffix for backtrace entries.
    fn format_location(file: Option<&str>, line: Option<u32>) -> String {
        match (file, line) {
            (Some(f), Some(l)) => format!(" at {} line {}", f, l),
            (Some(f), None) => format!(" at {}", f),
            (None, Some(l)) => format!(" at line {}", l),
            (None, None) => String::new(),
        }
    }
}
