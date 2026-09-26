//! Attaching a lazily rendered backtrace to an error: the capture is taken at
//! the throw ([`Interpreter::capture_backtrace`]) and rendered, as text and as
//! a `Backtrace` object, only when read (#9172).

use super::vm_backtrace::BacktraceCapture;
use crate::runtime::Interpreter;
use crate::value::{Value, ValueView};
use std::sync::Arc;

/// A capture's text, rendered when an error's backtrace string is first read.
#[derive(Debug)]
struct LazyBacktraceString {
    capture: Arc<BacktraceCapture>,
    /// For an error raised by using an unhandled Failure: the backtrace of the
    /// `fail`, rendered in rakudo's dual form above this one.
    failure_origin: Option<String>,
}

impl crate::value::LazyBacktraceText for LazyBacktraceString {
    // Cost: O(s), s = the captured stack's depth.
    fn render(&self) -> String {
        let text = self.capture.render_text();
        match &self.failure_origin {
            Some(orig) => format!("{orig}\n\nActually thrown at:\n{text}"),
            None => text,
        }
    }
}

/// A capture's `Backtrace` attributes, materialized when the exception's
/// `Backtrace` object is first read.
#[derive(Debug)]
struct LazyBacktraceAttrs {
    capture: Arc<BacktraceCapture>,
    leading: Vec<&'static str>,
}

impl crate::value::lazy_attrs::LazyAttrSource for LazyBacktraceAttrs {
    // Cost: O(s), s = the captured stack's depth.
    fn materialize(&self) -> Vec<(&'static str, Value)> {
        self.capture.render_attrs(&self.leading)
    }
}

impl Interpreter {
    /// Attach the current call-stack backtrace (the string form on the error,
    /// and a structured `Backtrace` plus line/file attributes on the exception
    /// instance, if any) to a runtime error that does not carry one yet, with
    /// `leading` native setting routines (`throw`, `die`) that raised it but
    /// have no VM callframes. Both backtraces are captured now and rendered only
    /// when read.
    // Cost: O(1) amortized (see `capture_backtrace`); rendering is deferred.
    pub(super) fn attach_lazy_backtrace_to_error(
        &self,
        err: &mut crate::value::RuntimeError,
        leading: &[&'static str],
    ) {
        let wants_text = !err.has_backtrace();
        let wants_value = err.exception.as_ref().is_some_and(|exc| {
            matches!(exc.view(), ValueView::Instance { attributes, .. }
                if !attributes.as_map().contains_key("backtrace"))
        });
        if !wants_text && !wants_value {
            return;
        }
        let capture = self.capture_backtrace();
        if wants_text {
            let failure_origin = err.failure_original_backtrace().map(str::to_string);
            err.set_backtrace_lazy(Arc::new(LazyBacktraceString {
                capture: Arc::clone(&capture),
                failure_origin,
            }));
        }
        if wants_value
            && let Some(ref exc_box) = err.exception
            && let ValueView::Instance { attributes, .. } = exc_box.view()
        {
            let backtrace = Value::make_instance_lazy(
                crate::symbol::Symbol::intern("Backtrace"),
                std::collections::HashMap::new(),
                Arc::new(LazyBacktraceAttrs {
                    capture,
                    leading: leading.to_vec(),
                }),
            );
            attributes.insert("backtrace".to_string(), backtrace);
            if let Some(line) = self.current_source_line() {
                attributes.insert_if_absent("line".to_string(), Value::int(line as i64));
            }
            if let Some(file) = self.current_source_file() {
                attributes.insert_if_absent("file".to_string(), Value::str_from(&file));
            }
        }
    }
}
