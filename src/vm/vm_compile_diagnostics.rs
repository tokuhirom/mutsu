//! Reporting what `$*CU.add-sorry` / `add-worry` queued once a unit's compile
//! phase ends (#15666; the queue is `crate::unit_diagnostics`).

use super::*;

impl Interpreter {
    /// A lone sorry is thrown as itself, so its own `.gist` is the
    /// `===SORRY!=== Error while compiling FILE` report. Several sorries, or a
    /// sorry next to worries, are bundled the way rakudo's `X::Comp::Group`
    /// renders them. Worries alone are printed to stderr and the unit runs on.
    // Cost: O(d * g), d = queued diagnostics, g = the cost of one exception's `.gist`.
    pub(crate) fn report_compile_diagnostics(
        &mut self,
        sorries: Vec<Value>,
        worries: Vec<Value>,
    ) -> Result<(), RuntimeError> {
        if sorries.is_empty() && worries.is_empty() {
            return Ok(());
        }
        if sorries.len() == 1 && worries.is_empty() && !is_parse_sorry(&sorries[0]) {
            let only = sorries.into_iter().next().unwrap_or(Value::NIL);
            return Err(RuntimeError::from_exception_value(only));
        }
        let mut worry_text = String::new();
        for worry in &worries {
            if !worry_text.is_empty() {
                worry_text.push('\n');
            }
            let body = self.diagnostic_body(worry)?;
            worry_text.push_str(&indent_lines(&body));
        }
        if sorries.is_empty() {
            self.write_warn_to_stderr(&format!("Potential difficulties:\n{worry_text}"));
            return Ok(());
        }
        // A lone sorry carries its file in the heading, as its own `.gist` does.
        let lone_parse_sorry = sorries.len() == 1 && worries.is_empty();
        let mut text = match (lone_parse_sorry, sorries.first().and_then(sorry_file)) {
            (true, Some(file)) => format!("===SORRY!=== Error while compiling {file}\n"),
            _ => String::from("===SORRY!===\n"),
        };
        let bodies = sorries
            .iter()
            .map(|sorry| self.diagnostic_body(sorry))
            .collect::<Result<Vec<_>, _>>()?;
        text.push_str(&bodies.join("\n"));
        if !worries.is_empty() {
            text.push_str(&format!("\nOther potential difficulties:\n{worry_text}"));
        }
        if !lone_parse_sorry {
            text.push('\n');
        }
        let mut attrs = std::collections::HashMap::new();
        attrs.insert("message".to_string(), Value::str(text));
        // A compile-time report has no frames to show.
        attrs.insert("backtrace".to_string(), Value::NIL);
        let group = Value::make_instance(Symbol::intern("X::Comp::Group"), attrs);
        Err(RuntimeError::from_exception_value(group))
    }

    /// `$ex.gist(:!sorry)`: the report of one diagnostic without the
    /// `===SORRY!===` heading, as the full `.gist` minus `.sorry_heading`.
    // Cost: O(g), g = the cost of the exception's `.gist`.
    fn diagnostic_body(&mut self, exception: &Value) -> Result<String, RuntimeError> {
        if is_parse_sorry(exception) {
            return Ok(parse_sorry_body(exception));
        }
        let full = self.render_gist_value(exception)?;
        // `X::Comp.sorry_heading`, from the file name the exception carries.
        let filename = match exception.view() {
            ValueView::Instance { attributes, .. } => attributes
                .as_map()
                .get("filename")
                .map(|file| file.deref_container())
                .filter(|file| matches!(file.view(), ValueView::Str(_)))
                .map(|file| file.to_string_value()),
            _ => None,
        };
        let heading = match filename {
            Some(file) => format!("===SORRY!=== Error while compiling {file}\n"),
            None => "===SORRY!=== Error while compiling:\n".to_string(),
        };
        Ok(match full.strip_prefix(heading.as_str()) {
            Some(body) => body.to_string(),
            None => full,
        })
    }
}

/// Every line of `text` indented by four spaces.
// Cost: O(n), n = length of `text`.
fn indent_lines(text: &str) -> String {
    text.lines()
        .map(|line| format!("    {line}"))
        .collect::<Vec<_>>()
        .join("\n")
}

/// Whether `exception` is a sorry the parser queued (`unit_diagnostics::add_parse_sorry`).
// Cost: O(a), a = attributes of the exception.
fn is_parse_sorry(exception: &Value) -> bool {
    match exception.view() {
        ValueView::Instance { attributes, .. } => attributes
            .as_map()
            .contains_key(crate::unit_diagnostics::PARSE_SORRY),
        _ => false,
    }
}

/// The report of a parse sorry as `X::Comp.gist` words it: the message, the
/// position, and the text around it.
// Cost: O(a), a = attributes of the exception.
fn parse_sorry_body(exception: &Value) -> String {
    let ValueView::Instance { attributes, .. } = exception.view() else {
        return String::new();
    };
    let map = attributes.as_map();
    let text = |name: &str| {
        map.get(name)
            .map(|v| v.deref_container().to_string_value())
            .unwrap_or_default()
    };
    format!(
        "{}\nat {}:{}\n------> {}<HERE>{}",
        text("message"),
        text("filename"),
        text("line"),
        text("pre"),
        text("post")
    )
}

/// The file a sorry names (`X::Comp.filename`), if it has one.
// Cost: O(a), a = attributes of the exception.
fn sorry_file(exception: &Value) -> Option<String> {
    match exception.view() {
        ValueView::Instance { attributes, .. } => attributes
            .as_map()
            .get("filename")
            .map(|file| file.deref_container())
            .filter(|file| matches!(file.view(), ValueView::Str(_)))
            .map(|file| file.to_string_value()),
        _ => None,
    }
}
