/// Global deprecation tracking for `is DEPRECATED` trait.
///
/// Records deprecated function/method calls and produces
/// the `Deprecation.report` formatted output.
use std::cell::RefCell;

/// A single deprecation entry, identified by (kind, name, package, message).
#[derive(Debug, Clone, Hash, Eq, PartialEq)]
struct DeprecationKey {
    kind: String, // "Sub" or "Method"
    name: String,
    package: String,
    message: String, // empty means "something else"
}

/// Tracks call sites for a single deprecation entry.
#[derive(Debug, Clone)]
struct DeprecationEntry {
    key: DeprecationKey,
    file: String,
    lines: Vec<i64>,
}

thread_local! {
    static DEPRECATION_EVENTS: RefCell<Vec<DeprecationEntry>> = const { RefCell::new(Vec::new()) };
}

/// Record a deprecation event (called when a deprecated function/method is invoked).
pub(crate) fn record_deprecation(
    kind: &str,
    name: &str,
    package: &str,
    message: &str,
    file: &str,
    line: i64,
) {
    DEPRECATION_EVENTS.with(|events| {
        let mut events = events.borrow_mut();
        let key = DeprecationKey {
            kind: kind.to_string(),
            name: name.to_string(),
            package: package.to_string(),
            message: message.to_string(),
        };
        // Find existing entry for this key+file
        if let Some(entry) = events.iter_mut().find(|e| e.key == key && e.file == file) {
            if !entry.lines.contains(&line) {
                entry.lines.push(line);
            }
        } else {
            events.push(DeprecationEntry {
                key,
                file: file.to_string(),
                lines: vec![line],
            });
        }
    });
}

/// Produce the deprecation report and clear accumulated events.
/// Returns `None` if there are no events, `Some(report)` otherwise.
pub(crate) fn take_report() -> Option<String> {
    DEPRECATION_EVENTS.with(|events| {
        let mut events = events.borrow_mut();
        if events.is_empty() {
            return None;
        }

        // Group events by key (same deprecation across different files/lines)
        // Each group: (key, vec of (file, lines))
        type GroupedEvents = Vec<(DeprecationKey, Vec<(String, Vec<i64>)>)>;
        let mut grouped: GroupedEvents = Vec::new();
        for entry in events.drain(..) {
            if let Some(g) = grouped.iter_mut().find(|(k, _)| *k == entry.key) {
                g.1.push((entry.file, entry.lines));
            } else {
                grouped.push((entry.key, vec![(entry.file, entry.lines)]));
            }
        }

        let count = grouped.len();
        let mut report = format!(
            "Saw {} occurrence{} of deprecated code.\n{}",
            count,
            if count == 1 { "" } else { "s" },
            "=".repeat(80),
        );

        for (key, sites) in &grouped {
            report.push_str(&format!(
                "\n{} {} (from {}) seen at:",
                key.kind, key.name, key.package
            ));
            for (file, lines) in sites {
                if lines.len() == 1 {
                    report.push_str(&format!("\n  {}, line {}", file, lines[0]));
                } else {
                    let line_strs: Vec<String> = lines.iter().map(|l| l.to_string()).collect();
                    report.push_str(&format!("\n  {}, lines {}", file, line_strs.join(",")));
                }
            }
            if key.message.is_empty() {
                report.push_str("\nPlease use something else instead.");
            } else {
                report.push_str(&format!("\nPlease use {} instead.", key.message));
            }
            report.push_str(&format!("\n{}", "-".repeat(80)));
        }

        Some(report)
    })
}

/// Build a combined report from multiple per-entry reports.
/// Used when Deprecation.report merges partial state.
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_basic_report() {
        // Clear any existing state
        DEPRECATION_EVENTS.with(|e| e.borrow_mut().clear());

        record_deprecation("Sub", "a", "GLOBAL", "", "test.p6", 10);
        let report = take_report().unwrap();
        assert!(report.contains("Saw 1 occurrence of deprecated code."));
        assert!(report.contains("Sub a (from GLOBAL) seen at:"));
        assert!(report.contains("test.p6, line 10"));
        assert!(report.contains("Please use something else instead."));

        // After take_report, events should be cleared
        assert!(take_report().is_none());
    }

    #[test]
    fn test_report_with_message() {
        DEPRECATION_EVENTS.with(|e| e.borrow_mut().clear());

        record_deprecation("Sub", "awith", "GLOBAL", "'fnorkle'", "test.p6", 20);
        record_deprecation("Sub", "awith", "GLOBAL", "'fnorkle'", "test.p6", 21);
        let report = take_report().unwrap();
        assert!(report.contains("Saw 1 occurrence of deprecated code."));
        assert!(report.contains("lines 20,21"));
        assert!(report.contains("Please use 'fnorkle' instead."));
    }
}
