//! A regex value keeps its parser source tree across the module cache's
//! serialization (#18003): without it, every match of a regex from a
//! precompiled module took the string path.

use crate::regex_tree::RegexTree;
use crate::value::{RegexAdverbs, Value};

/// `value` serialized and read back; `None` when either step fails.
fn round_trip(value: &Value) -> Option<Value> {
    let encoded = serde_json::to_string(value).ok()?;
    serde_json::from_str(&encoded).ok()
}

/// Whether `value` survives the round trip with a source tree.
fn keeps_tree(value: &Value) -> bool {
    round_trip(value).is_some_and(|v| v.regex_source_tree().is_some())
}

#[test]
fn a_plain_regex_keeps_its_source_tree() {
    let tree = RegexTree::parse_static("a (\\d+)", false);
    assert!(tree.is_some());
    let Some(tree) = tree else { return };
    let value = Value::regex("a (\\d+)".to_string()).with_regex_source_tree(tree);
    assert!(keeps_tree(&value));
    assert_eq!(
        round_trip(&value).map(|v| v.to_string_value()),
        Some(value.to_string_value())
    );
}

#[test]
fn an_adverbed_regex_keeps_its_source_tree() {
    let tree = RegexTree::parse_static("abc", false);
    assert!(tree.is_some());
    let Some(tree) = tree else { return };
    let adverbs = RegexAdverbs {
        pattern: std::sync::Arc::new("abc".to_string()),
        global: false,
        exhaustive: false,
        overlap: false,
        repeat: None,
        nth: None,
        pos: false,
        pos_value: None,
        continue_: false,
        continue_value: None,
        ignore_case: true,
        sigspace: false,
        samecase: false,
        samespace: false,
        source_adverbs: None,
        captured: None,
        topic: None,
        source_tree: None,
        id: Default::default(),
        name: Default::default(),
    };
    let value = Value::regex_with_adverbs(adverbs).with_regex_source_tree(tree);
    assert!(value.regex_source_tree().is_some());
    assert!(keeps_tree(&value));
}

#[test]
fn a_regex_without_a_tree_stays_without_one() {
    let restored = round_trip(&Value::regex("x".to_string()));
    assert!(restored.is_some_and(|v| v.regex_source_tree().is_none()));
}
