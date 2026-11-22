//! A regex value keeps its parser source tree across the module cache's
//! serialization (#18003): without it, every match of a regex from a
//! precompiled module took the string path.

use crate::regex_tree::RegexTree;
use crate::value::{RegexAdverbs, Value};

fn round_trip(value: &Value) -> Value {
    let encoded = serde_json::to_string(value).expect("serialize regex");
    serde_json::from_str(&encoded).expect("deserialize regex")
}

#[test]
fn a_plain_regex_keeps_its_source_tree() {
    let tree = RegexTree::parse_static("a (\\d+)", false).expect("static tree");
    let value = Value::regex("a (\\d+)".to_string()).with_regex_source_tree(tree);
    let restored = round_trip(&value);
    assert!(restored.regex_source_tree().is_some());
    assert_eq!(restored.to_string_value(), value.to_string_value());
}

#[test]
fn an_adverbed_regex_keeps_its_source_tree() {
    let tree = RegexTree::parse_static("abc", false).expect("static tree");
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
    assert!(round_trip(&value).regex_source_tree().is_some());
}

#[test]
fn a_regex_without_a_tree_stays_without_one() {
    assert!(round_trip(&Value::regex("x".to_string())).regex_source_tree().is_none());
}
