use super::*;
use crate::value::ValueView;

impl Interpreter {
    /// `.wordcase(:&filter = &tclc, :$where = True)`: title-case each word,
    /// but only words that smart-match `:where`, transformed by `:filter`.
    // Cost: O(n) plus one `:where` smartmatch and one `:filter` call per word,
    // n = chars of the invocant.
    pub(crate) fn dispatch_wordcase(
        &mut self,
        target: Value,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let text = target.to_string_value();
        let mut filter: Option<Value> = None;
        let mut where_matcher: Option<Value> = None;
        for arg in args {
            if let ValueView::Pair(key, value) = arg.view() {
                match key.as_str() {
                    "filter" => filter = Some(value.clone()),
                    "where" => where_matcher = Some(value.clone()),
                    _ => {}
                }
            }
        }

        let mut result = String::new();
        for (segment, is_word) in crate::value::wordcase_segments(&text) {
            if !is_word {
                result.push_str(&segment);
                continue;
            }
            let word = Value::str(segment.clone());
            // `:where` selects which words are transformed (default: all).
            let selected = match &where_matcher {
                Some(m) => self.smart_match(&word, m),
                None => true,
            };
            if !selected {
                result.push_str(&segment);
                continue;
            }
            // `:filter` transforms the selected word (default: tclc).
            let transformed = match &filter {
                Some(f) => self
                    .call_sub_value(f.clone(), vec![word], false)?
                    .to_string_value(),
                None => crate::value::tclc_str(&segment),
            };
            result.push_str(&transformed);
        }
        // An allomorph (IntStr/RatStr/NumStr/ComplexStr) argument reads its
        // Str part above (via `target.to_string_value()`), but the RESULT must
        // preserve the allomorph the way the 0-arg form does.
        if let ValueView::Mixin(inner, mixins) = target.view()
            && crate::value::types::allomorph_type_name(inner, mixins).is_some()
        {
            return Ok(crate::value::types::allomorph_wordcase_result(
                inner, result,
            ));
        }
        Ok(Value::str(result))
    }
}
