//! What a regex literal's pattern text says about evaluating it
//! ([`crate::opcode::RegexLiteralFacts`]), settled once per chunk rather than
//! scanned on every execution of its `LoadRegexClosure` (#19296).

use super::*;

impl Interpreter {
    /// What a regex literal's pattern text says about evaluating it: whether
    /// it interpolates a string as regex source (`<{ }>`), and whether it
    /// holds code that runs at match time: `$( )`, `<{ }>` or a `{ }` block
    /// that is not a `{n,m}` quantifier (the latter only decides whether the
    /// defining unit is worth carrying).
    // Cost: O(n), n = pattern length.
    pub(super) fn regex_literal_facts_of(base: &Value) -> crate::opcode::RegexLiteralFacts {
        let ValueView::Regex(p) = base.view() else {
            return crate::opcode::RegexLiteralFacts::default();
        };
        let has_eval_block = p.contains("<{");
        let has_code = has_eval_block || p.contains("$(") || {
            let b = p.as_bytes();
            b.iter().enumerate().any(|(i, &c)| {
                c == b'{'
                    && b.get(i + 1)
                        .is_some_and(|n| !n.is_ascii_digit() && !matches!(n, b',' | b'}' | b'*'))
            })
        };
        crate::opcode::RegexLiteralFacts {
            has_eval_block,
            has_code,
        }
    }

    /// [`Self::regex_literal_facts_of`] the regex-literal constant
    /// `const_idx` of `code`, settled for every `LoadRegexClosure` of the
    /// chunk on first use (`CompiledCode::regex_literal_facts`).
    // Cost: O(1) after the chunk's first regex literal; that first call is
    // O(o + p), o = the chunk's ops, p = the total length of its literals.
    pub(super) fn regex_literal_facts(
        code: &CompiledCode,
        const_idx: u32,
    ) -> crate::opcode::RegexLiteralFacts {
        let facts_of = |idx: u32| {
            code.constants
                .get(idx as usize)
                .map(Self::regex_literal_facts_of)
                .unwrap_or_default()
        };
        let table = code.regex_literal_facts.get_or_init(|| {
            code.ops
                .iter()
                .filter_map(|op| match op {
                    OpCode::LoadRegexClosure { const_idx, .. } => {
                        Some((*const_idx, facts_of(*const_idx)))
                    }
                    _ => None,
                })
                .collect()
        });
        table
            .get(&const_idx)
            .copied()
            .unwrap_or_else(|| facts_of(const_idx))
    }
}
