use super::*;

impl Parser {
    /// A quoted term: `"..."` / `“...”` (a `qq` string), `'...'` / `‘...’`
    /// (a `q` string) or `｢...｣` (no escapes). The node holds the decoded
    /// text, which is what both rakudo's `StrLiteral` and execution see.
    // Cost: O(n), n = length of the quoted body.
    pub(super) fn parse_quote(&mut self, quote: char) -> Option<RegexNode> {
        let escapes = quote != '\u{FF62}';
        let qq = crate::regex_qq_atoms::is_dq_opener(quote);
        let start = self.pos + 1;
        let end = crate::regex_qq_atoms::quote_end(&self.chars, self.pos)?;
        let close = self.chars[end];
        let body: String = self.chars.get(start..end)?.iter().collect();
        let text = if qq {
            match crate::parser::decode_qq_regex_quote(&body) {
                Some(text) => text,
                // A body that interpolates keeps what the `qq` parser makes of it.
                None => {
                    let expr = crate::parser::interpolate_qq_content(&body).ok()?;
                    if matches!(expr, crate::ast::Expr::Literal(_)) {
                        return None;
                    }
                    self.pos = end + 1;
                    return Some(RegexNode::Extension(RegexExtension::InterpolatedQuote {
                        source: body,
                        expr: Box::new(expr),
                    }));
                }
            }
        } else if escapes {
            crate::parser::decode_q_regex_quote(&body, close)
        } else {
            body
        };
        self.pos = end + 1;
        Some(RegexNode::Quote(text))
    }
}
