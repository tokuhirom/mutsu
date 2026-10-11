//! The phaser lift for a `token`/`rule`/`regex` declaration (#12748).
//!
//! A regex literal runs from its source tree, so lifting a phaser out of a code
//! block of the tree is enough (#10550). A declaration's matcher parses the
//! pattern *text* instead, and that text still spells the phaser: the lift has
//! to take the statement out of the text as well. The tree knows which
//! statements it lifted but not where they sit in the text, so the text is
//! scanned for the `INIT`/`CHECK` statements of each code block, nested blocks
//! included, and each is replaced by the lifted phaser's slot. A declaration
//! the scan cannot account for statement by statement keeps its phasers where
//! they are.

use super::lift::Lifter;
use crate::ast::{Expr, Stmt};
use crate::ast_visit::VisitMut;
use crate::regex_tree::{RegexNode, RegexTree};
use crate::value::{Value, ValueView};

/// The name every lifted phaser's slot starts with.
const SLOT_PREFIX: &str = "__phaser_result_";

/// Lift the `INIT`/`CHECK` statements out of the code blocks of a declaration's
/// regex into `check`/`init`, in the declaration's tree and in the pattern text
/// its body literal carries. Leaves the declaration alone when the text and the
/// tree cannot be edited together.
// Cost: O(n), n = size of the declaration's tree and pattern text.
pub(super) fn lift_declaration_phasers(
    body: &mut [Stmt],
    source_regex: &mut Option<RegexTree>,
    check: &mut Vec<Stmt>,
    init: &mut Vec<Stmt>,
) {
    let Some(tree) = source_regex.as_ref() else {
        return;
    };
    let [Stmt::Expr(Expr::Literal(literal))] = body else {
        return;
    };
    let ValueView::Regex(pattern) = literal.view() else {
        return;
    };
    let pattern = pattern.to_string();

    let mut lifted = tree.clone();
    let (mut begin, mut lifted_check, mut lifted_init) = (Vec::new(), Vec::new(), Vec::new());
    let mut lifter = Lifter::new(&mut begin, &mut lifted_check, &mut lifted_init);
    lifter.visit_regex_node_mut(&mut lifted.body);
    // Every lifted phaser is a slot declaration and its assignment.
    let lifted_total = (lifted_check.len() + lifted_init.len()) / 2;
    if lifted_total == 0 || !begin.is_empty() {
        return;
    }

    let mut blocks = CodeBlocks::default();
    blocks.visit_regex_node_mut(&mut lifted.body);
    let mut before = CodeBlocks::default();
    before.visit_regex_node_mut(&mut tree.clone().body);
    let Some(edits) = block_edits(&before.blocks, &blocks.blocks, lifted_total) else {
        return;
    };
    let Some(new_pattern) = rewrite_pattern(&pattern, &edits) else {
        return;
    };

    CodeRewriter {
        edits: &edits,
        next: 0,
    }
    .visit_regex_node_mut(&mut lifted.body);
    let mut value = Value::regex(new_pattern).with_regex_source_tree(lifted.clone());
    if let Some(declared) = literal.regex_declared_source() {
        value = value.with_regex_declared_source(declared);
    }
    *literal = value;
    *source_regex = Some(lifted);
    check.append(&mut lifted_check);
    init.append(&mut lifted_init);
}

/// One code block of a regex tree: its text and the slots of the phasers the
/// lift took out of its top-level statements.
#[derive(Default)]
struct CodeBlocks {
    blocks: Vec<(String, Vec<String>)>,
}

impl VisitMut for CodeBlocks {
    fn visit_regex_node_mut(&mut self, node: &mut RegexNode) {
        if let RegexNode::CodeBlock { code, body } = node {
            let mut collector = SlotCollector::default();
            collector.visit_stmts_mut(body);
            let slots = collector.slots;
            self.blocks.push((code.clone(), slots));
        } else {
            crate::ast_visit::walk_regex_node_mut(self, node);
        }
    }
}

/// The slots of the lifted phasers a statement list reads, in source order.
#[derive(Default)]
struct SlotCollector {
    slots: Vec<String>,
}

impl VisitMut for SlotCollector {
    fn visit_expr_mut(&mut self, expr: &mut Expr) {
        match expr {
            Expr::Var(name) if name.starts_with(SLOT_PREFIX) => self.slots.push(name.clone()),
            _ => crate::ast_visit::walk_expr_mut(self, expr),
        }
    }
}

/// The new text of each code block, `None` for one left as it was.
type Edits = Vec<Option<(String, String)>>;

/// Pair each code block with its text rewritten to read the slots of the
/// phasers the lift took out of it. `None` when the scan finds another number
/// of phaser statements than the lift took out, or the lift took out more than
/// the code blocks' top-level statements account for.
fn block_edits(
    before: &[(String, Vec<String>)],
    after: &[(String, Vec<String>)],
    lifted_total: usize,
) -> Option<Edits> {
    if before.len() != after.len() {
        return None;
    }
    let mut mapped = 0;
    let mut edits = Vec::with_capacity(after.len());
    for ((code, _), (_, slots)) in before.iter().zip(after) {
        if slots.is_empty() {
            edits.push(None);
            continue;
        }
        let spans = phaser_statement_spans(code);
        if spans.len() != slots.len() {
            return None;
        }
        let mut new_code = String::with_capacity(code.len());
        let mut cursor = 0;
        for (span, slot) in spans.iter().zip(slots) {
            new_code.push_str(&code[cursor..span.start]);
            new_code.push('$');
            new_code.push_str(slot);
            cursor = span.end;
        }
        new_code.push_str(&code[cursor..]);
        mapped += slots.len();
        edits.push(Some((code.clone(), new_code)));
    }
    (mapped == lifted_total).then_some(edits)
}

/// `pattern` with every edited code block respelled, left to right.
fn rewrite_pattern(pattern: &str, edits: &Edits) -> Option<String> {
    let mut out = String::with_capacity(pattern.len());
    let mut cursor = 0;
    for (old, new) in edits.iter().flatten() {
        let needle = format!("{{{old}}}");
        let at = pattern[cursor..].find(&needle)? + cursor;
        out.push_str(&pattern[cursor..at]);
        out.push('{');
        out.push_str(new);
        out.push('}');
        cursor = at + needle.len();
    }
    out.push_str(&pattern[cursor..]);
    Some(out)
}

/// Writes each edited code block's new text into the tree, in the order
/// [`CodeBlocks`] saw them.
struct CodeRewriter<'a> {
    edits: &'a Edits,
    next: usize,
}

impl VisitMut for CodeRewriter<'_> {
    fn visit_regex_node_mut(&mut self, node: &mut RegexNode) {
        if let RegexNode::CodeBlock { code, .. } = node {
            if let Some(Some((_, new))) = self.edits.get(self.next) {
                code.clone_from(new);
            }
            self.next += 1;
        } else {
            crate::ast_visit::walk_regex_node_mut(self, node);
        }
    }
}

/// Byte spans of the `INIT`/`CHECK` statements of a code block's text, in
/// source order, at any depth of nested blocks: the keyword and the block or
/// expression it applies to, without the `;` that ends it.
// Cost: O(n), n = length of `code`.
fn phaser_statement_spans(code: &str) -> Vec<std::ops::Range<usize>> {
    let bytes = code.as_bytes();
    let mut spans = Vec::new();
    let mut at_start = true;
    // The open brackets: a statement starts only directly inside a block.
    let mut open: Vec<u8> = Vec::new();
    let mut i = 0;
    while i < bytes.len() {
        let b = bytes[i];
        if let Some(next) = skip_quoted_or_comment(code, i) {
            at_start = false;
            i = next;
            continue;
        }
        let in_block = open.last().is_none_or(|&top| top == b'{');
        match b {
            b' ' | b'\t' | b'\r' | b'\n' => {}
            b'(' | b'[' | b'{' => {
                open.push(b);
                at_start = b == b'{';
            }
            b')' | b']' | b'}' => {
                open.pop();
                at_start = b == b'}' && open.last().is_none_or(|&top| top == b'{');
            }
            b';' => at_start = in_block,
            _ if in_block && at_start => {
                if let Some(keyword) = phaser_keyword(code, i) {
                    let end = statement_end(code, i + keyword.len());
                    spans.push(i..end);
                    i = end;
                    at_start = false;
                    continue;
                }
                at_start = false;
            }
            _ => at_start = false,
        }
        i += 1;
    }
    spans
}

/// `INIT` or `CHECK` when `code` has one as a whole word at `at`.
fn phaser_keyword(code: &str, at: usize) -> Option<&'static str> {
    ["INIT", "CHECK"].into_iter().find(|keyword| {
        code[at..].starts_with(keyword)
            && !code[at + keyword.len()..]
                .chars()
                .next()
                .is_some_and(|c| c.is_alphanumeric() || c == '_' || c == '-')
    })
}

/// Where the statement whose keyword ends at `from` ends: after the block it
/// starts with, else before the `;` (or the end of the text) that ends it.
fn statement_end(code: &str, from: usize) -> usize {
    let bytes = code.as_bytes();
    let mut i = from;
    while i < bytes.len() && bytes[i].is_ascii_whitespace() {
        i += 1;
    }
    let block = bytes.get(i) == Some(&b'{');
    let mut depth = 0usize;
    while i < bytes.len() {
        if let Some(next) = skip_quoted_or_comment(code, i) {
            i = next;
            continue;
        }
        match bytes[i] {
            b'(' | b'[' | b'{' => depth += 1,
            // The bracket that closes the enclosing block ends the statement.
            b')' | b']' | b'}' if depth == 0 => break,
            b')' | b']' | b'}' => {
                depth -= 1;
                if block && depth == 0 {
                    return i + 1;
                }
            }
            b';' if depth == 0 => break,
            _ => {}
        }
        i += 1;
    }
    code[..i].trim_end().len()
}

/// The index after the quoted string or comment starting at `at`, or `None`
/// when none starts there.
fn skip_quoted_or_comment(code: &str, at: usize) -> Option<usize> {
    let bytes = code.as_bytes();
    match bytes[at] {
        quote @ (b'\'' | b'"') => {
            let mut i = at + 1;
            while i < bytes.len() {
                match bytes[i] {
                    b'\\' => i += 2,
                    b if b == quote => return Some(i + 1),
                    _ => i += 1,
                }
            }
            Some(bytes.len())
        }
        // `$#array` is a variable, not a comment.
        b'#' if at == 0 || bytes[at - 1] != b'$' => {
            Some(code[at..].find('\n').map_or(bytes.len(), |n| at + n))
        }
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::phaser_statement_spans;

    fn spelled(code: &str) -> Vec<&str> {
        phaser_statement_spans(code)
            .into_iter()
            .map(|span| &code[span])
            .collect()
    }

    #[test]
    fn finds_top_level_phaser_statements() {
        assert_eq!(spelled(" INIT @log.push('G') "), ["INIT @log.push('G')"]);
        assert_eq!(
            spelled("my $x = 1; CHECK { $x++ }; INIT say 'a;b'"),
            ["CHECK { $x++ }", "INIT say 'a;b'"]
        );
        assert_eq!(spelled("INIT { 1 } foo()"), ["INIT { 1 }"]);
    }

    #[test]
    fn finds_nested_and_ignores_quoted_phasers() {
        assert_eq!(spelled("if $x { INIT 1 }"), ["INIT 1"]);
        assert!(spelled("foo(INIT 1)").is_empty());
        assert!(spelled("say 'INIT'; # INIT x").is_empty());
        assert!(spelled("INITIALIZE(); $INIT").is_empty());
    }
}
