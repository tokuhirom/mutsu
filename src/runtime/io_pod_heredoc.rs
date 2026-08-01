//! Heredoc-body detection for the `$=pod` line scanner.
//!
//! [`collect_pod_blocks`](super::Interpreter::collect_pod_blocks) walks the raw
//! source line by line rather than reading the parsed program, so it has no
//! notion of quoting context. Without the mask computed here, a `=begin pod` (or
//! a `#|` declarator comment) written *inside* a `q:to/END/` string literal is
//! collected into `$=pod` as if the program had declared it — which is exactly
//! what `t/doc-mode-pod-render.t` does when it embeds a whole document in a
//! heredoc to feed `is_run`. Rakudo's real `Pod::To::Text` then walks Pod nodes
//! the program never declared and dies in `declarator2text`.
//!
//! TODO: compile Pod collection into the parser so quoting context comes from
//! the real grammar. A line scanner cannot recover it in general; heredocs are
//! handled here because their bodies are the only quoted text whose extent is
//! whole lines, which a scanner *can* track.

use super::*;

impl Interpreter {
    /// Mark the source lines that lie inside a heredoc body.
    ///
    /// Lines inside a `=begin ... =end` block are deliberately left unmasked:
    /// there a `q:to/END/` is prose or a code sample, not a heredoc that opens.
    pub(super) fn heredoc_body_lines(lines: &[&str]) -> Vec<bool> {
        let mut inside = vec![false; lines.len()];
        let mut pod_depth = 0usize;
        let mut idx = 0usize;
        while idx < lines.len() {
            if let Some((directive, _)) = Self::active_pod_directive(lines[idx], None) {
                match directive {
                    "begin" => pod_depth += 1,
                    "end" => pod_depth = pod_depth.saturating_sub(1),
                    _ => {}
                }
                idx += 1;
                continue;
            }
            if pod_depth > 0 {
                idx += 1;
                continue;
            }
            let terminators = Self::heredoc_terminators_in_line(lines[idx]);
            if terminators.is_empty() {
                idx += 1;
                continue;
            }
            // Several heredocs may open on one line (`f(q:to/A/, q:to/B/)`);
            // their bodies follow in the order they were opened.
            let mut body = idx + 1;
            for terminator in terminators {
                while body < lines.len() && lines[body].trim() != terminator {
                    inside[body] = true;
                    body += 1;
                }
                // The terminator line itself is not body text.
                body = (body + 1).min(lines.len());
            }
            idx = body.max(idx + 1);
        }
        inside
    }

    /// The heredoc terminators opened on a single source line, in order.
    pub(super) fn heredoc_terminators_in_line(line: &str) -> Vec<String> {
        let mut out = Vec::new();
        let mut pos = 0usize;
        while let Some(found) = line[pos..].find(":to") {
            let at = pos + found;
            pos = at + ":to".len();
            if !Self::opens_quote_with_to_adverb(&line[..at]) {
                continue;
            }
            if let Some(terminator) = Self::heredoc_delimiter_text(&line[pos..]) {
                out.push(terminator);
            }
        }
        out
    }

    /// Whether the text preceding a `:to` is a quote construct it can adverb
    /// (`q`, `qq`, `Q`, each optionally carrying earlier adverbs as in
    /// `q:b:to`). Guards against unrelated `:to` text such as a `/:to/` regex.
    fn opens_quote_with_to_adverb(before: &str) -> bool {
        let mut head = before;
        // Walk back over any earlier `:adverb` groups.
        loop {
            let trimmed = head.trim_end();
            let Some(colon) = trimmed.rfind(':') else {
                break;
            };
            let name = &trimmed[colon + 1..];
            if name.is_empty()
                || !name
                    .chars()
                    .all(|c| c.is_ascii_alphanumeric() || c == '-' || c == '_')
            {
                break;
            }
            head = &trimmed[..colon];
        }
        let head = head.trim_end();
        let Some(prefix) = head
            .strip_suffix("qq")
            .or_else(|| head.strip_suffix('q'))
            .or_else(|| head.strip_suffix('Q'))
        else {
            return false;
        };
        // `Xq:to` is an identifier ending in `q`, not a quote construct.
        !prefix
            .chars()
            .next_back()
            .is_some_and(|c| c.is_alphanumeric() || c == '_' || c == '-')
    }

    /// Read the terminator out of the delimiter that follows `:to`
    /// (`/END/`, `"END"`, `<END>`, ... — plus any further adverbs in between).
    fn heredoc_delimiter_text(after_to: &str) -> Option<String> {
        let mut rest = after_to.trim_start();
        while let Some(adverb) = rest.strip_prefix(':') {
            let end = adverb
                .find(|c: char| !(c.is_ascii_alphanumeric() || c == '-' || c == '_'))
                .unwrap_or(adverb.len());
            if end == 0 {
                break;
            }
            rest = adverb[end..].trim_start();
        }
        let open = rest.chars().next()?;
        if open.is_alphanumeric() || open.is_whitespace() {
            return None;
        }
        let close = match open {
            '(' => ')',
            '[' => ']',
            '{' => '}',
            '<' => '>',
            other => other,
        };
        let body = &rest[open.len_utf8()..];
        let end = body.find(close)?;
        Some(body[..end].trim().to_string())
    }
}

#[cfg(test)]
mod tests {
    use crate::runtime::Interpreter;

    fn mask(source: &str) -> Vec<bool> {
        let lines: Vec<&str> = source.lines().collect();
        Interpreter::heredoc_body_lines(&lines)
    }

    #[test]
    fn masks_heredoc_body_but_not_terminator() {
        let mask = mask("my $s = q:to\"END\";\n=begin pod\n=end pod\nEND\nsay $s;\n");
        assert_eq!(mask, vec![false, true, true, false, false]);
    }

    #[test]
    fn leaves_pod_block_contents_alone() {
        // A `q:to` shown as a code sample inside Pod opens nothing.
        let mask = mask("=begin pod\nmy $s = q:to/END/;\nbody\nEND\n=end pod\nsay 1;\n");
        assert_eq!(mask, vec![false, false, false, false, false, false]);
    }

    #[test]
    fn masks_two_heredocs_opened_on_one_line() {
        let mask = mask("f(q:to/A/, q:to/B/);\naaa\nA\nbbb\nB\ndone\n");
        assert_eq!(mask, vec![false, true, false, true, false, false]);
    }

    #[test]
    fn ignores_to_that_is_not_a_quote_adverb() {
        let mask = mask("my $re = /:to/;\nsay 1;\n");
        assert_eq!(mask, vec![false, false]);
    }

    #[test]
    fn ignores_identifier_ending_in_q() {
        let mask = mask("my $x = $foq:to;\nsay 1;\n");
        assert_eq!(mask, vec![false, false]);
    }
}
