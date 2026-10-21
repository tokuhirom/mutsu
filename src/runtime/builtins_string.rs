use super::*;

impl Interpreter {
    // chr / ord / chrs / ords / unival / univals removed (Slice 6.3 / Category A
    // dedup): native implementations in src/builtins/functions.rs are
    // authoritative (reached via call_function_fallback -> native_function).
    // chrs is routed through native_function_variadic; ords/unival/univals are in
    // native_function_1arg (unival/univals delegate to the .unival/.univals
    // method, the single source of truth in the `Cool.unival`/`univals` rows).
    // flip / lc / uc / tc / trim / chars removed (Slice 6.3 dedup) — same.

    pub(super) fn builtin_unknown_backslash_escape(
        &self,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let escape = args
            .first()
            .map(|v| v.to_string_value())
            .unwrap_or_else(|| "?".to_string());
        Err(RuntimeError::typed_msg(
            "X::Backslash::UnrecognizedSequence",
            format!("Unrecognized backslash sequence: \\{}", escape),
        ))
    }


    /// Handle .split() method with full support for regex splitters, limits, and named params.
    pub(crate) fn handle_split_method(
        &mut self,
        target: Value,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        use crate::builtins::split::{SplitOpts, apply_split_opts, parse_split_limit};

        let (positional, mut opts) = SplitOpts::from_args(&args);
        if positional.is_empty() {
            return Err(RuntimeError::new("Must specify a pattern for split"));
        }

        opts.check_conflicts("Str")?;

        let splitter = positional[0].clone();
        if positional.len() > 1 {
            opts.limit = parse_split_limit(positional[1])?;
        }

        let text = target.to_string_value();
        let parts = self.split_with_splitter(&text, &splitter, opts.limit)?;
        let result = apply_split_opts(parts, &opts);
        Ok(Value::seq(result))
    }

    /// Handle split() function with full support for regex splitters.
    pub(crate) fn handle_split_function(
        &mut self,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        use crate::builtins::split::{SplitOpts, apply_split_opts, parse_split_limit};

        let (positional, mut opts) = SplitOpts::from_args(&args);
        if positional.len() < 2 {
            return Err(RuntimeError::new(
                "split requires at least 2 arguments: splitter and string",
            ));
        }

        let splitter = positional[0].clone();
        let target = positional[1].clone();
        if positional.len() > 2 {
            opts.limit = parse_split_limit(positional[2])?;
        }

        let text = target.to_string_value();
        let parts = self.split_with_splitter(&text, &splitter, opts.limit)?;
        let result = apply_split_opts(parts, &opts);
        Ok(Value::seq(result))
    }

    /// Core split implementation that handles string, regex, and list splitters.
    fn split_with_splitter(
        &mut self,
        text: &str,
        splitter: &Value,
        limit: Option<usize>,
    ) -> Result<Vec<(String, Option<SplitMatch>)>, RuntimeError> {
        match splitter.view() {
            ValueView::Regex(pattern) => self.split_by_regex(text, &pattern, limit),
            ValueView::RegexWithAdverbs(a) => self.split_by_regex(text, &a.pattern, limit),
            ValueView::Array(items, _) => {
                // Check if any item is a regex
                let has_regex = items.iter().any(|v| {
                    matches!(
                        v.view(),
                        ValueView::Regex(_) | ValueView::RegexWithAdverbs(..)
                    )
                });
                if has_regex {
                    self.split_by_regex_list(text, &items, limit)
                } else {
                    let strings: Vec<String> = items.iter().map(|v| v.to_string_value()).collect();
                    Ok(crate::builtins::split::split_by_strings(
                        text, &strings, limit,
                    ))
                }
            }
            _ => {
                let sep = splitter.to_string_value();
                // The one string splitter the pure cascade uses too: it
                // matches only at grapheme boundaries.
                Ok(crate::builtins::split::split_by_string(text, &sep, limit))
            }
        }
    }

    /// Build the `Match` object a `:v`/`:kv`/`:p` split reports for one regex
    /// separator. It is the same construction `~~ /…/` and `.match` use, so a
    /// separator Match carries the engine's span-bearing positional AND named
    /// captures (`$<name>` on a split separator used to come back `Nil`) and
    /// their `.from`/`.to` are real offsets into the split subject.
    fn split_separator_match(&mut self, mut caps: RegexCaptures, text: &str) -> Value {
        let from = caps.from as i64;
        let to = caps.to as i64;
        let mtarget = caps.target_or_new(text);
        self.reduce_regex_captures_made(&mut caps, Some(&mtarget));
        Value::make_match_object_full_visible(from, to, &caps.positional, &caps.named, mtarget)
            .with_match_cursor_pos(caps.narrowed_pos())
    }

    /// Split by a single regex pattern.
    // Cost: O(n + k) plus the engine's per-match cost, n = chars of the invocant,
    // k = pieces; one shared MatchTarget, each separator Match shares it (O(1)).
    fn split_by_regex(
        &mut self,
        text: &str,
        pattern: &str,
        limit: Option<usize>,
    ) -> Result<Vec<(String, Option<SplitMatch>)>, RuntimeError> {
        if limit == Some(0) {
            return Ok(Vec::new());
        }
        let chars: Vec<char> = text.chars().collect();
        // One target for the whole walk. Each `MatchTarget` copies the subject
        // and its char vector, so building one per separator made a regex split
        // O(separators x subject) -- 11.9 s on 80 KB against rakudo's 0.26 s
        // (#8247).
        let target = MatchTarget::new(text);
        let mut result = Vec::new();

        if text.is_empty() {
            result.push((String::new(), None));
            return Ok(result);
        }

        let max_splits = limit.map(|l| if l > 0 { l - 1 } else { 0 });
        let mut splits_done = 0;
        let mut pos = 0; // current segment start
        let mut search_from = 0; // where to start searching for next match

        loop {
            if let Some(max) = max_splits
                && splits_done >= max
            {
                let remaining: String = chars[pos..].iter().collect();
                result.push((remaining, None));
                return Ok(result);
            }

            if search_from > chars.len() {
                break;
            }

            // Try to match regex starting from search_from, using full text for context
            if let Some(caps) =
                self.regex_match_with_captures_from_target(pattern, &target, search_from)
            {
                let (from, to) = (caps.from, caps.to);
                let segment: String = chars[pos..from].iter().collect();
                let matched: String = chars[from..to].iter().collect();
                let match_obj = self.split_separator_match(caps, text);
                result.push((
                    segment,
                    Some(SplitMatch {
                        from,
                        to,
                        matched,
                        splitter_index: 0,
                        is_regex: true,
                        match_obj: Some(match_obj),
                    }),
                ));
                splits_done += 1;
                if from == to {
                    // Zero-width match: next segment starts at from,
                    // but search for next match from from+1
                    pos = from;
                    search_from = from + 1;
                } else {
                    pos = to;
                    search_from = to;
                }
            } else {
                let remaining: String = chars[pos..].iter().collect();
                result.push((remaining, None));
                return Ok(result);
            }
        }

        if pos <= chars.len() {
            let remaining: String = chars[pos..].iter().collect();
            result.push((remaining, None));
        }
        Ok(result)
    }

    /// Split by a list of splitters (mix of string and regex).
    ///
    /// Each splitter's next match is cached and re-searched only once the
    /// cursor has moved past its start, so a rare or absent splitter is not
    /// rescanned to the end once per piece (#9145).
    // Cost: O(s*n + k) searches, n = chars of the invocant, s = splitters,
    // k = pieces: each splitter's searches start at strictly increasing
    // cursors beyond its previous match, a k-way merge of the find streams.
    fn split_by_regex_list(
        &mut self,
        text: &str,
        splitters: &[Value],
        limit: Option<usize>,
    ) -> Result<Vec<(String, Option<SplitMatch>)>, RuntimeError> {
        if limit == Some(0) {
            return Ok(Vec::new());
        }
        let chars: Vec<char> = text.chars().collect();
        let grapheme_index = crate::builtins::grapheme_index::GraphemeIndex::build(text);
        // One target for the whole walk. Each `MatchTarget` copies the subject
        // and its char vector, so building one per separator made a regex split
        // O(separators x subject) -- 11.9 s on 80 KB against rakudo's 0.26 s
        // (#8247).
        let target = MatchTarget::new(text);
        let mut result = Vec::new();

        if text.is_empty() {
            result.push((String::new(), None));
            return Ok(result);
        }

        let max_splits = limit.map(|l| if l > 0 { l - 1 } else { 0 });
        let mut splits_done = 0;
        let mut pos = 0;
        let mut byte_pos = 0;
        // Per splitter: `None` = not searched yet, `Some(None)` = no match at
        // or after the last search start (so none ever again), `Some(Some(m))`
        // = its first match at or after an earlier cursor, still valid while
        // `m.from >= pos` (matching runs against the one full-text target, so
        // a match's extent does not depend on where the search began).
        let mut next: Vec<Option<Option<CachedSplitMatch>>> =
            splitters.iter().map(|_| None).collect();

        loop {
            if let Some(max) = max_splits
                && splits_done >= max
            {
                let remaining: String = chars[pos..].iter().collect();
                result.push((remaining, None));
                return Ok(result);
            }

            // (abs_from, abs_to, idx) of the winning splitter's cached match.
            let mut best: Option<(usize, usize, usize)> = None;

            for (idx, splitter) in splitters.iter().enumerate() {
                let stale = match &next[idx] {
                    Some(Some(m)) => m.from < pos,
                    Some(None) => false,
                    None => true,
                };
                if stale {
                    next[idx] = Some(self.split_list_find(
                        splitter,
                        &target,
                        text,
                        &grapheme_index,
                        &chars,
                        SplitCursor { char_pos: pos, byte_pos },
                    ));
                }
                let Some(Some(m)) = &next[idx] else { continue };
                let is_better = match best {
                    None => true,
                    // A regex splitter wins a tie on length; a string splitter
                    // only by starting strictly earlier.
                    Some((bf, bt, _)) => {
                        m.from < bf
                            || (m.caps.is_some() && m.from == bf && (m.to - m.from) > (bt - bf))
                    }
                };
                if is_better {
                    best = Some((m.from, m.to, idx));
                }
            }

            // The winner is consumed: the cursor moves past its start, so its
            // cache slot would be stale anyway.
            match best.and_then(|(_, _, idx)| next[idx].take().flatten().map(|m| (idx, m))) {
                Some((
                    idx,
                    CachedSplitMatch {
                        from,
                        to,
                        byte_to,
                        matched,
                        caps,
                    },
                )) => {
                    let segment: String = chars[pos..from].iter().collect();
                    let is_regex = caps.is_some();
                    let match_obj = caps.map(|c| self.split_separator_match(c, text));
                    result.push((
                        segment,
                        Some(SplitMatch {
                            from,
                            to,
                            matched,
                            splitter_index: idx,
                            is_regex,
                            match_obj,
                        }),
                    ));
                    pos = to;
                    byte_pos = byte_to;
                    if pos == from {
                        // Zero-width match: advance by one codepoint.
                        pos += 1;
                        if let Some(ch) = chars.get(from) {
                            byte_pos += ch.len_utf8();
                        }
                    }
                    splits_done += 1;
                }
                None => {
                    let remaining: String = chars[pos..].iter().collect();
                    result.push((remaining, None));
                    return Ok(result);
                }
            }
        }
    }
}

/// One splitter's next match in [`Interpreter::split_by_regex_list`].
struct CachedSplitMatch {
    from: usize,
    to: usize,
    byte_to: usize,
    matched: String,
    /// `Some` exactly when the splitter is a regex.
    caps: Option<RegexCaptures>,
}

/// Character and byte offsets at the same split cursor.
struct SplitCursor {
    char_pos: usize,
    byte_pos: usize,
}

impl Interpreter {
    /// First match of one list-form splitter at or after char `pos`.
    // Cost: O(n - pos) regex attempts, or O((n - pos) * m) for a string
    // splitter of m chars, n = chars of the invocant.
    fn split_list_find(
        &mut self,
        splitter: &Value,
        target: &MatchTarget,
        text: &str,
        grapheme_index: &crate::builtins::grapheme_index::GraphemeIndex,
        chars: &[char],
        cursor: SplitCursor,
    ) -> Option<CachedSplitMatch> {
        let caps = match splitter.view() {
            ValueView::Regex(p) => {
                self.regex_match_with_captures_from_target(&p, target, cursor.char_pos)
            }
            ValueView::RegexWithAdverbs(a) => {
                self.regex_match_with_captures_from_target(&a.pattern, target, cursor.char_pos)
            }
            _ => {
                let sep = splitter.to_string_value();
                if sep.is_empty() || cursor.byte_pos > text.len() {
                    return None;
                }
                let sep_chars: Vec<char> = sep.chars().collect();
                let from_byte = crate::builtins::grapheme_index::find_graphemes(
                    text,
                    grapheme_index,
                    cursor.byte_pos,
                    &sep,
                )?;
                let from = cursor.char_pos + text[cursor.byte_pos..from_byte].chars().count();
                return Some(CachedSplitMatch {
                    from,
                    to: from + sep_chars.len(),
                    byte_to: from_byte + sep.len(),
                    matched: sep,
                    caps: None,
                });
            }
        }?;
        let byte_from = cursor.byte_pos
            + chars[cursor.char_pos..caps.from]
                .iter()
                .map(|ch| ch.len_utf8())
                .sum::<usize>();
        let byte_to = byte_from
            + chars[caps.from..caps.to]
                .iter()
                .map(|ch| ch.len_utf8())
                .sum::<usize>();
        Some(CachedSplitMatch {
            from: caps.from,
            to: caps.to,
            byte_to,
            matched: chars[caps.from..caps.to].iter().collect(),
            caps: Some(caps),
        })
    }
}

/// SplitMatch from the builtins split module (re-export for local use).
use crate::builtins::split::SplitMatch;

