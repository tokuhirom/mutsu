use crate::symbol::Symbol;
use crate::value::{RuntimeError, StrBody, Value, ValueView};
use std::sync::Arc;

/// Named parameters for split
#[derive(Default)]
pub(crate) struct SplitOpts {
    pub v: bool,
    pub k: bool,
    pub kv: bool,
    pub p: bool,
    pub skip_empty: bool,
    pub limit: Option<usize>,
}

impl SplitOpts {
    /// Parse named parameters from args, returning positional args and options.
    ///
    /// A named argument never occupies a positional slot: `.split` declares
    /// `:v`/`:k`/`:kv`/`:p`/`:skip-empty` and swallows every other named through
    /// the implicit `*%_` every Raku method carries. So an unrecognized
    /// named-flavour `Pair` is dropped here rather than falling into `positional`
    /// (where it used to be read as the `$limit` argument, numifying to 0 and
    /// making `"a,b".split(",", :nonsense)` return an empty Seq). A *positional*
    /// `Pair` — the `ValuePair` flavour, ADR-0021 — is still a real argument and
    /// keeps its slot.
    pub(crate) fn from_args(args: &[Value]) -> (Vec<&Value>, SplitOpts) {
        let mut opts = SplitOpts::default();
        let mut positional = Vec::new();
        for arg in args {
            match arg.view() {
                ValueView::Pair(key, value) if key == "v" => opts.v = value.truthy(),
                ValueView::Pair(key, value) if key == "k" => opts.k = value.truthy(),
                ValueView::Pair(key, value) if key == "kv" => opts.kv = value.truthy(),
                ValueView::Pair(key, value) if key == "p" => opts.p = value.truthy(),
                ValueView::Pair(key, value) if key == "skip-empty" || key == "skip_empty" => {
                    opts.skip_empty = value.truthy()
                }
                ValueView::Pair(..) => {}
                _ => positional.push(arg),
            }
        }
        (positional, opts)
    }

    /// Whether the result reports each separator itself (`:v`/`:kv`/`:p`), so a
    /// regex splitter has to build its separator `Match` objects. `:k` reports
    /// only the splitter index.
    pub(crate) fn reports_matches(&self) -> bool {
        self.v || self.kv || self.p
    }

    /// Check for conflicting adverbs. Returns Err with X::Adverb if conflicts found.
    pub(crate) fn check_conflicts(&self, source: &str) -> Result<(), RuntimeError> {
        let mut active = Vec::new();
        if self.k {
            active.push("k");
        }
        if self.kv {
            active.push("kv");
        }
        if self.p {
            active.push("p");
        }
        if self.v {
            active.push("v");
        }
        if active.len() > 1 {
            use crate::symbol::Symbol;
            use std::collections::HashMap;
            let nogo: Vec<Value> = active.iter().map(|s| Value::str(s.to_string())).collect();
            let mut attrs = HashMap::new();
            attrs.insert("what".to_string(), Value::str("split".to_string()));
            attrs.insert("source".to_string(), Value::str(source.to_string()));
            attrs.insert("nogo".to_string(), Value::seq(nogo));
            let msg = format!(
                "Only one of :v, :k, :kv, :p may be specified on {}.split",
                source
            );
            attrs.insert("message".to_string(), Value::str(msg.clone()));
            let exc = Value::make_instance(Symbol::intern("X::Adverb"), attrs);
            let mut err = RuntimeError::new(msg);
            err.exception = Some(Box::new(exc));
            return Err(err);
        }
        Ok(())
    }
}

pub(crate) fn parse_split_limit(value: &Value) -> Result<Option<usize>, RuntimeError> {
    match value.view() {
        ValueView::Whatever | ValueView::HyperWhatever => return Ok(None),
        _ => {}
    }

    let numeric = value.to_f64();
    if numeric.is_nan() {
        let msg = "X::TypeCheck::Argument: split() limit must be an Int-compatible value, not NaN"
            .to_string();
        let mut attrs = std::collections::HashMap::new();
        attrs.insert("message".to_string(), Value::str(msg.clone()));
        let ex = Value::make_instance(Symbol::intern("X::TypeCheck::Argument"), attrs);
        let mut err = RuntimeError::new(msg);
        err.exception = Some(Box::new(ex));
        return Err(err);
    }
    if numeric.is_infinite() {
        return Ok(if numeric.is_sign_negative() {
            Some(0)
        } else {
            None
        });
    }

    let limit = crate::runtime::to_int(value);
    Ok(if limit <= 0 {
        Some(0)
    } else {
        Some(limit as usize)
    })
}

/// A split match: the matched separator text and which splitter index matched.
pub(crate) struct SplitMatch {
    pub from: usize,
    pub to: usize,
    /// The `Str` a string splitter reports for `:v`/`:kv`/`:p`. It is built
    /// once per split call and shared by every match (a refcount bump, not a
    /// copy of the separator per match, #19201). Unread when `match_obj` is
    /// set, so a regex splitter leaves it `Nil`.
    pub matched: Value,
    pub splitter_index: usize,
    /// Whether this match came from a regex splitter (affects :v/:kv/:p output).
    pub is_regex: bool,
    /// The fully-built separator `Match` for a regex splitter, carrying the
    /// span-bearing positional AND named captures the engine recorded. Only the
    /// runtime split paths hold the engine's `RegexCaptures`, so only they set
    /// it; a string splitter leaves it `None` and reports `matched` instead.
    pub match_obj: Option<Value>,
}

/// One piece of a split, before it becomes a `Str`.
pub(crate) enum Piece {
    /// A byte range of the split invocant (on `char` boundaries). It becomes a
    /// [`StrBody::Slice`] sharing the invocant's buffer, not a copy (#19201).
    Range(usize, usize),
    /// A piece built some other way (the codepoint-walking paths).
    Owned(String),
}

impl Piece {
    fn is_empty(&self) -> bool {
        match self {
            Piece::Range(start, end) => start == end,
            Piece::Owned(s) => s.is_empty(),
        }
    }

    /// The piece as a `Str`. `base` is the invocant the ranges index.
    // Cost: O(1) for a shared range; O(m) when [`StrBody::slice_of`] copies,
    // m = bytes of the piece.
    fn into_value(self, base: &Arc<StrBody>) -> Value {
        match self {
            Piece::Range(start, end) => Value::str_from_body(StrBody::slice_of(base, start, end)),
            Piece::Owned(s) => Value::str(s),
        }
    }
}

/// Split a string by a string splitter. Returns list of `(segment, Option<match>)`.
// Cost: O(n*m) worst, O(n + k) typical, n = bytes of the invocant, m = bytes
// of the separator, k = pieces produced (grapheme-boundary-aware search).
pub(crate) fn split_by_string(
    text: &str,
    sep: &str,
    limit: Option<usize>,
) -> Vec<(Piece, Option<SplitMatch>)> {
    if limit == Some(0) {
        return Vec::new();
    }
    let mut result = Vec::new();

    if text.is_empty() {
        if sep.is_empty() {
            return result;
        }
        result.push((Piece::Range(0, 0), None));
        return result;
    }

    if sep.is_empty() {
        // Only the empty separator walks codepoints; a non-empty one searches
        // the UTF-8 directly, so it must not pay for a char vector (#19201).
        let chars: Vec<char> = text.chars().collect();
        let max_splits = limit.map(|l| if l > 0 { l - 1 } else { 0 });
        let mut seg_start = 0;

        for (splits_done, match_pos) in (0..=chars.len()).enumerate() {
            if let Some(max) = max_splits
                && splits_done >= max
            {
                let remaining = Piece::Owned(chars[seg_start..].iter().collect());
                result.push((remaining, None));
                return result;
            }
            let segment = Piece::Owned(chars[seg_start..match_pos].iter().collect());
            result.push((
                segment,
                Some(SplitMatch {
                    from: match_pos,
                    to: match_pos,
                    // A non-regex zero-width match reports no separator.
                    matched: Value::NIL,
                    splitter_index: 0,
                    is_regex: false,
                    match_obj: None,
                }),
            ));
            seg_start = match_pos;
        }
        result.push((Piece::Range(0, 0), None));
        return result;
    }

    if let Some(parts) = split_ascii_by_string(text, sep, limit) {
        return parts;
    }
    let index = crate::builtins::grapheme_index::GraphemeIndex::build(text);
    let max_splits = limit.map(|l| if l > 0 { l - 1 } else { 0 });
    let mut splits_done = 0;
    let mut pos = 0;
    // The separator as reported by `:v`, built once and shared by every match.
    let sep_value = Value::str_from(sep);

    loop {
        if let Some(max) = max_splits
            && splits_done >= max
        {
            let remaining = Piece::Range(pos, text.len());
            result.push((remaining, None));
            return result;
        }

        match crate::builtins::grapheme_index::find_graphemes(text, &index, pos, sep) {
            Some(match_pos) => {
                let match_end = match_pos + sep.len();
                let from = index
                    .grapheme_at_boundary(text, match_pos)
                    .expect("grapheme search returns a boundary");
                let to = index
                    .grapheme_at_boundary(text, match_end)
                    .expect("grapheme search returns a boundary");
                let segment = Piece::Range(pos, match_pos);
                result.push((
                    segment,
                    Some(SplitMatch {
                        from,
                        to,
                        matched: sep_value.clone(),
                        splitter_index: 0,
                        is_regex: false,
                        match_obj: None,
                    }),
                ));
                pos = match_end;
                splits_done += 1;
            }
            None => {
                let remaining = Piece::Range(pos, text.len());
                result.push((remaining, None));
                return result;
            }
        }
    }
}

/// [`split_by_string`] for an ASCII invocant with no `\r` and a non-empty
/// separator, else `None`. Every char of such a text is a grapheme of its own
/// (`"\r\n"` is the only multi-char ASCII cluster), so a byte-level hit
/// always starts and ends on grapheme boundaries and a byte offset is the
/// grapheme index `:k`/`:v`/`:p` report: the grapheme index the general path
/// builds per call is not needed.
// Cost: O(n + k), n = bytes of the invocant, k = pieces produced.
fn split_ascii_by_string(
    text: &str,
    sep: &str,
    limit: Option<usize>,
) -> Option<Vec<(Piece, Option<SplitMatch>)>> {
    if !text.is_ascii() || text.as_bytes().contains(&b'\r') {
        return None;
    }
    let max_splits = limit.map(|l| l.saturating_sub(1));
    // A one-byte separator (`' '`, `','`) is searched as a char, which the
    // standard library answers with `memchr`.
    let sep_char = match sep.as_bytes() {
        [b] => Some(*b as char),
        _ => None,
    };
    let sep_value = Value::str_from(sep);
    let mut result = Vec::new();
    let mut pos = 0;
    let mut splits_done = 0;
    loop {
        if max_splits.is_some_and(|max| splits_done >= max) {
            result.push((Piece::Range(pos, text.len()), None));
            return Some(result);
        }
        let hit = match sep_char {
            Some(c) => text[pos..].find(c),
            None => text[pos..].find(sep),
        };
        let Some(off) = hit else {
            result.push((Piece::Range(pos, text.len()), None));
            return Some(result);
        };
        let from = pos + off;
        let to = from + sep.len();
        result.push((
            Piece::Range(pos, from),
            Some(SplitMatch {
                from,
                to,
                matched: sep_value.clone(),
                splitter_index: 0,
                is_regex: false,
                match_obj: None,
            }),
        ));
        pos = to;
        splits_done += 1;
    }
}

/// Split a string by multiple string splitters (list form).
///
/// At each cursor position the earliest occurrence of any splitter wins, the
/// longest splitter on a tie. Each splitter's next occurrence is cached and
/// only searched again once the cursor has moved past it, so every splitter
/// scans the invocant at most once over the whole call (a k-way
/// merge of the per-splitter find streams).
// Cost: O(n*m + k), n = bytes of the invocant, m = total bytes of the
// separators, k = pieces produced (grapheme-boundary-aware search per splitter).
pub(crate) fn split_by_strings(
    text: &str,
    splitters: &[String],
    limit: Option<usize>,
) -> Vec<(Piece, Option<SplitMatch>)> {
    if limit == Some(0) {
        return Vec::new();
    }
    let mut result = Vec::new();
    if text.is_empty() {
        result.push((Piece::Range(0, 0), None));
        return result;
    }

    let index = crate::builtins::grapheme_index::GraphemeIndex::build(text);
    let splitter_lengths: Vec<usize> = splitters.iter().map(|s| s.chars().count()).collect();
    // Each separator as reported by `:v`, built once per call and shared by its
    // matches rather than copied per match (#19201).
    let splitter_values: Vec<Value> = splitters.iter().map(|s| Value::str_from(s)).collect();
    // Per splitter: `Some(Some(p))` = next occurrence starts at `p`,
    // `Some(None)` = no occurrence at or after the last search start (so none
    // ever again), `None` = not searched yet. A cached `p` stays valid while
    // `p >= pos`: it is the first occurrence at or after an earlier cursor.
    let mut next: Vec<Option<Option<usize>>> = vec![None; splitters.len()];
    let max_splits = limit.map(|l| if l > 0 { l - 1 } else { 0 });
    let mut splits_done = 0;
    let mut pos = 0;

    loop {
        if let Some(max) = max_splits
            && splits_done >= max
        {
            let remaining = Piece::Range(pos, text.len());
            result.push((remaining, None));
            return result;
        }

        let mut best: Option<(usize, usize, usize)> = None;
        for (idx, sep) in splitters.iter().enumerate() {
            let sep_len = splitter_lengths[idx];
            if sep_len == 0 {
                continue;
            }
            let found = match next[idx] {
                Some(Some(p)) if p >= pos => Some(p),
                Some(None) => None,
                _ => {
                    let f = crate::builtins::grapheme_index::find_graphemes(text, &index, pos, sep);
                    next[idx] = Some(f);
                    f
                }
            };
            if let Some(start) = found {
                let is_better = match best {
                    None => true,
                    Some((bp, bl, _)) => start < bp || (start == bp && sep_len > bl),
                };
                if is_better {
                    best = Some((start, sep_len, idx));
                }
            }
        }

        match best {
            Some((match_pos, _match_len, splitter_idx)) => {
                let separator = &splitters[splitter_idx];
                let match_end = match_pos + separator.len();
                let from = index
                    .grapheme_at_boundary(text, match_pos)
                    .expect("grapheme search returns a boundary");
                let to = index
                    .grapheme_at_boundary(text, match_end)
                    .expect("grapheme search returns a boundary");
                let segment = Piece::Range(pos, match_pos);
                result.push((
                    segment,
                    Some(SplitMatch {
                        from,
                        to,
                        matched: splitter_values[splitter_idx].clone(),
                        splitter_index: splitter_idx,
                        is_regex: false,
                        match_obj: None,
                    }),
                ));
                pos = match_end;
                splits_done += 1;
            }
            None => {
                let remaining = Piece::Range(pos, text.len());
                result.push((remaining, None));
                return result;
            }
        }
    }
}

/// Create a separator value: the engine-built Match object for regex splits,
/// the matched text for string splits.
fn separator_value(m: &SplitMatch) -> Value {
    match &m.match_obj {
        Some(obj) => obj.clone(),
        None => m.matched.clone(),
    }
}

/// Insert separator information based on named param options.
fn push_separator_info(result: &mut Vec<Value>, m: &SplitMatch, opts: &SplitOpts) {
    // For non-regex zero-width matches (empty string separator), don't insert separator info
    if m.from == m.to && !m.is_regex {
        return;
    }
    if opts.v {
        result.push(separator_value(m));
    } else if opts.k {
        result.push(Value::int(m.splitter_index as i64));
    } else if opts.kv {
        result.push(Value::int(m.splitter_index as i64));
        result.push(separator_value(m));
    } else if opts.p {
        result.push(Value::value_pair(
            Value::int(m.splitter_index as i64),
            separator_value(m),
        ));
    }
}

/// Apply split options to raw split results.
// Cost: O(k), k = pieces: each piece shares the invocant's buffer (no copy).
pub(crate) fn apply_split_opts(
    parts: Vec<(Piece, Option<SplitMatch>)>,
    opts: &SplitOpts,
    base: &Arc<StrBody>,
) -> Vec<Value> {
    // Without :v/:k/:kv/:p there is one value per piece; with them, at most
    // two more per separator, so the first case is sized exactly.
    let separators_reported = opts.v || opts.k || opts.kv || opts.p;
    let mut result = Vec::with_capacity(if separators_reported {
        parts.len() * 3
    } else {
        parts.len()
    });

    for (segment, match_info) in parts {
        if opts.skip_empty && segment.is_empty() {
            if let Some(m) = &match_info {
                push_separator_info(&mut result, m, opts);
            }
            continue;
        }

        result.push(segment.into_value(base));

        if let Some(m) = &match_info {
            push_separator_info(&mut result, m, opts);
        }
    }

    result
}

/// Get the list of string splitters from a value (single string or array of strings).
/// Returns None if the splitter is a regex or contains regexes.
fn get_string_splitters(splitter: &Value) -> Option<Vec<String>> {
    match splitter.view() {
        ValueView::Regex(_) | ValueView::RegexWithAdverbs(..) | ValueView::Sub(..) => None,
        ValueView::Array(items, _) => {
            let mut strings = Vec::new();
            for item in items.iter() {
                match item.view() {
                    ValueView::Regex(_) | ValueView::RegexWithAdverbs(..) | ValueView::Sub(..) => {
                        return None;
                    }
                    _ => strings.push(item.to_string_value()),
                }
            }
            Some(strings)
        }
        _ => Some(vec![splitter.to_string_value()]),
    }
}

/// Perform string split as function: split($splitter, $string, ...).
/// Returns None if splitter contains regex.
pub(crate) fn native_split_function(args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    let (positional, mut opts) = SplitOpts::from_args(args);
    if positional.len() < 2 {
        return Some(Err(RuntimeError::new(
            "split requires at least 2 arguments: splitter and string",
        )));
    }

    let splitter = positional[0];
    let target = positional[1];
    if positional.len() > 2 {
        opts.limit = match parse_split_limit(positional[2]) {
            Ok(limit) => limit,
            Err(err) => return Some(Err(err)),
        };
    }

    let splitters = get_string_splitters(splitter)?;
    let base = crate::value::str_iter::shared_text(target);
    let text: &str = &base;

    let parts = if splitters.len() == 1 {
        split_by_string(text, &splitters[0], opts.limit)
    } else {
        split_by_strings(text, &splitters, opts.limit)
    };

    let result = apply_split_opts(parts, &opts, &base);
    Some(Ok(Value::seq(result)))
}
