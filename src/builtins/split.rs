#![allow(clippy::result_large_err)]

use crate::symbol::Symbol;
use crate::value::{RuntimeError, Value};
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
    pub(crate) fn from_args(args: &[Value]) -> (Vec<&Value>, SplitOpts) {
        let mut opts = SplitOpts::default();
        let mut positional = Vec::new();
        for arg in args {
            match arg {
                Value::Pair(key, value) if key == "v" => opts.v = value.truthy(),
                Value::Pair(key, value) if key == "k" => opts.k = value.truthy(),
                Value::Pair(key, value) if key == "kv" => opts.kv = value.truthy(),
                Value::Pair(key, value) if key == "p" => opts.p = value.truthy(),
                Value::Pair(key, value) if key == "skip-empty" || key == "skip_empty" => {
                    opts.skip_empty = value.truthy()
                }
                _ => positional.push(arg),
            }
        }
        (positional, opts)
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
            attrs.insert("nogo".to_string(), Value::Seq(Arc::new(nogo)));
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
    match value {
        Value::Whatever | Value::HyperWhatever => return Ok(None),
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
#[allow(dead_code)]
pub(crate) struct SplitMatch {
    pub from: usize,
    pub to: usize,
    pub matched: String,
    pub splitter_index: usize,
    /// Whether this match came from a regex splitter (affects :v/:kv/:p output).
    pub is_regex: bool,
    /// The original string being split (needed for Match object construction).
    pub orig: String,
    /// Positional captures from regex match (for :v Match objects).
    pub positional_captures: Vec<String>,
}

/// Split a string by a string splitter. Returns list of (segment, Option<match>).
fn split_by_string(
    text: &str,
    sep: &str,
    limit: Option<usize>,
) -> Vec<(String, Option<SplitMatch>)> {
    if limit == Some(0) {
        return Vec::new();
    }
    let mut result = Vec::new();
    let chars: Vec<char> = text.chars().collect();

    if text.is_empty() {
        if sep.is_empty() {
            return result;
        }
        result.push((String::new(), None));
        return result;
    }

    if sep.is_empty() {
        let max_splits = limit.map(|l| if l > 0 { l - 1 } else { 0 });
        let mut seg_start = 0;

        for (splits_done, match_pos) in (0..=chars.len()).enumerate() {
            if let Some(max) = max_splits
                && splits_done >= max
            {
                let remaining: String = chars[seg_start..].iter().collect();
                result.push((remaining, None));
                return result;
            }
            let segment: String = chars[seg_start..match_pos].iter().collect();
            result.push((
                segment,
                Some(SplitMatch {
                    from: match_pos,
                    to: match_pos,
                    matched: String::new(),
                    splitter_index: 0,
                    is_regex: false,
                    orig: String::new(),
                    positional_captures: Vec::new(),
                }),
            ));
            seg_start = match_pos;
        }
        result.push((String::new(), None));
        return result;
    }

    let sep_chars: Vec<char> = sep.chars().collect();
    let sep_len = sep_chars.len();
    let max_splits = limit.map(|l| if l > 0 { l - 1 } else { 0 });
    let mut splits_done = 0;
    let mut pos = 0;

    loop {
        if let Some(max) = max_splits
            && splits_done >= max
        {
            let remaining: String = chars[pos..].iter().collect();
            result.push((remaining, None));
            return result;
        }

        let mut found = None;
        if pos + sep_len <= chars.len() {
            for start in pos..=(chars.len() - sep_len) {
                if chars[start..start + sep_len] == sep_chars[..] {
                    found = Some(start);
                    break;
                }
            }
        }

        match found {
            Some(match_pos) => {
                let segment: String = chars[pos..match_pos].iter().collect();
                result.push((
                    segment,
                    Some(SplitMatch {
                        from: match_pos,
                        to: match_pos + sep_len,
                        matched: sep.to_string(),
                        splitter_index: 0,
                        is_regex: false,
                        orig: String::new(),
                        positional_captures: Vec::new(),
                    }),
                ));
                pos = match_pos + sep_len;
                splits_done += 1;
            }
            None => {
                let remaining: String = chars[pos..].iter().collect();
                result.push((remaining, None));
                return result;
            }
        }
    }
}

/// Split a string by multiple string splitters (list form).
fn split_by_strings(
    text: &str,
    splitters: &[String],
    limit: Option<usize>,
) -> Vec<(String, Option<SplitMatch>)> {
    if limit == Some(0) {
        return Vec::new();
    }
    let mut result = Vec::new();
    let chars: Vec<char> = text.chars().collect();

    if text.is_empty() {
        result.push((String::new(), None));
        return result;
    }

    let splitter_chars: Vec<Vec<char>> = splitters.iter().map(|s| s.chars().collect()).collect();
    let max_splits = limit.map(|l| if l > 0 { l - 1 } else { 0 });
    let mut splits_done = 0;
    let mut pos = 0;

    loop {
        if let Some(max) = max_splits
            && splits_done >= max
        {
            let remaining: String = chars[pos..].iter().collect();
            result.push((remaining, None));
            return result;
        }

        let mut best: Option<(usize, usize, usize)> = None;
        for (idx, sep_chars) in splitter_chars.iter().enumerate() {
            let sep_len = sep_chars.len();
            if sep_len == 0 {
                continue;
            }
            if pos + sep_len <= chars.len() {
                for start in pos..=(chars.len() - sep_len) {
                    if chars[start..start + sep_len] == sep_chars[..] {
                        let is_better = match best {
                            None => true,
                            Some((bp, bl, _)) => start < bp || (start == bp && sep_len > bl),
                        };
                        if is_better {
                            best = Some((start, sep_len, idx));
                        }
                        break;
                    }
                }
            }
        }

        match best {
            Some((match_pos, match_len, splitter_idx)) => {
                let segment: String = chars[pos..match_pos].iter().collect();
                let matched: String = chars[match_pos..match_pos + match_len].iter().collect();
                result.push((
                    segment,
                    Some(SplitMatch {
                        from: match_pos,
                        to: match_pos + match_len,
                        matched,
                        splitter_index: splitter_idx,
                        is_regex: false,
                        orig: String::new(),
                        positional_captures: Vec::new(),
                    }),
                ));
                pos = match_pos + match_len;
                splits_done += 1;
            }
            None => {
                let remaining: String = chars[pos..].iter().collect();
                result.push((remaining, None));
                return result;
            }
        }
    }
}

/// Create a separator value: Match object for regex splits, string for string splits.
fn separator_value(m: &SplitMatch) -> Value {
    if m.is_regex {
        use std::collections::HashMap;
        let orig = if m.orig.is_empty() {
            None
        } else {
            Some(m.orig.as_str())
        };
        Value::make_match_object_full(
            m.matched.clone(),
            m.from as i64,
            m.to as i64,
            &m.positional_captures,
            &HashMap::new(),
            &HashMap::new(),
            &[],
            &[],
            orig,
        )
    } else {
        Value::str(m.matched.clone())
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
        result.push(Value::Int(m.splitter_index as i64));
    } else if opts.kv {
        result.push(Value::Int(m.splitter_index as i64));
        result.push(separator_value(m));
    } else if opts.p {
        result.push(Value::ValuePair(
            Box::new(Value::Int(m.splitter_index as i64)),
            Box::new(separator_value(m)),
        ));
    }
}

/// Apply split options to raw split results.
pub(crate) fn apply_split_opts(
    parts: Vec<(String, Option<SplitMatch>)>,
    opts: &SplitOpts,
) -> Vec<Value> {
    let mut result = Vec::new();

    for (segment, match_info) in &parts {
        if opts.skip_empty && segment.is_empty() {
            if let Some(m) = match_info {
                push_separator_info(&mut result, m, opts);
            }
            continue;
        }

        result.push(Value::str(segment.clone()));

        if let Some(m) = match_info {
            push_separator_info(&mut result, m, opts);
        }
    }

    result
}

/// Get the list of string splitters from a value (single string or array of strings).
/// Returns None if the splitter is a regex or contains regexes.
fn get_string_splitters(splitter: &Value) -> Option<Vec<String>> {
    match splitter {
        Value::Regex(_) | Value::RegexWithAdverbs { .. } | Value::Sub { .. } => None,
        Value::Array(items, _) => {
            let mut strings = Vec::new();
            for item in items.iter() {
                match item {
                    Value::Regex(_) | Value::RegexWithAdverbs { .. } | Value::Sub { .. } => {
                        return None;
                    }
                    other => strings.push(other.to_string_value()),
                }
            }
            Some(strings)
        }
        other => Some(vec![other.to_string_value()]),
    }
}

/// Perform string split (no regex). Returns None if splitter contains regex.
pub(crate) fn native_split_method(
    target: &Value,
    args: &[Value],
) -> Option<Result<Value, RuntimeError>> {
    let (positional, mut opts) = SplitOpts::from_args(args);
    if positional.is_empty() {
        return Some(Err(RuntimeError::new("Must specify a pattern for split")));
    }

    if let Err(e) = opts.check_conflicts("Str") {
        return Some(Err(e));
    }

    let splitter = positional[0];
    if positional.len() > 1 {
        opts.limit = Some(positional[1].to_f64().max(0.0) as usize);
    }

    let splitters = get_string_splitters(splitter)?;
    let text = target.to_string_value();

    let parts = if splitters.len() == 1 {
        split_by_string(&text, &splitters[0], opts.limit)
    } else {
        split_by_strings(&text, &splitters, opts.limit)
    };

    let result = apply_split_opts(parts, &opts);
    Some(Ok(Value::Seq(Arc::new(result))))
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
    let text = target.to_string_value();

    let parts = if splitters.len() == 1 {
        split_by_string(&text, &splitters[0], opts.limit)
    } else {
        split_by_strings(&text, &splitters, opts.limit)
    };

    let result = apply_split_opts(parts, &opts);
    Some(Ok(Value::Seq(Arc::new(result))))
}
