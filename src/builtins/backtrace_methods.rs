//! `Backtrace` rendering and introspection.
//!
//! Everything here works off the `frames` attribute a `Backtrace` instance
//! carries (a List of `Backtrace::Frame` instances built by
//! `vm::vm_helpers::build_backtrace_value`). Nothing re-derives a frame list
//! from the interpreter state, so every method here agrees with `$bt[N]`,
//! `.list` and `.elems` by construction.
//!
//! ## How mutsu's frame model differs from Rakudo's
//!
//! Rakudo's frame 0 is always a CORE-setting frame (`Exception.throw` /
//! `Backtrace.new`), and its `Backtrace` filters those out when rendering.
//! mutsu has no Raku-written setting -- its `die`/`throw` are Rust functions
//! with no callframe -- so frame 0 is already the innermost *user* frame and
//! `.is-setting` / `.is-hidden` are uniformly `False`. The filters below are
//! still written in terms of those predicates (so they keep working if mutsu
//! ever grows hidden frames), but the *entry point* differs: `nice` starts at
//! index 0 inclusive, where Rakudo starts at index 1. See
//! `todo/tickets/backtrace-frame-indexing-returns-nil.md` for why matching
//! Rakudo's absolute frame count is deliberately not attempted.

use crate::gc::Gc;
use crate::symbol::Symbol;
use crate::value::{InstanceAttrs, RuntimeError, Value, ValueView};

/// Render a `Backtrace::Frame` instance as its `.Str`/`.gist` text.
///
/// Rakudo's `Backtrace::Frame.Str` is newline-**terminated**, which is what
/// makes `Backtrace.full`/`.concise`/`.summary` -- plain concatenations of the
/// frame strings -- come out one frame per line. Shared by the frame's own
/// `.Str` handler and by those three renderings so the natively-computed
/// strings stay byte-identical to a `.grep(...).join`.
pub(crate) fn frame_str(attributes: &Gc<InstanceAttrs>) -> String {
    let map = attributes.as_map();
    let subname = map
        .get("subname")
        .map(|v| v.to_string_value())
        .unwrap_or_default();
    let file = map
        .get("file")
        .map(|v| v.to_string_value())
        .unwrap_or_default();
    let line = map
        .get("line")
        .map(|v| v.to_string_value())
        .unwrap_or_else(|| "0".to_string());
    format!("{}\n", frame_entry(&subname, &file, &line))
}

/// The un-terminated `  in <subtype> <name> at <file> line <n>` body.
///
/// A frame with no subname is an anonymous block, and Rakudo renders it as
/// `  in block  at ...` (an empty name after the subtype), not as a `sub`.
fn frame_entry(subname: &str, file: &str, line: &str) -> String {
    if subname == "<unit>" || subname.is_empty() {
        format!("  in block {} at {} line {}", subname, file, line)
    } else {
        format!("  in sub {} at {} line {}", subname, file, line)
    }
}

/// The `Backtrace::Frame.code` value: mutsu does not retain the actual
/// routine a frame points into, so this synthesizes a `Routine` carrying just
/// the frame's `subname` (`.code.name` is the documented use). Shared by the
/// `code` accessor and the `.raku`/`.gist` renderer so both describe the same
/// object.
pub(crate) fn frame_code_value(attributes: &Gc<InstanceAttrs>) -> Value {
    let subname = attributes
        .as_map()
        .get("subname")
        .map(|v| v.to_string_value())
        .unwrap_or_default();
    Value::routine_parts(Symbol::intern("GLOBAL"), Symbol::intern(&subname), false)
}

/// A `Backtrace::Frame` is a "routine" frame when it has a real subname
/// (not the synthetic `<unit>` bottom frame and not an anonymous block).
pub(crate) fn frame_is_routine(attributes: &Gc<InstanceAttrs>) -> bool {
    let subname = attributes
        .as_map()
        .get("subname")
        .map(|v| v.to_string_value())
        .unwrap_or_default();
    !subname.is_empty() && subname != "<unit>"
}

/// The `frames` list of a `Backtrace` instance.
pub(crate) fn frames_of(attributes: &Gc<InstanceAttrs>) -> Vec<Value> {
    attributes
        .as_map()
        .get("frames")
        .map(crate::runtime::utils::value_to_list)
        .unwrap_or_default()
}

fn frame_field(frame: &Value, key: &str) -> String {
    match frame.view() {
        ValueView::Instance { attributes, .. } => attributes
            .as_map()
            .get(key)
            .map(|v| v.to_string_value())
            .unwrap_or_default(),
        _ => String::new(),
    }
}

fn subname_of(frame: &Value) -> String {
    frame_field(frame, "subname")
}

fn is_routine(frame: &Value) -> bool {
    match frame.view() {
        ValueView::Instance { attributes, .. } => frame_is_routine(&attributes),
        _ => false,
    }
}

/// mutsu tracks neither `is hidden-from-backtrace` routines nor CORE-setting
/// frames, so both predicates are uniformly false -- the same answer
/// `Backtrace::Frame.is-hidden` / `.is-setting` give.
fn is_hidden(_frame: &Value) -> bool {
    false
}

fn is_setting(_frame: &Value) -> bool {
    false
}

/// `Backtrace.next-interesting-index(Int $idx = 0, :$named, :$noproto, :$setting)`.
///
/// Scans **forward from `$idx + 1`** (Rakudo increments before looking, so the
/// starting index itself is never a candidate) for the first frame that is not
/// filtered out, and returns `None` when the list is exhausted -- `Nil` at the
/// Raku level.
///
/// `:$named` keeps only frames with a name (an anonymous block has an empty
/// subname; the `<unit>` frame counts as named, matching Rakudo, where
/// `.code.name` is the literal string `<unit>`). `:$setting` *includes*
/// setting frames rather than hiding them. `:$noproto` would hide `proto`
/// dispatchers; mutsu's frames record no dispatcher bit, so it is accepted and
/// hides nothing.
pub(crate) fn next_interesting_index(
    frames: &[Value],
    from: i64,
    named: bool,
    setting: bool,
) -> Option<usize> {
    let mut i = from.saturating_add(1).max(0);
    while (i as usize) < frames.len() {
        let frame = &frames[i as usize];
        let filtered = is_hidden(frame)
            || (named && subname_of(frame).is_empty())
            || (!setting && is_setting(frame));
        if !filtered {
            return Some(i as usize);
        }
        i += 1;
    }
    None
}

/// `Backtrace.outer-caller-idx(Int $startidx)` -- the indices of the frames
/// that lexically enclose (and therefore called) the frame at `$startidx`.
///
/// Rakudo walks the `.code.outer` chain and matches it against the frames
/// below `$startidx`, stopping once it has included one routine frame. mutsu's
/// frames record no lexical `.outer` link, so the chain is reconstructed from
/// the dynamic stack, which coincides with it for the shapes that matter:
///
/// * an anonymous block is entered from its enclosing scope, so its callers
///   are the immediately following frames, up to and including the first
///   routine -- the routine that contains the block;
/// * a declared routine's enclosing scope is the compilation unit, so its
///   caller is the outermost `<unit>` frame rather than whichever routine
///   happened to invoke it (verified against `raku`: for
///   `sub a { b() }; sub b { { die }() }`, `outer-caller-idx` of `b`'s frame
///   is the `<unit>` frame, not `a`'s).
///
/// An out-of-range index answers `($startidx,)` and a negative one `()`, both
/// matching Rakudo.
pub(crate) fn outer_caller_idx(frames: &[Value], start: i64) -> Vec<usize> {
    let len = frames.len() as i64;
    if start < 0 {
        return Vec::new();
    }
    if start >= len {
        return vec![start as usize];
    }
    if start == len - 1 {
        return Vec::new();
    }
    let last = (len - 1) as usize;
    if is_routine(&frames[start as usize]) {
        return if subname_of(&frames[last]) == "<unit>" {
            vec![last]
        } else {
            Vec::new()
        };
    }
    let mut out = Vec::new();
    for (offset, frame) in frames.iter().enumerate().skip(start as usize + 1) {
        out.push(offset);
        if is_routine(frame) {
            break;
        }
    }
    out
}

/// Render one `nice` entry for the frame at `idx`.
///
/// An anonymous frame borrows its enclosing scope's name (Rakudo does the
/// same, which is how `  in block <unit> at ...` gets its name), and reports
/// the index that name came from so the caller can resume the scan past it.
fn nice_entry(frames: &[Value], idx: usize) -> (String, Option<usize>) {
    let frame = &frames[idx];
    let file = frame_field(frame, "file");
    let line = frame_field(frame, "line");
    let subname = subname_of(frame);
    if subname.is_empty()
        && let Some(&caller) = outer_caller_idx(frames, idx as i64).first()
    {
        let name = subname_of(&frames[caller]);
        return (
            format!("{}\n", frame_entry(&name, &file, &line)),
            Some(caller),
        );
    }
    (format!("{}\n", frame_entry(&subname, &file, &line)), None)
}

/// `Backtrace.nice(:$oneline)` -- the "interesting" frames, one per line.
///
/// Without `:oneline` this walks every interesting frame from the first one.
/// With it, exactly one entry is rendered, starting at the **second**
/// interesting frame: `nice(:oneline)` names where the innermost frame was
/// *called from*, which is what makes `  in sub zipi at ... line 1` (rather
/// than the block that actually died) the documented answer for the
/// `sub zipi { { { die ... }() }() }` example. Verified against `raku` across
/// five backtrace shapes. A backtrace with a single frame has no second one to
/// report and falls back to that frame.
pub(crate) fn nice(frames: &[Value], oneline: bool) -> String {
    // mutsu's frame 0 is already user code (no setting frames), so the scan
    // starts one before it -- `next_interesting_index` looks at `from + 1`.
    let Some(first) = next_interesting_index(frames, -1, false, false) else {
        return String::new();
    };
    let start = if oneline {
        next_interesting_index(frames, first as i64, false, false).unwrap_or(first)
    } else {
        first
    };

    let mut out = String::new();
    let mut idx = start;
    loop {
        let (entry, consumed) = nice_entry(frames, idx);
        out.push_str(&entry);
        if oneline {
            break;
        }
        let after = consumed.unwrap_or(idx);
        match next_interesting_index(frames, after as i64, false, false) {
            Some(next) => idx = next,
            None => break,
        }
    }
    out
}

/// The named arguments a `Backtrace` introspection call may carry, plus its
/// single optional positional. Named-ness is a call-site property (ADR-0021):
/// only a string-keyed `Pair` in the argument list is a named argument.
struct BacktraceArgs {
    positional: Option<i64>,
    named: bool,
    setting: bool,
    oneline: bool,
}

fn parse_args(args: &[Value]) -> BacktraceArgs {
    let mut parsed = BacktraceArgs {
        positional: None,
        named: false,
        setting: false,
        oneline: false,
    };
    for arg in args {
        if arg.is_string_pair_value() {
            let (key, value) = match arg.view() {
                ValueView::Pair(k, v) => (k.to_string(), v.truthy()),
                ValueView::ValuePair(k, v) => (k.to_string_value(), v.truthy()),
                _ => continue,
            };
            match key.as_str() {
                "named" => parsed.named = value,
                "setting" => parsed.setting = value,
                "oneline" => parsed.oneline = value,
                // `:noproto` hides `proto` dispatchers; mutsu's frames carry
                // no dispatcher bit, so it is accepted and hides nothing.
                _ => {}
            }
        } else if parsed.positional.is_none()
            && let ValueView::Int(i) = arg.descalarize().view()
        {
            parsed.positional = Some(i);
        }
    }
    parsed
}

/// Dispatch the three `Backtrace` introspection methods at any arity.
///
/// Returns `None` when `method` is not one of them, so each arity cascade can
/// fall through to its remaining arms.
pub(crate) fn dispatch(
    attributes: &Gc<InstanceAttrs>,
    method: &str,
    args: &[Value],
) -> Option<Result<Value, RuntimeError>> {
    match method {
        "nice" | "next-interesting-index" | "outer-caller-idx" => {}
        _ => return None,
    }
    let frames = frames_of(attributes);
    let parsed = parse_args(args);
    Some(Ok(match method {
        "nice" => Value::str(nice(&frames, parsed.oneline)),
        "next-interesting-index" => {
            let from = parsed.positional.unwrap_or(0);
            match next_interesting_index(&frames, from, parsed.named, parsed.setting) {
                Some(idx) => Value::int(idx as i64),
                None => Value::NIL,
            }
        }
        // Rakudo answers `outer-caller-idx` with an `Array` (`[6]`), not a
        // `List`, so build the itemizable kind.
        _ => Value::real_array(
            outer_caller_idx(&frames, parsed.positional.unwrap_or(0))
                .into_iter()
                .map(|i| Value::int(i as i64))
                .collect(),
        ),
    }))
}
