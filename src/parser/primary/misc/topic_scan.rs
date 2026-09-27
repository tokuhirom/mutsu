//! The lexical scans behind the `{ … }` hash-composer-vs-block decision:
//! does a brace body reference the topic (`$_`, `.method`) or declare a
//! placeholder (`$^a`)? Either forces a Block even when the body otherwise
//! looks like a hash composer. Split out of `lambda.rs`, whose
//! `body_is_hash_composer` is the only consumer.

/// Whether a `{ … }` body references the topic `$_` (directly as `$_`/`@_`/`%_`
/// or via a `.^`/`.?` metamethod on the implicit topic). Such a body is a
/// closure/block even when it otherwise looks like a hash composer (`{ "$_" =>
/// … }`, `{ "{ .^name }X" => … }`), matching rakudo: the topic reference forces
/// a block, while a topic-free `{ "{$v}B" => 1 }` stays a hash. `$_` inside a
/// double-quoted string still counts (it interpolates the topic), so — unlike
/// the placeholder scan — double quotes are NOT skipped; single quotes are.
pub(super) fn body_references_topic(input: &str) -> bool {
    let bytes = input.as_bytes();
    let mut i = 0usize;
    // `depth` counts *bare* `{ … }` nesting; `depth == 1` is the immediate block
    // body. Only an *explicit* topic variable `$_`/`@_`/`%_` at the top level of
    // this body (in the key or the value, including inside a double-quoted
    // string) forces a block. An implicit-topic method call such as `.^name` or
    // `.Str` does NOT: rakudo keeps `{ "{ .^name }X" => 1 }` a Hash but makes
    // `{ "$_" => 1 }` / `{ a => $_ }` a Block. A `$_` inside a nested bare block
    // (`{ foo => (1,2,3).map: {$_} }`) belongs to that inner block's own topic,
    // so `depth > 1` references never count. String-interpolation braces are not
    // nesting, so the string stays at the current depth.
    let mut depth = 1u32;
    let mut in_d = false;
    while i < bytes.len() {
        let c = bytes[i];
        if in_d {
            match c {
                b'\\' => {
                    i += 2;
                    continue;
                }
                b'"' => in_d = false,
                b'$' | b'@' | b'%'
                    if depth == 1
                        && bytes.get(i + 1) == Some(&b'_')
                        && !bytes
                            .get(i + 2)
                            .is_some_and(|c| c.is_ascii_alphanumeric() || *c == b'_') =>
                {
                    return true;
                }
                _ => {}
            }
            i += 1;
            continue;
        }
        match c {
            b'"' => in_d = true,
            b'{' => depth += 1,
            b'}' => {
                depth -= 1;
                if depth == 0 {
                    break;
                }
            }
            // Single-quoted strings do not interpolate: skip their contents.
            b'\'' => {
                i += 1;
                while i < bytes.len() && bytes[i] != b'\'' {
                    i += 1;
                }
            }
            // A `< … >` word list is literal text too: the `.abs` in
            // `{ :parts< .abs > }` is a word, not a topic call.
            // `<->` opens an rw pointy signature, not a word list.
            b'<' if depth == 1 && bytes[i + 1..].starts_with(b"->") => {
                if let Some(end) = nested_signature_end(bytes, i + 1) {
                    i = end;
                    continue;
                }
            }
            b'<' => {
                if let Some(close) = angle_words_end(bytes, i) {
                    i = close;
                }
            }
            // A nested routine's or pointy block's signature declares (and
            // defaults) its own parameters: the `$_` in `{ a => sub ($_ =
            // Whatever) { … } }` or `{ a => -> $_ { … } }` is that closure's,
            // so rakudo keeps both hashes. Skip the signature; its body brace
            // is then ordinary nesting.
            b's' | b'-' if depth == 1 => {
                if let Some(end) = nested_signature_end(bytes, i) {
                    i = end;
                    continue;
                }
            }
            // `$_` / `@_` / `%_` topic variable at the top level of this body,
            // with a trailing word boundary (so `$_x` — an ordinary name — does
            // not match). Never inside a nested bare block.
            b'$' | b'@' | b'%'
                if depth == 1
                    && bytes.get(i + 1) == Some(&b'_')
                    && !bytes
                        .get(i + 2)
                        .is_some_and(|c| c.is_ascii_alphanumeric() || *c == b'_') =>
            {
                return true;
            }
            // An invocant-less method call (`.key`, `.^name`) is a topic
            // reference too: rakudo keeps `{ .key => 1 }` and `{ a => .key }`
            // blocks. Only outside a string — a `.^name` inside an
            // interpolation belongs to that interpolation's own closure, which
            // is why `{ "{ .^name }X" => 1 }` stays a hash.
            b'.' if depth == 1 && is_implicit_topic_call(bytes, i) => return true,
            _ => {}
        }
        i += 1;
    }
    false
}

/// If `at` starts an anonymous `sub (…)` or a pointy `-> …` signature, the
/// index just past that signature: after the closing `)` for `sub`, at the
/// body's `{` for a pointy block. Gives up (`None`) at a `;` or an unbalanced
/// closer, so a misread can never swallow the rest of the body.
fn nested_signature_end(bytes: &[u8], at: usize) -> Option<usize> {
    let is_ident = |c: u8| c.is_ascii_alphanumeric() || c == b'_' || c == b'-';
    let pointy = bytes[at..].starts_with(b"->");
    if !pointy {
        if !bytes[at..].starts_with(b"sub")
            || (at > 0 && is_ident(bytes[at - 1]))
            || bytes.get(at + 3).is_some_and(|&c| is_ident(c))
        {
            return None;
        }
        let mut j = at + 3;
        while bytes.get(j).is_some_and(|c| c.is_ascii_whitespace()) {
            j += 1;
        }
        if bytes.get(j) != Some(&b'(') {
            return None;
        }
    }
    let mut j = at + if pointy { 2 } else { 3 };
    let mut nest = 0u32;
    while let Some(&c) = bytes.get(j) {
        match c {
            b'(' | b'[' => nest += 1,
            b')' | b']' => {
                nest = nest.checked_sub(1)?;
                if nest == 0 && !pointy {
                    return Some(j + 1);
                }
            }
            b'{' if nest == 0 => return pointy.then_some(j),
            b'{' | b'}' | b';' => return None,
            b'\'' | b'"' => {
                j += 1;
                while bytes.get(j).is_some_and(|&q| q != c) {
                    j += 1;
                }
            }
            _ => {}
        }
        j += 1;
    }
    None
}

/// If the `<` at `at` opens a `< … >` word list, the index of its closing `>`.
///
/// A `<` opens a word list when it is glued to a term (`%h<k>`, the colonpair
/// value in `:parts<a b>`) or stands where a term is expected (`a => <x y>`,
/// `(<a b>)`); after a term and whitespace it is infix less-than (`$a < 3`).
/// `<<`, `<=`, `<=>` and `<==` are operators. The scan gives up (returning
/// `None`, i.e. "not a word list") at a brace or `;` before any `>`, so a
/// misread `<` can never swallow the rest of the body.
fn angle_words_end(bytes: &[u8], at: usize) -> Option<usize> {
    if matches!(bytes.get(at + 1), Some(b'<' | b'=')) {
        return None;
    }
    let is_term_end =
        |c: u8| c.is_ascii_alphanumeric() || matches!(c, b'_' | b')' | b']' | b'}' | b'\'' | b'"');
    let glued = at > 0 && !bytes[at - 1].is_ascii_whitespace();
    if !glued {
        let mut p = at;
        while p > 0 && bytes[p - 1].is_ascii_whitespace() {
            p -= 1;
        }
        if let Some(prev) = p.checked_sub(1).map(|q| bytes[q]) {
            let after_fat_arrow = prev == b'>' && p >= 2 && bytes[p - 2] == b'=';
            if is_term_end(prev) || (prev == b'>' && !after_fat_arrow) {
                return None;
            }
        }
    }
    let close = at
        + 1
        + bytes[at + 1..]
            .iter()
            .position(|c| matches!(c, b'>' | b'{' | b'}' | b';'))?;
    (bytes[close] == b'>').then_some(close)
}

/// Whether the `.` at `at` starts a method call with no invocant before it,
/// i.e. one that takes `$_` as its invocant.
fn is_implicit_topic_call(bytes: &[u8], at: usize) -> bool {
    // A name (`.key`, `.^name`, `.?maybe`, `.&f`) or a postcircumfix
    // (`.[0]`, `.{'k'}`, `.<k>`, `.(1)`). `.5` is a number, `..`/`...` a range
    // and `.=` an assignment metaop, so none of those qualify.
    let starts_call = bytes.get(at + 1).is_some_and(|c| {
        c.is_ascii_alphabetic()
            || matches!(c, b'_' | b'^' | b'?' | b'&' | b'[' | b'{' | b'<' | b'(')
    });
    if !starts_call {
        return false;
    }
    // `$.attr` / `@.attr` / `%.attr` / `&.attr` is the ATTRIBUTE twigil, a term
    // whose invocant is `self` -- not an implicit-topic call, so rakudo keeps
    // `{ a => $.g }` a Hash (checked for all four sigils). The sigil has to be
    // glued to the dot: an infix `%` or `&` before a real topic call is
    // separated from it by whitespace (`{a => 2 % .elems}` is a Block), and
    // that spelling falls through to the general rules below. Tested before the
    // whitespace-skip for exactly that reason.
    if at > 0 && matches!(bytes[at - 1], b'$' | b'@' | b'%' | b'&') {
        return false;
    }
    let mut p = at;
    while p > 0 && bytes[p - 1].is_ascii_whitespace() {
        p -= 1;
    }
    let Some(prev) = p.checked_sub(1).map(|q| bytes[q]) else {
        return true; // the body starts with the call
    };
    match prev {
        // A term ends here, so the call has a real invocant.
        b')' | b']' | b'}' | b'\'' | b'"' | b'.' => false,
        b'>' => {
            // `a => .key` has no invocant; `%h<a>.key` does.
            matches!(p.checked_sub(2).map(|q| bytes[q]), Some(b'='))
        }
        // `$!.message` is a term even though it ends in punctuation; a bare `!`
        // is prefix negation (`{ a => !.defined }` is a block).
        b'!' => !matches!(p.checked_sub(2).map(|q| bytes[q]), Some(b'$')),
        // `*.abs` / `**.abs` curries the Whatever, which *is* the invocant, so
        // `{ :s(*.abs) }` stays a hash. Infix multiplication is spelled the same
        // way from here (`{ a => 2 * .elems }` is a block), so decide on what
        // precedes the star: a term there makes it an infix and the call a
        // topic call.
        b'*' => {
            let mut q = p - 1;
            while q > 0 && bytes[q - 1] == b'*' {
                q -= 1;
            }
            while q > 0 && bytes[q - 1].is_ascii_whitespace() {
                q -= 1;
            }
            match q.checked_sub(1).map(|s| bytes[s]) {
                None => false,
                Some(b')' | b']' | b'}' | b'\'' | b'"') => true,
                Some(b'>') => !matches!(q.checked_sub(2).map(|s| bytes[s]), Some(b'=')),
                Some(c) => c.is_ascii_alphanumeric() || c == b'_',
            }
        }
        // A `/` right before the dot closes a term far more often than it
        // divides by one: it ends `$/`, a quoting construct (`q:to/EOF/.trim`,
        // `q/x/.uc`) or a regex literal (`/rx/.gist`). Infix division, on the
        // other hand, is spelled with a space on its left (`1 / .elems`), and
        // that spelling stays a topic reference.
        b'/' => matches!(
            p.checked_sub(2).map(|q| bytes[q]),
            None | Some(b' ') | Some(b'\t') | Some(b'\n') | Some(b'\r')
        ),
        // `$¢.foo` — the last byte of the UTF-8 `¢` is 0xA2.
        0xA2 if bytes[..p].ends_with("$\u{a2}".as_bytes()) => false,
        c => !(c.is_ascii_alphanumeric() || c == b'_'),
    }
}

/// Whether a `$^a`/`@^a`/`%^a` placeholder at the *immediate* level of this
/// body forces it to be a block rather than a hash composer.
///
/// A placeholder belongs to the innermost block that encloses it, so one inside
/// a nested block is that block's parameter and says nothing about this one:
/// rakudo makes `{ status => sub { 0 != $^a } }` a `Hash` and `{ a => $^x }` a
/// `Block`. The scan is therefore gated on `depth == 1`, exactly like the
/// sibling `body_references_topic` above.
pub(super) fn body_has_placeholder_vars(input: &str) -> bool {
    let mut depth = 1u32;
    let mut chars = input.chars().peekable();
    while let Some(c) = chars.next() {
        match c {
            '{' => depth += 1,
            '}' => {
                depth -= 1;
                if depth == 0 {
                    break;
                }
            }
            '$' | '@' | '%' if depth == 1 => {
                if let Some(&'^') = chars.peek() {
                    chars.next(); // consume '^'
                    if let Some(&next) = chars.peek()
                        && (next.is_alphabetic() || next == '_')
                    {
                        return true;
                    }
                }
            }
            // Skip string contents to avoid false positives
            '\'' => {
                for sc in chars.by_ref() {
                    if sc == '\'' {
                        break;
                    }
                }
            }
            _ => {}
        }
    }
    false
}
