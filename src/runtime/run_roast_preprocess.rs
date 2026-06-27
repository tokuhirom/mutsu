use super::*;

impl Interpreter {
    /// Whether roast fudge directives (`#?rakudo skip/todo`, `#?DOES`, `#?v6`,
    /// `#?rakudo.moar emit`, ...) should be preprocessed. These are a roast
    /// harness convention, NOT Raku language syntax — applying them to ordinary
    /// user code makes a stray `#?rakudo skip 'x'` comment silently drop the
    /// next statement. So fudge is OFF by default and enabled only when running
    /// the roast suite (`MUTSU_FUDGE=1`, set by `scripts/run-roast-test.sh`).
    pub(crate) fn fudge_enabled() -> bool {
        std::env::var("MUTSU_FUDGE")
            .map(|v| v == "1" || v.eq_ignore_ascii_case("true"))
            .unwrap_or(false)
    }

    /// Apply roast fudge preprocessing when enabled, otherwise return the source
    /// unchanged. See [`fudge_enabled`](Self::fudge_enabled).
    pub(crate) fn maybe_preprocess_roast_directives(input: &str) -> std::borrow::Cow<'_, str> {
        if Self::fudge_enabled() {
            std::borrow::Cow::Owned(Self::preprocess_roast_directives(input))
        } else {
            std::borrow::Cow::Borrowed(input)
        }
    }

    pub(super) fn preprocess_roast_directives(input: &str) -> String {
        // Pre-scan: collect #?DOES N annotations mapping sub names to test counts.
        // Pattern: #?DOES N line followed (possibly after blank/comment lines) by
        // a `sub name(...)` definition.
        let mut does_counts: std::collections::HashMap<String, usize> =
            std::collections::HashMap::new();
        {
            let lines: Vec<&str> = input.lines().collect();
            let mut i = 0;
            while i < lines.len() {
                let t = lines[i].trim_start();
                if let Some(count) = t
                    .strip_prefix("#?DOES")
                    .map(str::trim_start)
                    .and_then(|s| s.split_whitespace().next())
                    .and_then(|s| s.parse::<usize>().ok())
                {
                    // Look ahead for the sub definition
                    let mut j = i + 1;
                    while j < lines.len() {
                        let tj = lines[j].trim_start();
                        if tj.is_empty() || tj.starts_with('#') {
                            j += 1;
                            continue;
                        }
                        // Match `sub name(` pattern
                        if let Some(rest) = tj.strip_prefix("sub ")
                            && let Some(name_end) =
                                rest.find(|c: char| !c.is_alphanumeric() && c != '_' && c != '-')
                        {
                            let name = &rest[..name_end];
                            if !name.is_empty() {
                                does_counts.insert(name.to_string(), count);
                            }
                        }
                        break;
                    }
                }
                i += 1;
            }
        }

        let mut output = String::new();
        let mut pending_todo: Option<(String, usize, bool)> = None; // (reason, count, explicit)
        let mut skip_lines_remaining: usize = 0;
        let mut skip_reason: String = String::new();
        // Block-level skip: skip the next { ... } block
        let mut skip_block_pending: Option<String> = None;
        let mut skip_block_depth: usize = 0;
        let mut skip_block_reason: String = String::new();
        let mut skip_block_declared_tests: Option<usize> = None;
        let mut skip_block_declared_emitted = false;
        let mut skip_stmt_paren_depth: i32 = 0;
        let test_funcs = [
            "is(",
            "is ",
            "ok ",
            "ok(",
            "tap-ok ",
            "tap-ok(",
            "nok ",
            "nok(",
            "isnt ",
            "isnt(",
            "cmp-ok ",
            "cmp-ok(",
            "isa-ok ",
            "isa-ok(",
            "does-ok ",
            "can-ok ",
            "can-ok(",
            "subtest ",
            "subtest(",
            "lives-ok",
            "dies-ok",
            "eval-lives-ok",
            "eval-dies-ok",
            "throws-like",
            "like ",
            "like(",
            "unlike ",
            "pass ",
            "pass(",
            "flunk ",
            "is-deeply",
            "is-approx",
            "is-primed-sig",
            "is-primed-call",
            "priming-fails-bind-ok",
        ];

        let all_lines: Vec<&str> = input.lines().collect();
        for (line_idx, line) in all_lines.iter().enumerate() {
            let trimmed = line.trim_start();

            // Skip continuation lines of a multi-line skipped statement.
            if skip_stmt_paren_depth > 0 {
                for ch in trimmed.chars() {
                    match ch {
                        '(' | '[' | '{' => skip_stmt_paren_depth += 1,
                        ')' | ']' | '}' => skip_stmt_paren_depth -= 1,
                        _ => {}
                    }
                }
                output.push('\n');
                continue;
            }

            // Count-based skip: skip the next N test assertion lines.
            if skip_lines_remaining > 0 {
                if test_funcs.iter().any(|f| trimmed.starts_with(f)) {
                    skip_lines_remaining -= 1;
                    output.push_str(&format!(
                        "skip {}, 1;\n",
                        Self::raku_single_quoted_literal(&skip_reason)
                    ));
                    // Track paren depth for multi-line statements
                    let mut depth = 0i32;
                    for ch in trimmed.chars() {
                        match ch {
                            '(' | '[' | '{' => depth += 1,
                            ')' | ']' | '}' => depth -= 1,
                            _ => {}
                        }
                    }
                    if depth > 0 {
                        skip_stmt_paren_depth = depth;
                    }
                    continue;
                }
                output.push_str(line);
                output.push('\n');
                continue;
            }

            // Block-level skip: waiting for opening brace
            if let Some(ref reason) = skip_block_pending {
                if trimmed.starts_with("#?DOES") {
                    let count = trimmed
                        .strip_prefix("#?DOES")
                        .map(str::trim_start)
                        .and_then(|s| s.split_whitespace().next())
                        .and_then(|s| s.parse::<usize>().ok())
                        .unwrap_or(0);
                    if count > 0 {
                        skip_block_declared_tests = Some(count);
                    }
                    output.push('\n');
                    continue;
                }
                if trimmed.starts_with('{') {
                    skip_block_reason = reason.clone();
                    skip_block_pending = None;
                    skip_block_depth = 1;
                    skip_block_declared_emitted = false;
                    if let Some(count) = skip_block_declared_tests.take() {
                        for _ in 0..count {
                            output.push_str(&format!(
                                "skip {}, 1;\n",
                                Self::raku_single_quoted_literal(&skip_block_reason)
                            ));
                        }
                        skip_block_declared_emitted = true;
                    }
                    output.push('\n');
                    continue;
                } else if trimmed.is_empty() || trimmed.starts_with('#') {
                    output.push('\n');
                    continue;
                } else {
                    // Not a block — treat as single-line skip for test assertions.
                    if test_funcs.iter().any(|f| trimmed.starts_with(f)) {
                        output.push_str(&format!(
                            "skip {}, 1;\n",
                            Self::raku_single_quoted_literal(reason)
                        ));
                        skip_block_pending = None;
                        // If the statement spans multiple lines, set up to
                        // skip continuation lines until parens balance.
                        let mut depth = 0i32;
                        for ch in trimmed.chars() {
                            match ch {
                                '(' | '[' | '{' => depth += 1,
                                ')' | ']' | '}' => depth -= 1,
                                _ => {}
                            }
                        }
                        if depth > 0 {
                            skip_stmt_paren_depth = depth;
                        }
                        continue;
                    }
                    // Not a block/test line — cancel skip
                    skip_block_pending = None;
                    continue;
                }
            }
            // Inside a skipped block: track braces and emit skip for test lines
            if skip_block_depth > 0 {
                for ch in trimmed.chars() {
                    if ch == '{' {
                        skip_block_depth += 1;
                    } else if ch == '}' {
                        skip_block_depth -= 1;
                        if skip_block_depth == 0 {
                            break;
                        }
                    }
                }
                if skip_block_depth == 0 {
                    output.push('\n');
                    continue;
                }
                if skip_block_declared_emitted {
                    output.push('\n');
                    continue;
                }
                // Emit skip for lines that look like test assertions
                if test_funcs.iter().any(|f| trimmed.starts_with(f)) {
                    output.push_str(&format!(
                        "skip {}, 1;\n",
                        Self::raku_single_quoted_literal(&skip_block_reason)
                    ));
                } else {
                    // Check if this line calls a function annotated with #?DOES N
                    let mut emitted_does = false;
                    for (func_name, count) in &does_counts {
                        if trimmed.contains(&format!("{func_name}("))
                            || trimmed.starts_with(&format!("{func_name} "))
                        {
                            for _ in 0..*count {
                                output.push_str(&format!(
                                    "skip {}, 1;\n",
                                    Self::raku_single_quoted_literal(&skip_block_reason)
                                ));
                            }
                            emitted_does = true;
                            break;
                        }
                    }
                    if !emitted_does {
                        output.push('\n');
                    }
                }
                continue;
            }

            // #?rakudo.moar emit <code> — include code only for moar backend.
            // mutsu treats itself as moar-compatible, so emit the code.
            // #?rakudo.jvm emit and #?rakudo.js emit are ignored.
            if trimmed.starts_with("#?rakudo.moar emit ")
                || trimmed.starts_with("#?rakudo.moar emit\t")
            {
                let code = trimmed
                    .strip_prefix("#?rakudo.moar emit")
                    .unwrap()
                    .trim_start();
                output.push_str(code);
                output.push('\n');
                continue;
            }
            if (trimmed.starts_with("#?rakudo.jvm emit")
                || trimmed.starts_with("#?rakudo.js emit")
                || trimmed.starts_with("#?rakudo.js.browser emit"))
                && !trimmed.contains(".moar")
            {
                output.push('\n');
                continue;
            }

            // #?rakudo todo 'reason' or #?rakudo N todo 'reason' remain TODOs for
            // failing assertions, but passing assertions should be reported as
            // normal passes rather than "ok ... # TODO".
            if trimmed.starts_with("#?rakudo")
                && !trimmed.contains(".jvm")
                && !trimmed.contains(".js")
                && trimmed.contains("todo")
            {
                let after = trimmed.trim_start_matches("#?rakudo").trim_start();
                // Check for count: #?rakudo N todo "reason"
                let (count, explicit_count, after_count) = if let Some(first_char) =
                    after.chars().next()
                    && first_char.is_ascii_digit()
                {
                    let num_str: String =
                        after.chars().take_while(|c| c.is_ascii_digit()).collect();
                    let n: usize = num_str.parse().unwrap_or(1);
                    (n, true, after[num_str.len()..].trim_start())
                } else {
                    (1, false, after)
                };
                // Extract the reason string (single or double quoted)
                let reason = if let Some(start) = after_count.find('\'') {
                    if let Some(end) = after_count[start + 1..].find('\'') {
                        &after_count[start + 1..start + 1 + end]
                    } else {
                        "todo"
                    }
                } else if let Some(start) = after_count.find('"') {
                    if let Some(end) = after_count[start + 1..].find('"') {
                        &after_count[start + 1..start + 1 + end]
                    } else {
                        "todo"
                    }
                } else {
                    "todo"
                };
                pending_todo = Some((
                    format!("__mutsu_backend_todo__:{reason}"),
                    count,
                    explicit_count,
                ));
                output.push('\n');
                continue;
            }

            // Emit pending todo before next non-comment, non-empty line.
            // Emit `todo 'reason', count;` once with the full count so that
            // the Test module marks the correct number of upcoming tests as
            // TODO, regardless of how many source lines each test spans.
            if let Some((ref reason, count, explicit)) = pending_todo
                && !trimmed.is_empty()
                && !trimmed.starts_with('#')
            {
                let final_count = if !explicit && trimmed == "{" {
                    // Block-level #?rakudo todo without explicit count:
                    // scan ahead to count test assertions inside the block.
                    let mut block_depth = 1usize;
                    let mut test_count = 0usize;
                    for future_line in &all_lines[line_idx + 1..] {
                        let ft = future_line.trim_start();
                        for ch in ft.chars() {
                            if ch == '{' {
                                block_depth += 1;
                            } else if ch == '}' {
                                block_depth = block_depth.saturating_sub(1);
                            }
                        }
                        if block_depth == 0 {
                            break;
                        }
                        if test_funcs.iter().any(|f| ft.starts_with(f)) {
                            test_count += 1;
                        }
                    }
                    test_count.max(1)
                } else {
                    count
                };
                output.push_str(&format!(
                    "todo {}, {};\n",
                    Self::raku_single_quoted_literal(reason),
                    final_count
                ));
                pending_todo = None;
            }

            // #?rakudo N skip 'reason' — count-based skip directive.
            // Skip the next N test lines. Block-level #?rakudo skip (without count)
            // is ignored since mutsu is not rakudo.
            if trimmed.starts_with("#?rakudo")
                && trimmed.contains("skip")
                && !trimmed.contains(".jvm")
                && !trimmed.contains(".moar")
                && !trimmed.contains(".js")
            {
                let after_prefix = trimmed.trim_start_matches("#?rakudo").trim_start();
                if let Some(first_char) = after_prefix.chars().next()
                    && first_char.is_ascii_digit()
                {
                    // Parse the count: #?rakudo N skip 'reason'
                    let count: usize = after_prefix
                        .split_whitespace()
                        .next()
                        .and_then(|s| s.parse().ok())
                        .unwrap_or(1);
                    skip_reason = if let Some(start) = after_prefix.find('\'') {
                        if let Some(end) = after_prefix[start + 1..].find('\'') {
                            after_prefix[start + 1..start + 1 + end].to_string()
                        } else {
                            "skip".to_string()
                        }
                    } else {
                        "skip".to_string()
                    };
                    skip_lines_remaining = count;
                    output.push('\n');
                    continue;
                }
                // Skip without count: skip next block if it starts with '{',
                // otherwise skip the next non-comment line.
                let after_skip = after_prefix
                    .strip_prefix("skip")
                    .unwrap_or(after_prefix)
                    .trim_start();
                skip_reason = if let Some(start) = after_skip.find('"') {
                    if let Some(end) = after_skip[start + 1..].find('"') {
                        after_skip[start + 1..start + 1 + end].to_string()
                    } else {
                        "skip".to_string()
                    }
                } else if let Some(start) = after_skip.find('\'') {
                    if let Some(end) = after_skip[start + 1..].find('\'') {
                        after_skip[start + 1..start + 1 + end].to_string()
                    } else {
                        "skip".to_string()
                    }
                } else {
                    "skip".to_string()
                };
                skip_block_pending = Some(skip_reason.clone());
                output.push('\n');
                continue;
            }

            // #?v6.0.0+ skip 'reason' — version-based skip directive.
            // mutsu is v6+, so these skips apply. Handle like #?rakudo skip.
            if trimmed.starts_with("#?v6") && trimmed.contains("skip") {
                // Extract the part after the version marker (e.g., "#?v6.0.0+ skip 'reason'")
                let after_version = if let Some(pos) = trimmed.find("skip") {
                    trimmed[pos..]
                        .strip_prefix("skip")
                        .unwrap_or("")
                        .trim_start()
                } else {
                    ""
                };
                skip_reason = if let Some(start) = after_version.find('\'') {
                    if let Some(end) = after_version[start + 1..].find('\'') {
                        after_version[start + 1..start + 1 + end].to_string()
                    } else {
                        "skip".to_string()
                    }
                } else if let Some(start) = after_version.find('"') {
                    if let Some(end) = after_version[start + 1..].find('"') {
                        after_version[start + 1..start + 1 + end].to_string()
                    } else {
                        "skip".to_string()
                    }
                } else {
                    "skip".to_string()
                };
                skip_block_pending = Some(skip_reason.clone());
                output.push('\n');
                continue;
            }

            output.push_str(line);
            output.push('\n');
        }

        output
    }
}
