/// Perl 5 compatibility detection for variable names.
///
/// These functions detect Perl 5 special variables that are not valid in Raku
/// and emit structured `X::Syntax::Perl5Var` error messages.
use crate::parser::helpers::{is_raku_identifier_continue, is_raku_identifier_start};

/// Detect Perl 5 special variables that are unsupported in Raku.
/// Returns `Some(error_message)` if the input starts with a Perl 5 variable pattern,
/// or `None` if it is a valid Raku variable.
pub(crate) fn detect_perl5_scalar_var(input: &str) -> Option<String> {
    // $^A through $^Z (uppercase) are Perl 5 variables.
    // Raku placeholder parameters use lowercase: $^a through $^z.
    if input.starts_with('^') && input.len() >= 2 {
        let next = input.as_bytes()[1];
        if next.is_ascii_uppercase() {
            let var_char = next as char;
            return Some(format!(
                "X::Syntax::Perl5Var: Unsupported use of $^{} variable",
                var_char
            ));
        }
    }
    // $" — Perl 5 list separator; in Raku use .join()
    if input.starts_with('"') {
        // Check that this is bare $" not inside a string interpolation context.
        // $" followed by something that looks like assignment or end of statement.
        // We need to be careful: in `"$foo"`, the `$` starts interpolation, not $".
        // This check is called from scalar_var which is called in expression context,
        // so $" at the start means the Perl 5 variable.
        return Some(
            "X::Syntax::Perl5Var: Unsupported use of $\" variable; in Raku please use .join() method"
                .to_string(),
        );
    }
    // $$ — Perl 5 PID; in Raku use $*PID
    // $$ is Perl 5's PID variable only when followed by whitespace or assignment.
    // In all other contexts, $$ is a valid Raku construct (e.g., $$x is scalar deref,
    // $$$$ in a signature means four anonymous scalar params).
    if let Some(after) = input.strip_prefix('$') {
        let is_perl5_pid = after.is_empty()
            || after.starts_with(' ')
            || after.starts_with('\t')
            || after.starts_with('\n')
            || after.starts_with('\r')
            || after.starts_with('=')
            || after.starts_with(';');
        if is_perl5_pid {
            return Some(
                "X::Syntax::Perl5Var: Unsupported use of $$ variable; in Raku please use $*PID"
                    .to_string(),
            );
        }
    }
    // $' — Perl 5 postmatch; in Raku use $/.postmatch
    // Only flag when followed by a space/operator, not when starting a string literal.
    if let Some(after) = input.strip_prefix('\'') {
        let looks_like_var = after.is_empty()
            || after.starts_with(' ')
            || after.starts_with('=')
            || after.starts_with(';')
            || after.starts_with('\n')
            || after.starts_with(')')
            || after.starts_with(',');
        if looks_like_var {
            return Some(
                "X::Syntax::Perl5Var: Unsupported use of $' variable; in Raku please use $/.postmatch"
                    .to_string(),
            );
        }
    }
    // $\ — Perl 5 output record separator; in Raku use .nl-out attribute
    // Only flag when followed by whitespace+assignment, to avoid false positives
    // in other contexts (e.g., `say $\` which should give "Confused").
    if let Some(after) = input.strip_prefix('\\') {
        let after_trimmed = after.trim_start();
        let is_perl5_assignment = !after_trimmed.is_empty()
            && after_trimmed.starts_with('=')
            && !after_trimmed.starts_with("==")
            && !after_trimmed.starts_with("=>");
        if is_perl5_assignment {
            return Some(
                "X::Syntax::Perl5Var: Unsupported use of $\\ variable; in Raku please use the filehandle's .nl-out attribute"
                    .to_string(),
            );
        }
    }
    // $& — Perl 5 match variable; in Raku use $/ or $<>.
    // `$&foo` is a code-object deref (handled later), so only flag when the
    // `&` is not the start of a routine name.
    if let Some(after) = input.strip_prefix('&')
        && !after.chars().next().is_some_and(is_raku_identifier_start)
    {
        return Some(
            "X::Syntax::Perl5Var: Unsupported use of $& variable; in Raku please use $<>"
                .to_string(),
        );
    }
    // $` — Perl 5 prematch; in Raku use $/.prematch. A backtick never starts a
    // valid Raku variable.
    if input.starts_with('`') {
        return Some(
            "X::Syntax::Perl5Var: Unsupported use of $` variable; in Raku please use $/.prematch"
                .to_string(),
        );
    }
    // $| — Perl 5 autoflush; in Raku use the filehandle's .out-buffer attribute.
    if input.starts_with('|') {
        return Some(
            "X::Syntax::Perl5Var: Unsupported use of $| variable; in Raku please use the filehandle's .out-buffer attribute"
                .to_string(),
        );
    }
    // $? — Perl 5 child error. `$?FILE`/`$?LINE`/`$?PACKAGE` etc. are valid Raku
    // compile-time variables (a `?`-twigil before an identifier), so only flag a
    // bare `$?`.
    if let Some(after) = input.strip_prefix('?')
        && !after.chars().next().is_some_and(is_raku_identifier_start)
    {
        return Some(
            "X::Syntax::Perl5Var: Unsupported use of $? variable; in Raku please use $! for handling child errors also"
                .to_string(),
        );
    }
    // $@ — Perl 5 error variable; in Raku use $!. Only flag assignment position
    // (`$@ = ...`): bare `$@` is allowed, and `$@!attr` is an item-context deref
    // of a private array attribute.
    if let Some(after) = input.strip_prefix('@') {
        let trimmed = after.trim_start();
        if trimmed.starts_with('=')
            && !trimmed.starts_with("==")
            && !trimmed.starts_with("=>")
            && !trimmed.starts_with("=:=")
        {
            return Some(
                "X::Syntax::Perl5Var: Unsupported use of $@ variable; in Raku please use $!"
                    .to_string(),
            );
        }
    }
    // $; $, $. — Perl 5 separator/format variables. These characters are
    // normally operators/separators (and `$.foo` is Raku attribute access), so
    // only flag them in assignment position (`$; = ...`), which is how Perl 5
    // code uses them and what distinguishes them from valid Raku constructs.
    for (ch, suggestion) in [
        (';', "real multidimensional hashes"),
        (',', ".join() method"),
        ('.', "the .kv method on e.g. .lines"),
    ] {
        if let Some(after) = input.strip_prefix(ch) {
            let trimmed = after.trim_start();
            let is_assignment = trimmed.starts_with('=')
                && !trimmed.starts_with("==")
                && !trimmed.starts_with("=>")
                && !trimmed.starts_with("=:=");
            if is_assignment {
                return Some(format!(
                    "X::Syntax::Perl5Var: Unsupported use of ${} variable; in Raku please use {}",
                    ch, suggestion
                ));
            }
        }
    }
    // $# followed by identifier — Perl 5 last index; in Raku use @array.end
    if let Some(after) = input.strip_prefix('#')
        && !after.is_empty()
        && after.chars().next().is_some_and(is_raku_identifier_start)
    {
        let end = after
            .find(|c: char| !is_raku_identifier_continue(c))
            .unwrap_or(after.len());
        let name = &after[..end];
        return Some(format!(
            "X::Syntax::Perl5Var: Unsupported use of $#{} variable; in Raku please use @{}.end",
            name, name
        ));
    }
    None
}

/// Detect Perl 5 special array/hash variables (`@-`, `@+`, `%-`, `%+`, `%!`)
/// unsupported in Raku. `input` is the text after the sigil. Because `-`/`+`
/// are operators and `%!foo`/`@!foo` are private attributes, these are only
/// flagged in assignment position (`@- = ...`), matching how Perl 5 uses them.
pub(crate) fn detect_perl5_sigil_var(sigil: char, input: &str) -> Option<String> {
    let assignment_after = |after: &str| -> bool {
        let trimmed = after.trim_start();
        trimmed.starts_with('=')
            && !trimmed.starts_with("==")
            && !trimmed.starts_with("=>")
            && !trimmed.starts_with("=:=")
    };
    // @-/@+/%-/%+ : match-position arrays/hashes; suggest .from / .to.
    for (ch, suggestion) in [('-', ".from method"), ('+', ".to method")] {
        if let Some(after) = input.strip_prefix(ch)
            && assignment_after(after)
        {
            return Some(format!(
                "X::Syntax::Perl5Var: Unsupported use of {}{} variable; in Raku please use {}",
                sigil, ch, suggestion
            ));
        }
    }
    // %! : Perl 5 error hash. `%!foo` is a private attribute, so only flag the
    // standalone variable in assignment position.
    if sigil == '%'
        && let Some(after) = input.strip_prefix('!')
        && assignment_after(after)
    {
        return Some("X::Syntax::Perl5Var: Unsupported use of %! variable".to_string());
    }
    None
}
