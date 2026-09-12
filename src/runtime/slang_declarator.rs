//! Package declarators added by a slang (ADR-0091).
//!
//! A module that wants a new package declarator — a keyword that introduces a
//! package the way `class` and `role` do — adds a proto-regex candidate to the
//! host grammar from its `sub EXPORT`:
//!
//! ```text
//! token package_declarator:sym<test-bundle> {
//!     :my $*PKGDECL := 'role';
//!     <sym><.kok>
//!     { $*LANG.set_how('role', Test::Async::Metamodel::BundleHOW); }
//!     <package_def>
//!     <.set_braid_from(self)>
//! }
//! ```
//!
//! That is the standard NQP idiom, and everything in it that decides what the
//! new declarator *means* is declarative: the candidate's `:sym<...>` is the
//! keyword, `$*PKGDECL` is the package kind it builds, and `$*LANG.set_how`
//! names the metaclass. mutsu reads exactly those three facts out of the
//! candidate and maps them onto the declarator machinery it already has for
//! `EXPORTHOW::DECLARE` (the `monitor` keyword of the bundled `OO::Monitors`).
//!
//! The rest of the candidate is Rakudo's own compiler surface — `<package_def>`,
//! `<.set_braid_from(self)>`, `HLL::Compiler.lineof`, and the `QAST`-building
//! actions role beside it. ADR-0026 §4 declined to execute that, and ADR-0091
//! keeps the refusal: the candidate body is *read*, never run.

use super::*;
use crate::value::ValueView;

/// One package declarator a slang grammar role registered.
#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct SlangDeclarator {
    /// The keyword itself — the `:sym<...>` of the proto-regex candidate.
    pub(crate) keyword: String,
    /// The `$*PKGDECL` the candidate declares, i.e. which package kind the
    /// keyword builds. `role` makes the keyword a peer of `role`; anything
    /// else (including a kind named after the declarator itself) is built as
    /// a `class`, which is what Rakudo's `package_def` does for every
    /// class-like `$*PKGDECL`.
    pub(crate) kind: String,
    /// Name of the metaclass the declaration is built with, from
    /// `$*LANG.set_how($*PKGDECL, ...)`. Empty when the slang set none, in
    /// which case the keyword is a plain alias for its package kind.
    pub(crate) how_type: String,
}

impl SlangDeclarator {
    pub(crate) fn is_role(&self) -> bool {
        self.kind == "role"
    }
}

/// The declarator keyword of a `package_declarator:sym<name>` grammar rule,
/// or `None` for any other rule name.
pub(crate) fn declarator_keyword(rule: &str) -> Option<&str> {
    let rest = rule.strip_prefix("package_declarator:sym<")?;
    let inner = rest.strip_suffix('>')?;
    (!inner.is_empty() && !inner.contains('<') && !inner.contains('>')).then_some(inner)
}

/// Lift the *declarative* prologue out of a proto-regex candidate's source:
/// its `:my ...;` dynamic-variable declarations and its top-level `{ ... }`
/// code blocks, in source order, as a Raku statement list.
///
/// Everything else — the atoms, subrule calls and assertions that make up the
/// match itself — is dropped; this is a reading of what the candidate
/// *declares*, not a translation of what it matches.
pub(crate) fn candidate_prologue(regex_src: &str) -> String {
    let bytes: Vec<char> = regex_src.chars().collect();
    let mut out = String::new();
    let mut i = 0usize;
    while i < bytes.len() {
        match bytes[i] {
            // A quoted literal inside the regex: skip it wholesale so a `{`
            // or `:my` spelled inside a string is not mistaken for syntax.
            q @ ('\'' | '"') => {
                i += 1;
                while i < bytes.len() && bytes[i] != q {
                    i += if bytes[i] == '\\' { 2 } else { 1 };
                }
                i += 1;
            }
            // A subrule / assertion (`<package_def>`, `<.kok>`, `<?{ ... }>`):
            // skipped whole, braces included.
            '<' => {
                let mut depth = 0usize;
                while i < bytes.len() {
                    match bytes[i] {
                        '<' => depth += 1,
                        '>' => {
                            depth -= 1;
                            if depth == 0 {
                                i += 1;
                                break;
                            }
                        }
                        _ => {}
                    }
                    i += 1;
                }
            }
            '{' => {
                let start = i + 1;
                let mut depth = 0usize;
                while i < bytes.len() {
                    match bytes[i] {
                        '{' => depth += 1,
                        '}' => {
                            depth -= 1;
                            if depth == 0 {
                                break;
                            }
                        }
                        _ => {}
                    }
                    i += 1;
                }
                let body: String = bytes[start..i.min(bytes.len())].iter().collect();
                out.push_str(body.trim());
                out.push_str(";\n");
                i += 1;
            }
            ':' if bytes[i..].starts_with(&[':', 'm', 'y']) => {
                let start = i + 3;
                let mut j = start;
                let mut depth = 0usize;
                while j < bytes.len() {
                    match bytes[j] {
                        '(' | '[' | '{' => depth += 1,
                        ')' | ']' | '}' => depth = depth.saturating_sub(1),
                        ';' if depth == 0 => break,
                        _ => {}
                    }
                    j += 1;
                }
                let decl: String = bytes[start..j.min(bytes.len())].iter().collect();
                out.push_str("my ");
                out.push_str(decl.trim());
                out.push_str(";\n");
                i = j + 1;
            }
            _ => i += 1,
        }
    }
    out
}

impl Interpreter {
    /// Read one `package_declarator:sym<keyword>` candidate into the
    /// declarator it registers. `regex_src` is the candidate's own source.
    ///
    /// The metaclass comes from the candidate's own `$*LANG.set_how($kind, X)`
    /// when it has one (Rakudo's grammar swaps the HOW for the duration of the
    /// declaration and restores it in `set_package`), and otherwise from the
    /// mapping the module's `EXPORT` installed before `define_slang`.
    pub(crate) fn slang_declarator_from_candidate(
        &self,
        keyword: &str,
        regex_src: Option<&str>,
    ) -> SlangDeclarator {
        let prologue = candidate_prologue(regex_src.unwrap_or_default());
        let stmts = crate::parse_dispatch::parse_source(&prologue)
            .map(|(stmts, _)| stmts)
            .unwrap_or_default();
        let mut kind: Option<String> = None;
        let mut set_hows: Vec<(String, String)> = Vec::new();
        collect_declarator_facts(&stmts, &mut kind, &mut set_hows);
        let kind = kind.unwrap_or_else(|| keyword.to_string());
        let how_type = set_hows
            .iter()
            .rev()
            .find(|(k, _)| *k == kind)
            .map(|(_, how)| how.clone())
            .or_else(|| {
                self.slang_declarator_hows
                    .get(&kind)
                    .map(Self::slang_how_type_name)
            })
            .unwrap_or_default();
        SlangDeclarator {
            keyword: keyword.to_string(),
            kind,
            how_type,
        }
    }

    fn slang_how_type_name(how: &Value) -> String {
        match how.view() {
            ValueView::Package(sym) => sym.resolve(),
            ValueView::Instance { class_name, .. } => class_name.resolve(),
            _ => how.to_string_value(),
        }
    }
}

/// Walk the candidate prologue for the two facts a declarator is made of:
/// the `$*PKGDECL` it declares and the `$*LANG.set_how` calls it makes.
fn collect_declarator_facts(
    stmts: &[Stmt],
    kind: &mut Option<String>,
    set_hows: &mut Vec<(String, String)>,
) {
    for stmt in stmts {
        match stmt {
            Stmt::SyntheticBlock(inner) | Stmt::Block(inner) => {
                collect_declarator_facts(inner, kind, set_hows);
            }
            Stmt::VarDecl { name, expr, .. } if name == "*PKGDECL" => {
                if let Some(text) = literal_str(expr) {
                    *kind = Some(text);
                }
            }
            Stmt::Expr(Expr::MethodCall {
                target, name, args, ..
            }) if matches!(&**target, Expr::Var(v) if v == "*LANG")
                && name.resolve() == "set_how"
                && args.len() == 2 =>
            {
                if let (Some(k), Some(how)) = (literal_str(&args[0]), type_name(&args[1])) {
                    set_hows.push((k, how));
                }
            }
            _ => {}
        }
    }
}

fn literal_str(expr: &Expr) -> Option<String> {
    match expr {
        Expr::Literal(v) => match v.view() {
            ValueView::Str(s) => Some(s.to_string()),
            _ => None,
        },
        Expr::BareWord(w) => Some(w.clone()),
        _ => None,
    }
}

/// The name of a type mentioned as an argument. A fresh parse of the prologue
/// has no types declared, so a metaclass name arrives as a bareword; the other
/// spellings are what the same expression becomes once the type *is* known.
fn type_name(expr: &Expr) -> Option<String> {
    match expr {
        Expr::BareWord(w) => Some(w.clone()),
        Expr::Var(v) if !v.starts_with(['$', '@', '%', '&']) => Some(v.clone()),
        Expr::Literal(v) => match v.view() {
            ValueView::Package(sym) => Some(sym.resolve()),
            _ => None,
        },
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn keyword_is_read_off_the_proto_regex_candidate_name() {
        assert_eq!(
            declarator_keyword("package_declarator:sym<test-bundle>"),
            Some("test-bundle")
        );
        assert_eq!(declarator_keyword("package_declarator"), None);
        assert_eq!(declarator_keyword("term:sym<identifier>"), None);
        assert_eq!(declarator_keyword("package_declarator:sym<>"), None);
    }

    #[test]
    fn prologue_keeps_declarations_and_code_blocks_and_drops_the_match() {
        let src = ":ratchet :my $*OUTERPACKAGE := self.package; :my $*PKGDECL := 'role'; \
                   <sym><.kok> { $*LANG.set_how('role', Some::HOW); } <package_def> \
                   <.set_braid_from(self)> ";
        let prologue = candidate_prologue(src);
        assert!(prologue.contains("my $*OUTERPACKAGE := self.package;"));
        assert!(prologue.contains("my $*PKGDECL := 'role';"));
        assert!(prologue.contains("$*LANG.set_how('role', Some::HOW);"));
        // The match itself contributes nothing.
        assert!(!prologue.contains("package_def"));
        assert!(!prologue.contains("set_braid_from"));
    }

    #[test]
    fn a_brace_inside_an_assertion_is_not_read_as_a_code_block() {
        let prologue = candidate_prologue("<?{ $*IN-DECL }> :my $*PKGDECL := 'role';");
        assert!(!prologue.contains("IN-DECL"));
        assert!(prologue.contains("my $*PKGDECL := 'role';"));
    }
}
