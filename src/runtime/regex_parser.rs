//! [`RegexParser`]: the structural regex parser, as a value of its own rather
//! than methods of [`Interpreter`].
//!
//! A regex source is parsed in one of two modes ([`RegexParseMode`]). `Match`
//! parses a pattern the running program is about to match with, and reads the
//! runtime as it goes: interpolated variables, the grammar's own tokens, the
//! current package. `Validate` checks a regex literal's syntax while the
//! program is still being compiled, and must read none of that, because none
//! of it exists yet.
//!
//! The parser carries the runtime as an `Option`: `Match` parses get the
//! interpreter, and a `Validate` parse gets `None`. A runtime read in a
//! validation is therefore not a convention to keep but impossible to write.
//! The parse-time validator used to borrow a whole thread-local
//! `Interpreter::new()` as its receiver instead -- a second full start-up
//! (~0.8M instructions) for every program with a regex literal.
//!
//! [`RegexParseMode`]: super::regex_parse::RegexParseMode

use super::Interpreter;

/// See the module docs.
#[derive(Clone, Copy)]
pub(crate) struct RegexParser<'a> {
    rt: Option<&'a Interpreter>,
}

impl<'a> RegexParser<'a> {
    /// A parser that reads `rt` (a `Match` parse), or no runtime at all
    /// (`None`: a parse-time `Validate`).
    // Cost: O(1).
    #[inline]
    pub(crate) fn new(rt: Option<&'a Interpreter>) -> Self {
        Self { rt }
    }

    /// The running interpreter, or `None` while validating at parse time.
    // Cost: O(1).
    #[inline]
    pub(super) fn rt(&self) -> Option<&'a Interpreter> {
        self.rt
    }

    // ----- the runtime questions the structural parse asks ------------------
    //
    // Each answers as an empty runtime would when there is none: no grammar,
    // no package, no variables.

    /// The current package's symbol; `GLOBAL` without a runtime.
    // Cost: O(1).
    pub(super) fn package_sym(&self) -> crate::symbol::Symbol {
        self.rt.map_or_else(
            || crate::symbol::Symbol::intern("GLOBAL"),
            Interpreter::current_package_sym,
        )
    }

    /// Whether the current package is a grammar (or class) that defines its
    /// own token/rule/regex `name`.
    // Cost: O(|name|) expected, a token-table probe.
    pub(super) fn grammar_defines_token(&self, name: &str) -> bool {
        self.rt.is_some_and(|rt| {
            !rt.current_package().is_empty() && rt.resolve_token_defs(name).is_some()
        })
    }

    /// [`Self::grammar_defines_token`], also counting a `proto` token of that
    /// name in the current package.
    // Cost: O(|name|) expected, two table probes.
    pub(super) fn grammar_defines_token_or_proto(&self, name: &str) -> bool {
        self.rt.is_some_and(|rt| {
            let pkg = rt.current_package();
            !pkg.is_empty()
                && (rt.resolve_token_defs(name).is_some()
                    || rt.has_proto_token_in_pkg(name, crate::symbol::Symbol::intern(&pkg)))
        })
    }

    /// Whether a token `name` of a package other than `GLOBAL` replaces the
    /// built-in character class of that name.
    // Cost: O(|name|) expected, a token-table probe.
    pub(super) fn grammar_overrides_builtin(&self, name: &str) -> bool {
        self.rt.is_some_and(|rt| {
            !crate::qualified::is_global_package(crate::symbol::Symbol::intern(
                &rt.current_package(),
            )) && rt.resolve_token_defs(name).is_some()
        })
    }
}

/// The structural parse's entry points, as the running interpreter reaches
/// them: each parses with this interpreter as the runtime.
impl Interpreter {
    // Cost: as `RegexParser::parse_regex_uncached`.
    pub(super) fn parse_regex_uncached(
        &self,
        pattern: &str,
        mode: super::regex_parse::RegexParseMode,
    ) -> Option<crate::runtime::RegexPattern> {
        RegexParser::new(Some(self)).parse_regex_uncached(pattern, mode)
    }

    // Cost: as `RegexParser::parse_regex_uncached_interpolated`.
    pub(super) fn parse_regex_uncached_interpolated(
        &self,
        interpolated: &str,
        mode: super::regex_parse::RegexParseMode,
    ) -> Option<crate::runtime::RegexPattern> {
        RegexParser::new(Some(self)).parse_regex_uncached_interpolated(interpolated, mode)
    }
}
