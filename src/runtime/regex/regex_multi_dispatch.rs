//! A `<name>` call that names several `multi token`/`rule`/`regex` candidates
//! and no proto.
//!
//! Rakudo has no longest-token matching to offer such a call: with no
//! `proto` (and no `:sym<>` variants for a protoregex to rank) the call is an
//! ordinary multi-method dispatch over the candidates' signatures, so one
//! candidate wins by narrowness and two equally narrow ones die with
//! `X::Multi::Ambiguous`. mutsu used to run every candidate and union the
//! ends, which parsed what rakudo rejects
//! ([#11875](https://github.com/tokuhirom/mutsu/issues/11875)).

use super::super::*;
use crate::symbol::Symbol;

/// What the signature dispatch of a protoless multi call decided.
pub(super) enum MultiVerdict {
    /// Not a protoless multi call (one candidate, a proto, `:sym<>` variants):
    /// the caller's own resolution stands.
    NotApplicable,
    /// The one candidate the call dispatches to.
    Winner(Arc<FunctionDef>),
    /// Dispatch failed (ambiguous candidates, no candidate accepting the
    /// arguments, or a `where` that threw): the error the match entry point
    /// rethrows.
    Failed(RuntimeError),
}

impl Interpreter {
    /// Dispatch the call `<name>(args)` from `pkg` over `defs`, its resolved
    /// candidates, when it is a protoless multi call.
    // Cost: O(c * p) to rank and bind c candidates of p parameters each (a
    // `where` clause runs per candidate that reaches binding); O(1) for a
    // call that is not protoless-multi.
    pub(super) fn dispatch_protoless_multi(
        &mut self,
        name: &str,
        pkg: Symbol,
        defs: &[Arc<FunctionDef>],
        args: &[Value],
    ) -> MultiVerdict {
        if defs.len() < 2
            || defs
                .iter()
                .any(|def| Self::extract_sym_adverb(&def.name.resolve()).is_some())
            || self.has_proto_token_in_pkg(name, pkg)
        {
            return MultiVerdict::NotApplicable;
        }
        let candidates = defs
            .iter()
            .map(|def| (def.name.resolve(), Arc::clone(def)))
            .collect();
        match self.choose_best_matching_candidate(name, args, candidates) {
            Ok(Some(winner)) => MultiVerdict::Winner(winner),
            Ok(None) => MultiVerdict::Failed(self.protoless_no_match_error(name, pkg, defs, args)),
            Err(err) => MultiVerdict::Failed(err),
        }
    }

    /// `X::Multi::NoMatch` for a protoless multi call no candidate accepts, in
    /// the shape of rakudo's message (`Cannot resolve caller t(G:D: Int:D);
    /// none of these signatures matches: ...`).
    // Cost: O(c * p) to render c candidate signatures of p parameters; only on
    // the error path.
    fn protoless_no_match_error(
        &mut self,
        name: &str,
        pkg: Symbol,
        defs: &[Arc<FunctionDef>],
        args: &[Value],
    ) -> RuntimeError {
        let sigs: Vec<String> = defs
            .iter()
            .map(|def| {
                crate::value::signature::make_signature_value(
                    crate::value::signature::param_defs_to_sig_info(
                        &def.param_defs,
                        def.return_type.clone(),
                    ),
                    Some(self),
                )
                .to_string_value()
            })
            .collect();
        let profile = self.format_call_arg_profile(args);
        super::super::methods_signature_errors::make_multi_no_match_error_detailed(
            name,
            pkg.as_str(),
            true,
            &profile,
            &sigs,
        )
    }

    /// An argument-less call's static candidate `list` (what
    /// `resolve_token_patterns_static_in_pkg` collected) narrowed to the one
    /// candidate a protoless multi call dispatches to: `Some` of that
    /// candidate, or `None` when the list stands as it is (not such a call, or
    /// a dispatch that finds no candidate). The call has no arguments, so
    /// which candidate wins does not depend on the match position and the
    /// answer may be memoized.
    // Cost: O(1) for a list of fewer than two candidates or a proto call;
    // otherwise the registry walk plus the dispatch of
    // [`Self::dispatch_protoless_multi`].
    pub(super) fn narrow_protoless_static(
        &mut self,
        name: &str,
        pkg: Symbol,
        list: &[(String, Symbol, Option<String>)],
    ) -> Result<Option<Vec<(String, Symbol, Option<String>)>>, RuntimeError> {
        if list.len() < 2 || list.iter().any(|(_, _, sym)| sym.is_some()) {
            return Ok(None);
        }
        let defs = self.resolve_token_defs_in_pkg(name, pkg);
        match self.dispatch_protoless_multi(name, pkg, &defs, &[]) {
            MultiVerdict::Winner(winner) => Ok(Self::token_pattern_from_def(&winner)
                .map(|pattern| vec![(pattern, self.protoless_winner_package(&winner, pkg), None)])),
            MultiVerdict::Failed(err) => Err(err),
            MultiVerdict::NotApplicable => Ok(None),
        }
    }

    /// Park the error a failed protoless multi dispatch of `<name>` raises, for
    /// the match entry point to rethrow. A cached candidate list only records
    /// THAT the dispatch failed (an analysis reads the list too, and must not
    /// raise at a branch the match never reaches), so the call that does run
    /// dispatches again to obtain the error.
    // Cost: the dispatch of [`Self::narrow_protoless_static`]; only on the
    // error path.
    pub(super) fn park_protoless_dispatch_error(&mut self, name: &str, pkg: Symbol) {
        let defs = self.resolve_token_defs_in_pkg(name, pkg);
        if let MultiVerdict::Failed(err) = self.dispatch_protoless_multi(name, pkg, &defs, &[]) {
            super::super::regex_parse::PENDING_REGEX_ERROR.with(|slot| {
                *slot.borrow_mut() = Some(err);
            });
        }
    }

    /// The package a winning candidate dispatches its own subrules in: an
    /// inherited rule dispatches virtually through the receiver grammar, as
    /// the ordinary resolution does for the ancestor entries it collects.
    // Cost: O(m), m = length of `pkg`'s MRO.
    pub(super) fn protoless_winner_package(&self, winner: &FunctionDef, pkg: Symbol) -> Symbol {
        if winner.package != pkg
            && self
                .mro_readonly(pkg.as_str())
                .iter()
                .any(|scope| scope.as_str() == winner.package.as_str())
        {
            pkg
        } else {
            winner.package
        }
    }
}
