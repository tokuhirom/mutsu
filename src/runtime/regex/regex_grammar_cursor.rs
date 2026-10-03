//! The grammar cursor a rule invocation owns (#9803).
//!
//! In Rakudo a regex runs on a *cursor*, and a grammar's cursor IS an instance
//! of the grammar: `$!attr` inside a method a rule calls (`<.acc>`) is an
//! attribute of the cursor of the rule invocation making the call, and when the
//! rule returns that cursor becomes the rule's Match. So
//!
//! ```raku
//! grammar G { has $.inv; token TOP { <t> }; token t { a <.acc> }
//!             method acc { $!inv = True; self } }
//! say G.parse("a")<t>.inv;   # True
//! say G.parse("a").inv;      # (Any): TOP's own cursor was never written
//! ```
//!
//! A cursor is minted without BUILD (`nqp::create`), so a declared attribute no
//! method wrote is its uninitialised value and a `= default` is not applied.
//!
//! The compiled engine models the invocation as a `Frame` (ADR-0135 D3) and owns
//! the instance there: [`Interpreter::rx_cursor_of`] creates it the first time
//! a call in that frame runs a grammar method, publishes it in
//! `Interpreter::rx_cursor` for that one call, and the frame's return files the
//! instance on the callee's capture node, whose Match materializes its
//! attributes (`match_lazy`). An invocation that never calls a method never
//! creates one.

use std::cell::RefCell;

use super::super::*;
use super::regex_helpers::NamedRegexLookupSpec;

impl Interpreter {
    /// Does `<spec>` called from `pkg` name a plain grammar METHOD (not a
    /// token/regex/rule)? Only plain, argument-less identifier subrules count,
    /// dispatched against a real grammar package: `<::>` indirection, char-class
    /// specs and builtin assertions are handled elsewhere. The name must be a
    /// user method of this grammar's MRO or a composed role (not an
    /// inherited Cursor/Grammar builtin, which the normal subrule/builtin paths already
    /// cover).
    // Cost: O(1) expected: one method-table probe, which rejects every subrule that
    // is not a method before the name is looked at; O(len) more for a method,
    // len = the subrule name.
    pub(super) fn subrule_names_user_method(
        &mut self,
        spec: &NamedRegexLookupSpec,
        pkg: Symbol,
    ) -> bool {
        !spec.token_lookup
            && !pkg.is_empty()
            && self.grammar_has_user_method_sym(pkg.as_str(), spec.lookup_sym)
            && !spec.lookup_name.is_empty()
            && !spec.lookup_name.contains("::")
            && spec
                .lookup_name
                .chars()
                .all(|c| c.is_alphanumeric() || c == '_' || c == '-')
    }

    /// A fresh cursor of grammar `pkg` at `pos` of `chars`: an INSTANCE of the
    /// grammar carrying the cursor state (`orig`/`from`/`pos`/`to`), not the bare
    /// type object. Raku hands a method the in-progress cursor, which is what
    /// makes the documented `method mark(--> ::?CLASS:D) { $!invalid = True; self }`
    /// idiom work: a type object made every attribute touch die with "Cannot look
    /// up attributes in a G type object", and returning `self` (a type object)
    /// read as "no match". Method resolution still finds the grammar's own method
    /// because the instance's class IS the grammar.
    ///
    /// The instance is a `CREATE`: every declared attribute is present in its
    /// uninitialised state, with no `= default` and no BUILD, as `nqp::create`
    /// mints a cursor. The slots have to exist for a method's `$!attr = ...` to
    /// persist, since the write-back only updates keys already on the instance.
    // Cost: O(a), a = the grammar's attributes (`create_instance`), plus O(1) when
    // a subject is published (`orig` shares its payload) or O(n), n = chars of
    // the subject, to build it.
    pub(super) fn new_grammar_cursor(&mut self, chars: &[char], pos: usize, pkg: Symbol) -> Value {
        let cursor = self.create_instance(pkg);
        let orig = match super::regex_helpers::current_match_target() {
            // Only when `chars` IS the published subject's own buffer: a nested
            // match on another subject must not lend its text.
            Some(target) if std::ptr::eq(target.chars().as_ptr(), chars.as_ptr()) => {
                Value::str_arc(std::sync::Arc::clone(target.text()))
            }
            _ => Value::str(chars.iter().collect::<String>()),
        };
        if let ValueView::Instance { attributes, .. } = cursor.view() {
            attributes.insert("orig", orig);
            attributes.insert("from", Value::int(pos as i64));
            attributes.insert("pos", Value::int(pos as i64));
            attributes.insert("to", Value::int(pos as i64));
        }
        cursor
    }

    /// Open the cursor scope of a rule invocation the walk is about to
    /// evaluate. Pair with [`Self::leave_rule_cursor`].
    // Cost: O(1) amortized.
    #[inline]
    pub(super) fn enter_rule_cursor(&mut self) {
        self.regex_state.walk_cursors.push(None);
    }

    /// Close the scope [`Self::enter_rule_cursor`] opened: the grammar instance a
    /// method the invocation called wrote to, if any, for the caller to file on
    /// the invocation's ends.
    // Cost: O(1).
    #[inline]
    pub(super) fn leave_rule_cursor(&mut self) -> Option<Value> {
        self.regex_state.walk_cursors.pop().flatten()
    }

    /// File `cursor` on every end of the invocation that owned it.
    // Cost: O(e), e = the ends.
    pub(super) fn file_rule_cursor(
        cursor: Option<Value>,
        ends: &mut [(usize, crate::runtime::regex_types::RegexCaptures)],
    ) {
        if let Some(cursor) = cursor {
            for (_, caps) in ends {
                caps.set_cursor(cursor.clone());
            }
        }
    }

    /// The cursor of the innermost walked rule invocation, created on the first
    /// request; `None` outside any (the method then gets a throwaway instance).
    // Cost: O(1) once created; the first request is `new_grammar_cursor`'s.
    pub(super) fn walk_rule_cursor(
        &mut self,
        chars: &[char],
        pos: usize,
        pkg: Symbol,
    ) -> Option<Value> {
        if let Some(cursor) = self.regex_state.walk_cursors.last()? {
            return Some(cursor.clone());
        }
        let cursor = self.new_grammar_cursor(chars, pos, pkg);
        *self.regex_state.walk_cursors.last_mut()? = Some(cursor.clone());
        Some(cursor)
    }

    /// The cursor `slot` (the calling rule invocation's) holds, created on the
    /// first request: an instance of grammar `pkg` at `pos`.
    // Cost: O(1) once created; the first request is `new_grammar_cursor`'s.
    pub(super) fn rx_cursor_of(
        &mut self,
        slot: &RefCell<Option<Value>>,
        chars: &[char],
        pos: usize,
        pkg: Symbol,
    ) -> Value {
        if let Some(cursor) = slot.borrow().as_ref() {
            return cursor.clone();
        }
        let cursor = self.new_grammar_cursor(chars, pos, pkg);
        *slot.borrow_mut() = Some(cursor.clone());
        cursor
    }
}

/// The invocant `.parse` hands its start rule (#10848). Rakudo calls the
/// start rule on `self.new` — a BUILT instance, with `= default` values,
/// BUILD and TWEAK applied (even `G.new(n => 7).parse` sees the default) —
/// while every subrule runs on a cursor minted without BUILD. So `self` in a
/// code block of the start rule's own body reads the grammar's defaults, and a
/// code block of any subrule reads uninitialised attributes.
///
/// The parse arms the invocant; the first engine run of the start rule's
/// pattern takes it for that run (the walk's start-rule scope, or the compiled
/// engine's root frame) and puts it back when the run ends, so a nested run —
/// a subrule, or another regex the code calls — never sees it.
#[derive(Default)]
pub(crate) struct StartRuleInvocant {
    /// Armed by `.parse`, not yet taken by a run.
    armed: Option<Value>,
    /// The walk's start-rule scope: its `walk_cursors` depth and the invocant.
    walk: Option<(usize, Value)>,
    /// Published by the compiled engine for one `Code` op: `Some(invocant)`
    /// when the op runs in the start rule's own root frame, `Some(None)` in
    /// any other frame, `None` when the walk runs the atom.
    rx_code: Option<Option<Value>>,
}

impl Interpreter {
    /// Arm `invocant` as the start rule's for the parse in progress, handing
    /// back what was armed before (a parse inside a code block nests).
    // Cost: O(1).
    pub(crate) fn arm_start_rule_invocant(&mut self, invocant: Option<Value>) -> Option<Value> {
        std::mem::replace(&mut self.regex_state.start_invocant.armed, invocant)
    }

    /// The built invocant `.parse` hands the start rule of grammar `pkg`
    /// (`pkg.new`), positioned at `pos` of `text` as rakudo's is: its cursor
    /// attributes stay at the start position for the whole parse.
    // Cost: one default construction of `pkg` (its BUILD/TWEAK runs), plus
    // O(n), n = chars of `text`, to share the subject.
    pub(crate) fn build_start_rule_invocant(
        &mut self,
        pkg: Symbol,
        text: &str,
        pos: usize,
    ) -> Result<Value, RuntimeError> {
        let invocant = self.dispatch_new(Value::package(pkg), Vec::new())?;
        if let ValueView::Instance { attributes, .. } = invocant.view() {
            attributes.insert("orig", Value::str(text.to_string()));
            attributes.insert("from", Value::int(pos as i64));
            attributes.insert("pos", Value::int(pos as i64));
            attributes.insert("to", Value::int(pos as i64));
        }
        Ok(invocant)
    }

    /// Take the armed invocant for a compiled run's root frame; hand it back
    /// with [`Self::restore_rx_start_invocant`] when the run ends.
    // Cost: O(1).
    pub(super) fn take_rx_start_invocant(&mut self) -> Option<Value> {
        self.regex_state.start_invocant.armed.take()
    }

    // Cost: O(1).
    pub(super) fn restore_rx_start_invocant(&mut self, invocant: Option<Value>) {
        if invocant.is_some() {
            self.regex_state.start_invocant.armed = invocant;
        }
    }

    /// Publish the invocant a compiled `Code` op's block runs on (see
    /// [`StartRuleInvocant::rx_code`]).
    // Cost: O(1).
    pub(super) fn publish_rx_code_invocant(&mut self, invocant: Option<Value>) {
        self.regex_state.start_invocant.rx_code = Some(invocant);
    }

    /// Open the walk's scope for the start rule's own pattern: a rule
    /// invocation like any other ([`Self::enter_rule_cursor`]) that also takes
    /// the armed invocant. Pair with [`Self::leave_start_rule_cursor`].
    // Cost: O(1) amortized.
    pub(super) fn enter_start_rule_cursor(&mut self) -> Option<(usize, Value)> {
        self.enter_rule_cursor();
        let scope = self
            .regex_state
            .start_invocant
            .armed
            .take()
            .map(|inv| (self.regex_state.walk_cursors.len(), inv));
        std::mem::replace(&mut self.regex_state.start_invocant.walk, scope)
    }

    // Cost: O(1).
    pub(super) fn leave_start_rule_cursor(
        &mut self,
        saved: Option<(usize, Value)>,
    ) -> Option<Value> {
        if let Some((_, inv)) = std::mem::replace(&mut self.regex_state.start_invocant.walk, saved)
        {
            self.regex_state.start_invocant.armed = Some(inv);
        }
        self.leave_rule_cursor()
    }

    /// The invocant a code block at the current point runs on when it is the
    /// start rule's own: the compiled engine's publication for this op, else
    /// the walk's start-rule scope when it is the innermost invocation.
    // Cost: O(1).
    pub(super) fn code_block_start_invocant(&mut self) -> Option<Value> {
        if let Some(published) = self.regex_state.start_invocant.rx_code.take() {
            return published;
        }
        match &self.regex_state.start_invocant.walk {
            Some((depth, inv)) if *depth == self.regex_state.walk_cursors.len() => {
                Some(inv.clone())
            }
            _ => None,
        }
    }
}
