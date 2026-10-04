//! A `<name>` call that resolves to a plain grammar METHOD, not a rule.
//!
//! `rule TOP { <.panic> }` where `method panic { die ... }` is defined calls
//! the method on the in-progress cursor and reads one end from what it returns.
//! The call answers at most one end and never resumes, so the compiled engine
//! calls [`Interpreter::regex_grammar_method_end`] directly, as a leaf, with the
//! cursor of the frame making the call (ADR-0135 §8, Slice E, eighteenth part).
//! The walk reaches the same routine through
//! [`Interpreter::try_regex_subrule_as_method`].

use super::super::*;

impl Interpreter {
    /// Call the grammar method `name` of `pkg` on `invocant` (the cursor of the
    /// rule invocation making the call) at `pos` of `chars`, with `args`. The
    /// end it answers, or `None` for no match.
    ///
    /// The method's exception (`die` inside it) does not read as a silent
    /// non-match: the first one raised in the match is kept in
    /// `PENDING_REGEX_ERROR`, which the parse driver rethrows, and once one is
    /// pending no later call runs its method (a `||` branch the engine still
    /// tries, see `eval_regex_inline_code`).
    ///
    /// The method is user code, so under `MUTSU_RX_DIFF` it is recorded and
    /// replayed like a code atom (`rx_code_call`, ADR-0135 D6): the walk's run
    /// of the same match must not call it a second time. The method sees no
    /// captures (its arguments were evaluated before the call), so the record
    /// is keyed by its name and position alone.
    // Cost: the method's own run, plus O(1) to read its answer.
    pub(super) fn regex_grammar_method_end(
        &mut self,
        name: &str,
        chars: &[char],
        pos: usize,
        pkg: Symbol,
        args: &[Value],
        invocant: Value,
    ) -> Option<usize> {
        // The failure-position probe of a failed `.parse` re-walks the start
        // rule purely to measure it (ADR-0009): it must not run user code a
        // second time (#11608: an overridden `method ws` saw every position
        // twice). Without running the method its extent is unknown, so the
        // probe treats the call as no match; the probe's answer is only a
        // diagnostic position.
        if super::regex_helpers::CODE_ATOMS_INERT.with(std::cell::Cell::get) {
            return None;
        }
        // The pending-exception test is part of the recorded invocation: the
        // walk's replay runs after the compiled run raised it.
        let end: Option<(usize, RegexCaptures)> = 'run: {
            let interp = &mut *self;
            if crate::runtime::regex_parse::PENDING_REGEX_ERROR.with(|e| e.borrow().is_some()) {
                break 'run None;
            }
            interp
                .grammar_method_call(name, chars, pos, pkg, args, invocant)
                .map(|end| (end, RegexCaptures::default()))
        };
        end.map(|(end, _)| end)
    }

    /// [`Self::regex_grammar_method_end`]'s run of the method.
    // Cost: the method's own run, plus O(1) to read its answer.
    fn grammar_method_call(
        &mut self,
        name: &str,
        chars: &[char],
        pos: usize,
        pkg: Symbol,
        args: &[Value],
        invocant: Value,
    ) -> Option<usize> {
        // The invocant is an INSTANCE of the grammar carrying the cursor state
        // (`from`/`pos`/`to`/`orig`), not the bare type object: raku hands such a
        // method the in-progress cursor, which is what makes the documented
        // `method mark(--> ::?CLASS:D) { $!invalid = True; self }` idiom work.
        // It is the calling rule invocation's own cursor (Rakudo's cursor is the
        // grammar instance), so the method's attribute writes land on it and
        // travel onto the rule's Match (#9803). Its positional state moves to
        // this call.
        if let ValueView::Instance { attributes, .. } = invocant.view() {
            attributes.insert("from", Value::int(pos as i64));
            attributes.insert("pos", Value::int(pos as i64));
            attributes.insert("to", Value::int(pos as i64));
        }
        // Run the method in the grammar's package over an isolated copy of the
        // env (`run_regex_sub_call_here`: only dynamic-variable writes reach
        // the caller).
        let called = self.run_regex_sub_call_here(Some(pkg), |interp| {
            interp.call_method_with_values(invocant, name, args.to_vec())
        });
        let v = match called {
            Ok(v) => v,
            Err(e) => {
                // The FIRST exception is the one the parse dies with: a later
                // `||` branch (`'%' <.panic: "a"> || <.panic: "b">`) must not
                // replace it.
                crate::runtime::regex_parse::PENDING_REGEX_ERROR.with(|slot| {
                    slot.borrow_mut().get_or_insert(e);
                });
                return None;
            }
        };
        // A returned grammar INVOCANT (typically `self`) reports an ABSOLUTE
        // position in `pos`, so the parse resumes there — the idiomatic
        // `{ …; self }` is a zero-width success at `pos`.
        //
        // The class-name test alone is not enough: a grammar's parse cursors
        // report the grammar's own class too (raku: `Grammar` IS a `Match`
        // subclass), so a method that returns a real sub-match
        // (`return self.subparse(...)`, `$str ~~ /re/`) would be misread as a
        // zero-width `self` and swallow its extent. A Match carries its own
        // from/to and belongs to the extent branch below; only a non-Match
        // instance of the grammar is the invocant.
        if let ValueView::Instance {
            class_name,
            attributes,
            ..
        } = v.view()
            && class_name == pkg
            && !v.is_match_instance()
        {
            // A negative `pos` is a failed cursor (what `callsame` into a
            // built-in rule answers when it does not match), not a zero-width
            // success.
            let end = match attributes.as_map().get("pos").and_then(|p| p.as_int()) {
                Some(p) if p < 0 => return None,
                Some(p) => p as usize,
                None => pos,
            };
            return (end <= chars.len()).then_some(end);
        }
        // A defined Match/Cursor return advances the parse by its extent.
        // (Match goes through the seam; a non-Match cursor-like instance with a
        // `to` attribute also counts.) Anything else — undefined, not a cursor —
        // is no match.
        let to = v
            .match_to()
            .or_else(|| {
                if let ValueView::Instance { attributes, .. } = v.view() {
                    attributes.as_map().get("to").and_then(|t| t.as_int())
                } else {
                    None
                }
            })
            .filter(|&t| t >= 0)?;
        let end = pos + to as usize;
        (end <= chars.len()).then_some(end)
    }

}
