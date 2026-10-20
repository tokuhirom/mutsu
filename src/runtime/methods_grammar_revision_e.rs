//! `Mu.Callable` and the `Grammar` error-reporting methods language revision
//! 6.e adds (`src/core.e/Mu.rakumod`, `src/core.e/Grammar.rakumod`; #14130).

use crate::value::{RuntimeError, Value, ValueView};

use super::Interpreter;

impl Interpreter {
    /// `Mu.Callable($method)`: the method object `.^find_method($method)`
    /// answers, which called with an invocant runs the method.
    // Cost: O(m), m = the members of the type the lookup reads.
    pub(crate) fn mu_callable(
        &mut self,
        target: &Value,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let name = args
            .first()
            .map(Value::to_string_value)
            .unwrap_or_default();
        self.classhow_find_method(target, &name)
            .ok_or_else(|| super::methods_signature_errors::method_not_found_for_value(&name, target))
    }

    /// The text the cursor `target` parses and the position it stands at.
    fn grammar_orig_and_pos(&mut self, target: &Value) -> Result<(Vec<char>, i64), RuntimeError> {
        let orig = self.call_method_with_values(target.clone(), "orig", Vec::new())?;
        let pos = match self.call_method_with_values(target.clone(), "pos", Vec::new()) {
            Ok(pos) => match pos.view() {
                ValueView::Int(n) => n,
                _ => 0,
            },
            Err(_) => 0,
        };
        Ok((orig.to_string_value().chars().collect(), pos))
    }

    /// `Grammar.locprepost`: the text before and after the cursor position on
    /// its line, at most 40 characters each, `<BOL>`/`<EOL>` when empty.
    // Cost: O(n), n = characters of the parsed text.
    pub(crate) fn grammar_locprepost(&mut self, target: &Value) -> Result<Value, RuntimeError> {
        let (orig, pos) = self.grammar_orig_and_pos(target)?;
        Ok(Self::locprepost_of(&orig, pos))
    }

    fn locprepost_of(orig: &[char], pos: i64) -> Value {
        let pos = pos.clamp(0, orig.len() as i64) as usize;
        let prestart = pos.saturating_sub(40);
        let pre_all: String = orig[prestart..pos].iter().collect();
        // `.subst(/.*\n/, "")`: the first line break and what precedes it.
        let pre = match pre_all.find('\n') {
            Some(at) => pre_all[at + 1..].to_string(),
            None => pre_all,
        };
        let pre = if pre.is_empty() { "<BOL>".to_string() } else { pre };
        let post_end = (pos + 40).min(orig.len());
        let post_all: String = orig[pos..post_end].iter().collect();
        let post = post_all.split('\n').next().unwrap_or("").to_string();
        let post = if post.is_empty() { "<EOL>".to_string() } else { post };
        Value::real_array(vec![Value::str(pre), Value::str(post)])
    }

    /// `Grammar.typed_exception(X::Comp $ex, *%opts)`: a `Failure` of `$ex`
    /// built with the line, position and surrounding text of the cursor.
    // Cost: O(n), n = characters of the parsed text.
    pub(crate) fn grammar_typed_exception(
        &mut self,
        target: &Value,
        args: &[Value],
        pos_override: Option<i64>,
    ) -> Result<Value, RuntimeError> {
        let Some(ex) = args.iter().find(|a| {
            !matches!(a.view(), ValueView::Pair(..) | ValueView::ValuePair(..))
        }) else {
            return Err(RuntimeError::new(
                "Too few positionals passed to 'typed_exception'; expected 2 arguments but got 1",
            ));
        };
        let (orig, own_pos) = self.grammar_orig_and_pos(target)?;
        let pos = pos_override.unwrap_or(own_pos);
        let mut opts: Vec<Value> = args
            .iter()
            .filter(|a| matches!(a.view(), ValueView::Pair(..)))
            .cloned()
            .collect();
        let has = |opts: &[Value], key: &str| {
            opts.iter()
                .any(|o| matches!(o.view(), ValueView::Pair(k, _) if k == key))
        };
        let clamped = pos.clamp(0, orig.len() as i64) as usize;
        let line = 1 + orig[..clamped].iter().filter(|&&c| c == '\n').count() as i64;
        let (pre, post) = match Self::locprepost_of(&orig, pos).view() {
            ValueView::Array(items, ..) => (items[0].clone(), items[1].clone()),
            _ => (Value::NIL, Value::NIL),
        };
        opts.retain(|o| !matches!(o.view(), ValueView::Pair(k, _) if k == "line" || k == "pre" || k == "post"));
        opts.push(Value::pair("line".to_string(), Value::int(line)));
        if !has(&opts, "pos") {
            opts.push(Value::pair("pos".to_string(), Value::int(pos)));
        }
        opts.push(Value::pair("pre".to_string(), pre));
        opts.push(Value::pair("post".to_string(), post));
        opts.push(Value::pair("is-compile-time".to_string(), Value::int(1)));
        let exception = self.call_method_with_values(ex.clone(), "new", opts)?;
        self.call_method_with_values(exception, "Failure", Vec::new())
    }

    /// `Grammar.SETFAIL($failed, :$filename)`: the `X::Syntax::Confused`
    /// `Failure` of a parse that stopped at `$failed`'s position.
    // Cost: O(n), n = characters of the parsed text.
    pub(crate) fn grammar_setfail(
        &mut self,
        target: &Value,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        if !is_concrete(target) {
            return Ok(Value::NIL);
        }
        let failed = args
            .iter()
            .find(|a| !matches!(a.view(), ValueView::Pair(..) | ValueView::ValuePair(..)))
            .cloned()
            .unwrap_or(Value::NIL);
        let filename = args
            .iter()
            .find_map(|a| match a.view() {
                ValueView::Pair(k, v) if k == "filename" && is_concrete(v) => {
                    Some(v.to_string_value())
                }
                _ => None,
            })
            .unwrap_or_else(|| "<anon>".to_string());
        let pos = match self.call_method_with_values(failed, "pos", Vec::new()) {
            Ok(pos) => match pos.view() {
                ValueView::Int(n) => Some(n),
                _ => None,
            },
            Err(_) => None,
        };
        let confused = Value::package(crate::symbol::Symbol::intern("X::Syntax::Confused"));
        self.grammar_typed_exception(
            target,
            &[confused, Value::pair("filename".to_string(), Value::str(filename))],
            pos,
        )
    }
}

/// Whether `v` is an instance, not a type object (or `Nil`).
// Cost: O(1).
fn is_concrete(v: &Value) -> bool {
    !matches!(v.view(), ValueView::Package(_) | ValueView::Nil)
}
