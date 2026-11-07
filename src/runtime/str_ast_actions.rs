//! `Str.AST(:actions($obj))`: the actions object hears of the productions
//! the source contains (see [`crate::rakuast::action_events`]).

use super::*;

/// The Raku side of the bridge: for each event whose action method the object
/// has, build the grammar match it receives (`from`, `to`, `orig` and the
/// `<module-name>` capture of a `use`) and call the method.
const DRIVER: &str = r#"
sub (Mu $actions, Str $orig, @events) {
    for @events -> @e {
        my ($rule, $from, $to, $name) = @e;
        next unless $actions.^can($rule);
        my $m = __mutsu_grammar_match($orig, $from, $to, $name);
        $actions."$rule"($m);
    }
    True
}
"#;

thread_local! {
    static DRIVER_ENTRY: std::cell::RefCell<Option<Value>> = const { std::cell::RefCell::new(None) };
}

impl Interpreter {
    /// `Str.AST` with an actions object: parse, then report the productions.
    // Cost: the parse, plus one action call per production the object handles.
    pub(crate) fn str_ast_with_actions(
        &mut self,
        source: &str,
        compunit: bool,
        actions: &Value,
    ) -> Result<Value, RuntimeError> {
        crate::parser::comment_log::start();
        let result = self.str_ast_with_paths(source, compunit);
        let byte_ranges = crate::parser::comment_log::finish();
        let tree = result?;
        let comments: Vec<(i64, i64)> = byte_ranges
            .into_iter()
            .filter(|(from, to)| source.is_char_boundary(*from) && source.is_char_boundary(*to))
            .map(|(from, to)| {
                (
                    source[..from].chars().count() as i64,
                    source[..to].chars().count() as i64,
                )
            })
            .collect();
        let events = crate::rakuast::action_events::collect(
            &tree,
            source,
            &comments,
            &crate::builtins::method_table::rakuast_children,
        );
        if events.is_empty() {
            return Ok(tree);
        }
        let list = Value::array(
            events
                .into_iter()
                .map(|e| {
                    Value::array(vec![
                        Value::str(e.rule.to_string()),
                        Value::int(e.from),
                        Value::int(e.to),
                        e.name.map_or(Value::NIL, Value::str),
                    ])
                })
                .collect(),
        );
        let entry = match DRIVER_ENTRY.with(|e| e.borrow().clone()) {
            Some(entry) => entry,
            None => {
                self.ensure_raku_bases()?;
                let entry = self.eval_code_string(DRIVER)?;
                DRIVER_ENTRY.with(|e| *e.borrow_mut() = Some(entry.clone()));
                entry
            }
        };
        self.call_sub_value(
            entry,
            vec![actions.clone(), Value::str(source.to_string()), list],
        )?;
        Ok(tree)
    }

    /// `__mutsu_grammar_match($orig, $from, $to, $module-name)`: the match a
    /// `Raku::Grammar` production leaves, as an action method sees it.
    // Cost: O(|orig|).
    pub(super) fn builtin_grammar_match(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let orig = args.first().map(Value::to_string_value).unwrap_or_default();
        let at = |i: usize| args.get(i).and_then(Value::as_int).unwrap_or(0);
        let cursor = Self::make_cursor_value("Raku::Grammar", &orig, at(1), at(2), false);
        if let (Some(name), ValueView::Instance { attributes, .. }) =
            (args.get(3).filter(|v| !v.is_nil()), cursor.view())
        {
            let mut named = ValueMap::default();
            named.insert("module-name".to_string(), name.clone());
            attributes.insert("named", Value::hash_bare_values(named));
        }
        Ok(cursor)
    }
}
