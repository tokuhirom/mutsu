//! The store target check shared by a statement assignment (`SetGlobal`) and
//! an assignment expression (`AssignExpr`) to a bareword.

use super::*;

impl Interpreter {
    /// Reject a store whose target bareword names an immutable VALUE rather
    /// than a container: a type object, the bare `Nil` term, an enum value.
    // Cost: O(1) (a class-table and env probe).
    pub(super) fn check_bareword_store(
        &mut self,
        name: &str,
        name_sym: Symbol,
        name_str: &str,
        raw_mode: bool,
    ) -> Result<(), RuntimeError> {
        // Reject assignment to immutable type objects (e.g., `Foo .= new`),
        // the bare `Nil` term, and an enum value (`enum Fo <A B>; A = 3`).
        // A `constant` DECLARATION (`raw_mode`) is exempt: it binds the
        // name rather than modifying whatever the name currently means,
        // so `constant Int = 5` shadows the builtin (raku prints 5) and
        // `our Mu constant D = Metamodel::ClassHOW.new_type(:name<D>)`
        // -- the documented manual-MOP idiom, whose RHS registers a
        // class literally named `D` before the binding runs -- is legal.
        //
        // These three are one mechanism, not three special cases: each is
        // a bareword whose name denotes an immutable VALUE rather than a
        // container (X::Assignment::RO, per the readonly-assign-exception
        // taxonomy's rule 3), and the check was previously scoped to
        // user-declared classes only (`self.has_class`), silently
        // no-opping on a builtin type (`Int = 5`), `Nil = 5`, and an enum
        // value. It also used to throw an untyped `X::AdHoc` instead of
        // `X::Assignment::RO` even for the user-class case it did catch.
        // `_` is excluded even though it carries no sigil in its own
        // storage key: it is the TOPIC's env key (`$_`, never
        // slot-allocated -- it is dynamically resolved via env by
        // design, unlike an ordinary `my`/`our` variable, which either
        // gets a local slot or keeps its own twigil in its key), and a
        // bare `_` is never a valid Raku term at all (rejected at
        // `exec_get_bare_word_op`'s very first check). So whenever a
        // regular variable's CURRENT value happens to be a copy of an
        // enum member or type object (`$_ = $state` where `$state`
        // holds an enum value from an earlier iteration), only `$_`'s
        // reassignment can reach this bareword-shaped check at all --
        // and it must never be treated as reassigning that term.
        if !raw_mode
            && name != "_"
            && !name.starts_with('$')
            && !name.starts_with('@')
            && !name.starts_with('%')
            && !name.starts_with('&')
            && !crate::qualified::is_qualified(name_sym)
        {
            // A bareword that has never been (re)bound to something
            // else still resolves to the type object it names (an
            // unreferenced builtin type is never actually stored in
            // env, and a pre-seeded slot holds `Nil` -- both mirror
            // the fallback `exec_get_bare_word_op` uses for reads).
            // Anything else (e.g. a sigilless `\Int := 5` shadow)
            // means the name was rebound to a real value and must NOT
            // be treated as the type object anymore.
            //
            // The "never referenced" (env has no entry at all) AND the
            // "pre-seeded Nil slot" cases are trustworthy ONLY for a
            // TitleCase name. A lowercase bareword reaching `SetGlobal`
            // with `None`/`Nil` currently stored is overwhelmingly more
            // likely to be an ordinary variable's write than a genuine
            // reference to a lowercase native-type synonym (`int`,
            // `str`, `num`, `array`, `bool`, ...) -- `str = 5` as a bare
            // statement is not idiomatic Raku, while `$str`/`$int` are
            // extremely common variable names whose sigil-stripped
            // storage key is indistinguishable from the type name at
            // this point. TWO separate shapes hit this: a `for`-loop
            // sub-signature destructure leaf (`for @tests -> ($str,
            // $expected, |args) {...}`, roast S32-str/comb.t), which is
            // bound directly by `SetGlobal` rather than a local slot,
            // so its first-ever write sees `None`; and an uninitialized
            // outer `my $str;` (Nil) captured and assigned INSIDE a
            // closure (`my $str; lives-ok { $str = 1 }, "..."` --
            // `Test`'s `lives-ok` catches the resulting spurious
            // X::Assignment::RO and reports a false test failure,
            // caught by `t/immutable-lvalue-assignment-gaps.t`'s
            // regression control), whose free-variable write also
            // reaches `SetGlobal` and sees the captured `Some(Nil)`.
            // A TitleCase name (`Int`, `Nil`, a user class) has no such
            // realistic collision — Raku convention never uses a
            // TitleCase bareword as an ordinary variable's storage key
            // -- so the check requires it for those shapes; only a
            // REAL `Package(SomeType)` current value (set exclusively
            // by genuine class/type registration, e.g. `class Foo {}`
            // sets env["Foo"] = Package("Foo") directly) is trusted
            // unconditionally.
            //
            // `Package(Any)` is NOT that -- it is a generic "not yet
            // materialized" placeholder `SetVarDynamic` pre-seeds for
            // ANY closure-captured variable regardless of its name
            // (see the matching special case in
            // `exec_get_bare_word_op`, "A `my $Buf = Buf.new`
            // declaration pre-seeds env[\"Buf\"] with the placeholder
            // `Package(Any)`"), so it needs the SAME uppercase gate as
            // `None`/a genuine `Nil` slot: `my $str; lives-ok { $str =
            // 1 }, "..."` captures the outer, not-yet-assigned `$str`
            // this way, and without the gate its free-variable write
            // was misidentified as assigning to the lowercase native
            // type `str` (a spurious `X::Assignment::RO` that
            // `lives-ok` correctly caught and reported as a test
            // failure, even though nothing in the block actually
            // "died").
            let current_view = self.env().get_sym(name_sym).map(Value::view);
            let first_letter_uppercase = name.starts_with(|c: char| c.is_uppercase());
            let unbound_type_slot = match current_view {
                Some(ValueView::Package(p)) if p == "Any" && name != "Any" => {
                    first_letter_uppercase
                }
                Some(ValueView::Package(_)) => true,
                Some(ValueView::Nil) | None => first_letter_uppercase,
                _ => false,
            };
            if unbound_type_slot && name == "Nil" {
                return Err(RuntimeError::assignment_ro_nil());
            }
            if unbound_type_slot && (self.has_class(name) || Self::is_builtin_type(name)) {
                return Err(RuntimeError::assignment_ro_type_object(name));
            }
            // A GENUINE enum-constant reassignment (`Red = 5`) writes to
            // the bareword that IS the constant's own binding. That
            // binding lives in the ENUM-KEY namespace (#7914), not under
            // the plain env key — which is a same-named `$Red`'s storage
            // — so it is the only store this check may consult. Probing
            // `env[name]` instead used to need a `name == key` guard,
            // because an ORDINARY variable that merely held an enum value
            // transiently landed in the very same key: a for-loop's
            // second `.kv` param (`for %h.kv -> $k, $v {...}`) rebinding
            // `$v` via `SetGlobal("v", ...)` saw the previous iteration's
            // `Enum{key:"Red",..}` and misread the rebind as "assigning
            // over the `Red` constant" (a spurious X::Assignment::RO in
            // `roast/S12-enums/misc.t`). Keyed by the enum key itself,
            // the namespace cannot confuse the two — the guard is kept
            // only as a cheap assertion of that invariant. The target
            // must also be SPELLED as a bareword: the compiler keys a
            // sigilless target `\Red`, while `$red = 5` (or `(my $red)`)
            // stores under the bare `red`, which an enum key `red` of
            // the package must not make immutable (#14019).
            if name_str.starts_with('\\')
                && let Some(ValueView::Enum { enum_type, key, .. }) =
                    self.enum_bare_value(name).map(Value::view)
                && name == key.resolve()
            {
                return Err(RuntimeError::assignment_ro_typename(
                    &enum_type.resolve(),
                    &key.resolve(),
                ));
            }
        }
        Ok(())
    }
}
