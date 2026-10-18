//! What a method reads of a lazy receiver: its *pull* (ADR-13891).
//!
//! A `LazyList` is mutsu's representation of a `Seq` (or of a lazy `List` or
//! `Array`) whose iterator has not run to its end. Rakudo has no class for it:
//! its methods are those of the type it reports (`Seq`, `List`, `Array`), and
//! each of them pulls from the iterator as much as it needs. A method row
//! answers a reified receiver; a *pull* is the part of a method that runs on
//! the receiver before it is reified, registered by the owner Rakudo declares
//! the method on, next to the row it leads to:
//!
//! - it answers without reifying the rest (`AT-POS` pulls up to its index,
//!   `cache` keeps the iterator, `gist` of an infinite sequence is `(...)`),
//!   usually by handing the pulled prefix to the method's own row; or
//! - it declines (`None`), and the lazy step reifies the receiver whole when
//!   the method needs every element ([`reifies`]).
//!
//! The lazy step finds the pull by walking the owners of the type the receiver
//! reports, most-derived first, so a lazy `@`-array's `AT-POS` is `Array`'s and
//! a lazy `Seq`'s is `Seq`'s. `pulls_are_declared_by_rakudo` checks every
//! pull's owner.

use crate::runtime::Interpreter;
use crate::value::{LazyList, RuntimeError, Value};

/// A pull: the interpreter, the receiver, the receiver's list, the call's
/// arguments. `None` declines.
pub(crate) type PullFn =
    fn(&mut Interpreter, &Value, &LazyList, &[Value]) -> Option<Result<Value, RuntimeError>>;

/// One method's pull on one owner.
pub(crate) struct LazyPull {
    /// The type Rakudo declares the method on.
    pub(crate) owner: &'static str,
    pub(crate) name: &'static str,
    pub(crate) pull: PullFn,
}

macro_rules! pulls {
    ($($owner:literal . $name:literal => $pull:path),* $(,)?) => {
        &[$(LazyPull { owner: $owner, name: $name, pull: $pull }),*]
    };
}

/// Every pull, in no particular order: the walk asks each owner in turn.
pub(crate) static PULLS: &[LazyPull] = pulls![
    "Seq"."AT-POS" => at_pos,
    "Seq"."EXISTS-POS" => exists_pos,
    "List"."AT-POS" => at_pos,
    "List"."EXISTS-POS" => exists_pos,
    "Array"."AT-POS" => at_pos,
    "Seq"."cache" => cache,
    "Any"."cache" => cache,
    "Seq"."gist" => gist,
    "Seq"."Str" => str,
    "Seq"."raku" => raku,
    "List"."gist" => gist,
    "List"."Str" => str,
    "List"."raku" => raku,
    "Array"."raku" => raku,
    "Seq"."join" => join,
    "List"."join" => join,
    "Mu"."item" => item,
];

/// The methods that reify a lazy receiver whole before they run, when no
/// pull answered: the lazy step's step 2 and the VM's forcing lanes
/// (`lazy_list_needs_forcing`) read this one table.
///
/// It is a table, not "every method the reported type has a row for": on an
/// infinite sequence Rakudo answers `Bool`, `is-lazy` and `lazy` without
/// reifying, and an unlisted name goes on to the rows unchanged. Audited on
/// `(1..*).map(*+0)` against Rakudo (ADR-13891 §3, 2026-10-19):
///
/// - stay lazy, and are answered before this table: `.list`, `.List`,
///   `.Array`, `.Seq`, `.values`, `.cache` (laziness-preserving coercions),
///   `.flat`, `.unique`, `.repeated`, `.squish`, `.skip`, `.rotor`,
///   `.batch`, `.produce` (the streaming adaptors), `.kv`, `.pairs`,
///   `.antipairs`, `.keys` (the index pipes), `.first`, `.head` (pull a
///   prefix);
/// - throw `X::Cannot::Lazy` from the force (a `Failure` in Rakudo for
///   `elems`, `end`, `Numeric`, `Int`, `sum`, `reverse`, `pick`, `roll`):
///   the counting, ordering and sampling methods;
/// - `hyper` and `race` reify, so an infinite sequence throws: Rakudo
///   hyperizes it lazily, which mutsu's `HyperSeq` cannot hold yet;
/// - `item` is not here: it itemizes the receiver as it is (its pull).
// Cost: O(n), n = names in the table.
pub(crate) fn reifies(method: &str) -> bool {
    matches!(
        method,
        "list"
            | "Array"
            | "Numeric"
            | "Int"
            | "elems"
            | "end"
            | "hyper"
            | "race"
            | "first"
            | "grep"
            | "map"
            | "sort"
            | "reverse"
            | "join"
            | "head"
            | "tail"
            | "min"
            | "max"
            | "minmax"
            | "sum"
            | "flat"
            | "unique"
            | "repeated"
            | "squish"
            | "classify"
            | "categorize"
            | "produce"
            | "skip"
            | "rotor"
            | "batch"
            | "reduce"
            | "Supply"
            | "combinations"
            | "permutations"
            | "values"
            | "List"
            | "Str"
            | "Stringy"
            | "gist"
            | "raku"
            | "Seq"
            | "cache"
            | "pick"
            | "roll"
            | "keys"
            | "kv"
            | "pairs"
            | "antipairs"
            | "sink"
    )
}

/// The owners a lazy receiver's methods are looked up on: the MRO of the type
/// it reports.
// Cost: O(1).
fn owners_of(target: &Value) -> &'static [&'static str] {
    const fn mro(name: &str) -> &'static [&'static str] {
        match crate::builtin_types::catalog::builtin_type_mro_strs(name) {
            Some(mro) => mro,
            None => panic!("a lazy receiver reports a catalogued type"),
        }
    }
    const SEQ: &[&str] = mro("Seq");
    const LIST: &[&str] = mro("List");
    const ARRAY: &[&str] = mro("Array");
    match crate::value::type_name::value_type_name(target) {
        "Array" => ARRAY,
        "List" => LIST,
        _ => SEQ,
    }
}

/// The pull `method` has on the lazy receiver `target`, if any: the first one
/// its reported type's MRO reaches.
// Cost: O(o * p), o = owners in the MRO (at most 5), p = registered pulls.
pub(crate) fn pull_of(target: &Value, method: &str) -> Option<PullFn> {
    owners_of(target).iter().find_map(|&owner| {
        PULLS
            .iter()
            .find(|entry| entry.owner == owner && entry.name == method)
            .map(|entry| entry.pull)
    })
}

/// `AT-POS($i)` / `EXISTS-POS($i)`: the `postcircumfix:<[ ]>` protocol that
/// `$l[$i]` compiles to (`Language/subscripts.rakudoc`), so it pulls only as
/// many elements as the index needs, as the subscript does (#8521), and hands
/// them to the method's own row.
// Cost: O(i) pulled elements, i = the index's bound, plus the row's.
fn index(
    interp: &mut Interpreter,
    method: &str,
    list: &LazyList,
    args: &[Value],
) -> Option<Result<Value, RuntimeError>> {
    let [index] = args else {
        return None;
    };
    Some(
        interp
            .force_lazy_list_for_index(list, index)
            .and_then(|prefix| {
                interp.call_method_with_values(Value::array(prefix), method, args.to_vec())
            }),
    )
}

// Cost: as [`index`].
fn at_pos(
    interp: &mut Interpreter,
    _target: &Value,
    list: &LazyList,
    args: &[Value],
) -> Option<Result<Value, RuntimeError>> {
    index(interp, "AT-POS", list, args)
}

// Cost: as [`index`].
fn exists_pos(
    interp: &mut Interpreter,
    _target: &Value,
    list: &LazyList,
    args: &[Value],
) -> Option<Result<Value, RuntimeError>> {
    index(interp, "EXISTS-POS", list, args)
}

/// `cache`: Rakudo's `.cache` caches elements as they are pulled, it does not
/// reify the list. A `LazyList` already keeps what it pulled, so the answer is
/// a `List` view over the same iterator; a later `$l[2,3]` sees what the
/// iterator yields then (a cat handle's `.nl-in` changed mid-iteration).
// Cost: O(1).
fn cache(
    _interp: &mut Interpreter,
    _target: &Value,
    list: &LazyList,
    args: &[Value],
) -> Option<Result<Value, RuntimeError>> {
    if !args.is_empty() {
        return None;
    }
    list.cache_lazy_view().map(Ok)
}

/// `gist`/`Str`/`raku` of a list that may be infinite: rendering it would
/// reify it, so Rakudo prints a placeholder (`(...)` and `[...]` for
/// `gist`/`raku`, `...` for `Str`), and `Seq.raku` of a bare lazy `Seq`
/// renders a bounded prefix (`(1, 2, 3...).lazy.Seq`). A list that will end
/// (an eager `gather`, a cat handle's lines) declines, and is reified.
// Cost: O(1) for a placeholder; `lazy_seq_raku`'s bounded prefix otherwise.
fn render(
    interp: &mut Interpreter,
    method: &str,
    list: &LazyList,
    args: &[Value],
) -> Option<Result<Value, RuntimeError>> {
    if !args.is_empty() || !list.renders_lazy_placeholder() {
        return None;
    }
    if method == "raku" && Interpreter::lazy_seq_raku_applies(list) {
        return Some(interp.lazy_seq_raku(list).map(Value::str));
    }
    Some(Ok(Value::str(crate::value::lazy_list_placeholder(
        method,
        list.in_array_context(),
    ))))
}

// Cost: as [`render`].
fn gist(
    interp: &mut Interpreter,
    _target: &Value,
    list: &LazyList,
    args: &[Value],
) -> Option<Result<Value, RuntimeError>> {
    render(interp, "gist", list, args)
}

// Cost: as [`render`].
fn str(
    interp: &mut Interpreter,
    _target: &Value,
    list: &LazyList,
    args: &[Value],
) -> Option<Result<Value, RuntimeError>> {
    render(interp, "Str", list, args)
}

// Cost: as [`render`].
fn raku(
    interp: &mut Interpreter,
    _target: &Value,
    list: &LazyList,
    args: &[Value],
) -> Option<Result<Value, RuntimeError>> {
    render(interp, "raku", list, args)
}

/// `join`: a list that may be infinite joins as `...` without being pulled
/// (Rakudo's `Seq.join` of a lazy iterator); one that will end declines, and is
/// reified.
// Cost: O(1).
fn join(
    _interp: &mut Interpreter,
    target: &Value,
    _list: &LazyList,
    args: &[Value],
) -> Option<Result<Value, RuntimeError>> {
    (args.len() <= 1 && crate::builtins::is_join_lazy(target)).then(|| Ok(Value::str_from("...")))
}

/// `item`: Rakudo's `Mu.item` puts the receiver in a `$` container as it is;
/// it reads no element, so an infinite sequence itemizes too.
// Cost: O(1).
fn item(
    _interp: &mut Interpreter,
    target: &Value,
    _list: &LazyList,
    args: &[Value],
) -> Option<Result<Value, RuntimeError>> {
    args.is_empty().then(|| Ok(target.clone().item()))
}

#[cfg(test)]
mod tests {
    use super::PULLS;

    /// A pull belongs to a method Rakudo declares on the pull's owner, as a
    /// row does (`rows_are_declared_by_rakudo`).
    #[test]
    fn pulls_are_declared_by_rakudo() {
        let undeclared: Vec<String> = PULLS
            .iter()
            .filter(|p| {
                !crate::builtins::native_method_row::native_method_declared(p.owner, p.name)
            })
            .map(|p| format!("{}.{}", p.owner, p.name))
            .collect();
        assert!(
            undeclared.is_empty(),
            "pulls Rakudo does not declare: {undeclared:?}"
        );
    }

    /// One pull per `(owner, name)`: the walk takes the first.
    #[test]
    fn pulls_are_unique() {
        for (i, a) in PULLS.iter().enumerate() {
            assert!(
                !PULLS[i + 1..]
                    .iter()
                    .any(|b| a.owner == b.owner && a.name == b.name),
                "duplicate pull {}.{}",
                a.owner,
                a.name
            );
        }
    }
}
