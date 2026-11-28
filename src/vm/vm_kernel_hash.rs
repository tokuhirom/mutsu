//! `%h{$int}` reads and stores, and `@grid[$i][$j]` reads, for loop kernels
//! (ADR-19288 D5, amendment A1; campaign B, #19294).
//!
//! The same contract as `vm_kernel_elem` (array elements), which these extend
//! to a hash keyed by an `Int` and to a two-level array read:
//!
//! - **status 0 = OK, 1 = DECLINE.** A decline reruns the iteration on the
//!   generic path, which owns every shape these helpers do not serve.
//! - **A store checks everything before it writes**: a decline mutates
//!   nothing. A store of a new key allocates that key's Rust `String`, but no
//!   GC object, and its rerun overwrites the same key.
//! - **No user code, no polling, no GC allocation.**
//!
//! The checks mirror the interpreter's own lanes, which are the generic path's
//! answer for these shapes: the `%h{$int}` read fast path
//! (`Interpreter::index_fast_path_element`, #19308), the lean `%h{$k} = $v`
//! store (`vm_var_assign_element_lean`, #19330) and the positional read
//! (`ArrayData::plain_positional_read`). A plain hash keys an `Int` by its
//! decimal `Str`, so the key is formatted on the stack (`DecimalKey`), as the
//! read fast path does, never into a fresh `String` for a read.

use super::vm_decimal_key::DecimalKey;
use super::vm_kernel_elem::{
    KERNEL_ELEM_DECLINE, KERNEL_ELEM_OK, array_node_served, with_plain_array_node,
};
use super::vm_var_assign_element_lean::LeanStoreNode;
use super::*;
use crate::opcode::kernel_slot::SlotKind;
use crate::value::HashData;

/// Whether local slot `slot` of `code` can be a kernel hash slot: a plain user
/// `%` lexical the chunk declares, not `our`/`state`, no attribute key.
// Cost: O(1) (memoized per chunk by `local_read_class`).
pub(super) fn kernel_hash_slot_ok(code: &CompiledCode, slot: u32) -> bool {
    code.lean_hash_store_slot(slot as usize)
}

/// The per-entry check for the `%` local in `slot`: the twin of
/// `kernel_elem_entry_ok`, with the associative user `postcircumfix:<{ }>`
/// probe. `name_idx` is the store's `IndexAssignLocal` name operand; a read
/// does not consult it.
// Cost: O(1) expected: a few latch loads, plus a few symbol-keyed probes for a
// store.
pub(super) fn kernel_hash_entry_ok(
    interp: &Interpreter,
    code: &CompiledCode,
    slot: u32,
    name_idx: u32,
    store: bool,
) -> bool {
    let idx = slot as usize;
    if !kernel_hash_slot_ok(code, slot) || idx >= interp.locals.len() {
        return false;
    }
    if !super::vm_kernel_elem::local_read_unspoiled_for(code, idx) {
        return false;
    }
    if !super::vm_kernel_elem::process_latches_clear(interp, false) {
        return false;
    }
    let Some((LeanStoreNode::Hash(hash), cell)) = interp.lean_store_slot_node(idx) else {
        return false;
    };
    if !store {
        return true;
    }
    let node = crate::gc::Gc::as_ptr(&hash) as usize;
    interp.lean_store_variable_ok(code, idx, name_idx)
        && interp.lean_store_alias_agrees(code.const_sym(name_idx), node, cell.as_ref())
}

/// The identity of the hash node the slot holds, for the entry's alias rule
/// (a stored hash may not be reachable under another element name).
// Cost: O(1).
pub(super) fn kernel_hash_node_id(interp: &Interpreter, slot: u32) -> Option<usize> {
    plain_hash_node(interp, slot, false).map(|hash| crate::gc::Gc::as_ptr(&hash) as usize)
}

/// The kind of some value of the slot's hash (the first in its map's order),
/// which a kernel compiles its reads of that hash for.
// Cost: O(1).
pub(super) fn kernel_hash_kind_hint(interp: &Interpreter, slot: u32) -> Option<SlotKind> {
    let hash = plain_hash_node(interp, slot, false)?;
    let first = hash.map.values().next()?;
    match first.view() {
        ValueView::Int(_) => Some(SlotKind::Int),
        ValueView::Num(_) => Some(SlotKind::Num),
        _ => None,
    }
}

/// The hash node the slot holds, in its own word or inside its binding cell,
/// when a read keyed by an `Int` string is plain there: no object-hash key
/// type. A store (`store`) also needs what the lean store needs: no type meta,
/// `is default`, bare values, storage cells or v6.e pseudo-stash.
// Cost: O(1) (one refcount bump; one uncontended cell lock for a cell).
#[inline(always)]
fn plain_hash_node(
    interp: &Interpreter,
    slot: u32,
    store: bool,
) -> Option<crate::gc::Gc<HashData>> {
    let word = interp.locals.get(slot as usize)?;
    let hash = match word.view() {
        ValueView::Hash(hash) => hash.clone(),
        ValueView::ContainerRef(cell) => {
            if word.container_ref_is_itemized() {
                return None;
            }
            match cell.lock().ok()?.view() {
                ValueView::Hash(hash) => hash.clone(),
                _ => return None,
            }
        }
        _ => return None,
    };
    if hash.key_type.is_some() || hash.has_typed_keys() {
        return None;
    }
    if store
        && (hash.has_type_meta()
            || hash.default.is_some()
            || hash.bare_values
            || hash.storage_cells
            || hash.pseudo_stash_6e)
    {
        return None;
    }
    Some(hash)
}

/// The present value of `%h{$key}`, when it is an ordinary value (not a bound
/// cell, a deferred entry or `Nil`): what the read fast path answers.
// Cost: O(1) expected (one probe; the key formatted in O(d), d <= 20 digits).
#[inline(always)]
fn read_value(interp: &Interpreter, slot: u32, key: i64) -> Option<Value> {
    let hash = plain_hash_node(interp, slot, false)?;
    let key = DecimalKey::of(key);
    let entry = hash.get(key.as_str())?;
    match entry.view() {
        ValueView::ContainerRef(_) | ValueView::HashEntryRef { .. } | ValueView::Nil => None,
        _ => Some(entry.clone()),
    }
}

/// `%h{$int}` for a value that is a plain `Int`.
///
/// # Safety
/// `interp` and `code` are the live interpreter and chunk the kernel entry
/// received; `out` points to a writable `i64`. No other borrow of the
/// interpreter is live during the call.
// Cost: O(1) expected.
pub(super) unsafe extern "C" fn kernel_hash_read_int(
    interp: *mut Interpreter,
    _code: *const CompiledCode,
    slot: u32,
    key: i64,
    out: *mut i64,
) -> u32 {
    // SAFETY: see the function's safety contract.
    let vm = unsafe { &*interp };
    match read_value(vm, slot, key).as_ref().map(Value::view) {
        Some(ValueView::Int(n)) => {
            // SAFETY: `out` is writable (contract).
            unsafe { *out = n };
            KERNEL_ELEM_OK
        }
        _ => KERNEL_ELEM_DECLINE,
    }
}

/// `%h{$int}` for a value that is a plain `Num`.
///
/// # Safety
/// As [`kernel_hash_read_int`], with `out` a writable `f64`.
// Cost: O(1) expected.
pub(super) unsafe extern "C" fn kernel_hash_read_num(
    interp: *mut Interpreter,
    _code: *const CompiledCode,
    slot: u32,
    key: i64,
    out: *mut f64,
) -> u32 {
    // SAFETY: see the function's safety contract.
    let vm = unsafe { &*interp };
    match read_value(vm, slot, key).as_ref().map(Value::view) {
        Some(ValueView::Num(f)) => {
            // SAFETY: `out` is writable (contract).
            unsafe { *out = f };
            KERNEL_ELEM_OK
        }
        _ => KERNEL_ELEM_DECLINE,
    }
}

/// Write `value` under `%h{$key}` when the lean store would: the entry is
/// absent or holds a plain value (not a bound cell, a deferred entry or a
/// `Proxy`, which the store writes through). Nothing is written on a decline.
// Cost: O(1) expected, plus O(d) to copy a new key, d <= 20 digits.
#[inline(always)]
fn store_value(interp: &Interpreter, slot: u32, key: i64, value: Value) -> u32 {
    let Some(hash) = plain_hash_node(interp, slot, true) else {
        return KERNEL_ELEM_DECLINE;
    };
    let key = DecimalKey::of(key);
    if let Some(entry) = hash.map.get(key.as_str())
        && (entry.is_container_ref() || entry.is_hash_entry_ref_value() || entry.is_proxy_value())
    {
        return KERNEL_ELEM_DECLINE;
    }
    // SAFETY: audited aliased in-place container write (see
    // `value::aliased_mut`), the one the lean `%h{$k} = $v` store makes. No
    // borrow into this node is live across the write: `hash` is a `Gc` handle
    // and the view and cell guard it came from are dropped; no second mutator
    // thread exists (`kernel_hash_entry_ok` checked
    // `multi_mutator_threads_live`, and only code the kernel does not run
    // could start one), so this thread is the only writer of the node.
    let data: &mut HashData = unsafe { crate::value::gc_contents_mut(&hash) };
    Value::hash_insert_through_str(&mut data.map, key.as_str(), value);
    KERNEL_ELEM_OK
}

/// [`store_value`] under the key `"prefix$key"`: the text of the `Str`
/// constant `prefix` of `code` followed by `key` in decimal, built straight
/// into the `String` a new entry is filed under (no `Str` value is made).
/// The text is what the interpolation would produce: the constant is NFC and
/// a decimal digit or `-` composes with nothing, so the join is NFC too; the
/// kernel entry checked that no user `^find_method` could reach the `Int`'s
/// stringification (`Typed::str_keys`), and no user `infix:<~>` is declared
/// (`USER_INFIX_DECLS`).
// Cost: O(1) expected, plus O(p + d) to build the key, p = prefix bytes,
// d <= 20 digits.
#[inline(always)]
fn store_value_prefixed(
    interp: &Interpreter,
    code: &CompiledCode,
    slot: u32,
    prefix: u32,
    key: i64,
    value: Value,
) -> u32 {
    let Some(ValueView::Str(prefix)) = code.constants.get(prefix as usize).map(Value::view) else {
        return KERNEL_ELEM_DECLINE;
    };
    let Some(hash) = plain_hash_node(interp, slot, true) else {
        return KERNEL_ELEM_DECLINE;
    };
    let digits = crate::builtins::str_prim::IntDecimal::of(key);
    let prefix = prefix.as_str();
    let mut text = String::with_capacity(prefix.len() + digits.as_str().len());
    text.push_str(prefix);
    text.push_str(digits.as_str());
    let present = match hash.map.get(text.as_str()) {
        Some(entry)
            if entry.is_container_ref()
                || entry.is_hash_entry_ref_value()
                || entry.is_proxy_value() =>
        {
            return KERNEL_ELEM_DECLINE;
        }
        Some(_) => true,
        None => false,
    };
    // SAFETY: as in `store_value`: the audited aliased in-place write the
    // lean store makes, with no borrow into the node live across it and this
    // thread the only writer (`kernel_hash_entry_ok` checked
    // `multi_mutator_threads_live`).
    let data: &mut HashData = unsafe { crate::value::gc_contents_mut(&hash) };
    if present {
        Value::hash_insert_through_str(&mut data.map, text.as_str(), value);
    } else {
        data.map.insert(text, value.itemize_for_hash_element());
    }
    KERNEL_ELEM_OK
}

/// `%h{"prefix$int"} = $v` for an `Int` value; `prefix` is the index of the
/// `Str` constant in `code`.
///
/// # Safety
/// As [`kernel_hash_store_int`].
// Cost: O(1) expected, plus the key's text.
pub(super) unsafe extern "C" fn kernel_hash_store_prefixed_int(
    interp: *mut Interpreter,
    code: *const CompiledCode,
    slot: u32,
    prefix: u32,
    key: i64,
    value: i64,
) -> u32 {
    // SAFETY: see the function's safety contract.
    let (vm, code) = unsafe { (&*interp, &*code) };
    store_value_prefixed(vm, code, slot, prefix, key, Value::int(value))
}

/// `%h{"prefix$int"} = $v` for a `Num` value.
///
/// # Safety
/// As [`kernel_hash_store_int`].
// Cost: O(1) expected, plus the key's text.
pub(super) unsafe extern "C" fn kernel_hash_store_prefixed_num(
    interp: *mut Interpreter,
    code: *const CompiledCode,
    slot: u32,
    prefix: u32,
    key: i64,
    value: f64,
) -> u32 {
    // SAFETY: see the function's safety contract.
    let (vm, code) = unsafe { (&*interp, &*code) };
    store_value_prefixed(vm, code, slot, prefix, key, Value::num(value))
}

/// `%h{$int} = $v` for an `Int` value.
///
/// # Safety
/// `interp` and `code` are the live interpreter and chunk the kernel entry
/// received, and `kernel_hash_entry_ok` answered true for this slot (as a
/// store) at this kernel entry. No other borrow of the interpreter is live
/// during the call.
// Cost: O(1) expected.
pub(super) unsafe extern "C" fn kernel_hash_store_int(
    interp: *mut Interpreter,
    _code: *const CompiledCode,
    slot: u32,
    key: i64,
    value: i64,
) -> u32 {
    // SAFETY: see the function's safety contract.
    let vm = unsafe { &*interp };
    store_value(vm, slot, key, Value::int(value))
}

/// `%h{$int} = $v` for a `Num` value.
///
/// # Safety
/// As [`kernel_hash_store_int`].
// Cost: O(1) expected.
pub(super) unsafe extern "C" fn kernel_hash_store_num(
    interp: *mut Interpreter,
    _code: *const CompiledCode,
    slot: u32,
    key: i64,
    value: f64,
) -> u32 {
    // SAFETY: see the function's safety contract.
    let vm = unsafe { &*interp };
    store_value(vm, slot, key, Value::num(value))
}

/// The inner element `@grid[$i][$j]`, when `@grid[$i]` is a plain `Array`
/// held directly (itemized or not, as `[...]` elements are) and `[$j]` is a
/// present element of it: what `ArrayData::plain_positional_read` answers for
/// both steps, minus the shapes a kernel cannot hold (a bound cell, a hole).
// Cost: O(1).
#[inline(always)]
fn read_element2_with<R>(
    interp: &Interpreter,
    slot: u32,
    i: i64,
    j: i64,
    read: impl FnOnce(&Value) -> Option<R>,
) -> Option<R> {
    let i = usize::try_from(i).ok()?;
    let j = usize::try_from(j).ok()?;
    with_plain_array_node(interp, slot, |outer| match outer.get(i)?.view() {
        ValueView::Array(row, ArrayKind::Array | ArrayKind::ItemArray) => {
            array_node_served(&row).then_some(())?;
            read(row.get(j)?)
        }
        _ => None,
    })
}

/// The kind of `@grid[0][0]`, which a kernel compiles its two-level reads of
/// the slot for.
// Cost: O(1).
pub(super) fn kernel_elem2_kind_hint(interp: &Interpreter, slot: u32) -> Option<SlotKind> {
    read_element2_with(interp, slot, 0, 0, |v| match v.view() {
        ValueView::Int(_) => Some(SlotKind::Int),
        ValueView::Num(_) => Some(SlotKind::Num),
        _ => None,
    })
}

/// `@grid[$i][$j]` for an element that is a plain `Int`.
///
/// # Safety
/// As [`kernel_hash_read_int`]; `kernel_elem_entry_ok` answered true for the
/// slot (as a read) at this kernel entry.
// Cost: O(1).
pub(super) unsafe extern "C" fn kernel_elem2_read_int(
    interp: *mut Interpreter,
    _code: *const CompiledCode,
    slot: u32,
    i: i64,
    j: i64,
    out: *mut i64,
) -> u32 {
    // SAFETY: see the function's safety contract.
    let vm = unsafe { &*interp };
    match read_element2_with(vm, slot, i, j, Value::as_int) {
        Some(n) => {
            // SAFETY: `out` is writable (contract).
            unsafe { *out = n };
            KERNEL_ELEM_OK
        }
        _ => KERNEL_ELEM_DECLINE,
    }
}

/// `@grid[$i][$j]` for an element that is a plain `Num`.
///
/// # Safety
/// As [`kernel_elem2_read_int`], with `out` a writable `f64`.
// Cost: O(1).
pub(super) unsafe extern "C" fn kernel_elem2_read_num(
    interp: *mut Interpreter,
    _code: *const CompiledCode,
    slot: u32,
    i: i64,
    j: i64,
    out: *mut f64,
) -> u32 {
    // SAFETY: see the function's safety contract.
    let vm = unsafe { &*interp };
    match read_element2_with(vm, slot, i, j, Value::as_num) {
        Some(f) => {
            // SAFETY: `out` is writable (contract).
            unsafe { *out = f };
            KERNEL_ELEM_OK
        }
        _ => KERNEL_ELEM_DECLINE,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// The mainline of `src`, with the slot and store name operand of its
    /// first `IndexAssignLocal`.
    fn compile(src: &str) -> (CompiledCode, u32, u32) {
        let (stmts, _) = crate::parse_dispatch::parse_source(src).expect("source parses");
        let (code, _) = crate::compiler::Compiler::new().compile(&stmts);
        let (slot, name_idx) = code
            .ops
            .iter()
            .find_map(|op| match op {
                OpCode::IndexAssignLocal { slot, name_idx, .. } => Some((*slot, *name_idx)),
                _ => None,
            })
            .expect("an element store");
        (code, slot, name_idx)
    }

    fn interp_with(code: &CompiledCode, slot: u32, value: Value) -> Interpreter {
        let mut interp = Interpreter::new();
        let mut slots = vec![Value::NIL; code.locals.len()];
        slots[slot as usize] = value;
        interp.locals.install_root_frame(&slots);
        interp
    }

    fn hash_of(pairs: &[(&str, Value)]) -> Value {
        let mut map = crate::value::ValueMap::default();
        for (k, v) in pairs {
            Value::hash_insert_through_str(&mut map, k, v.clone());
        }
        Value::hash(map)
    }

    fn read_int(interp: &mut Interpreter, slot: u32, key: i64) -> Option<i64> {
        let mut out = 0i64;
        // SAFETY: a live interpreter, a writable `out`; `code` is unused.
        let status = unsafe { kernel_hash_read_int(interp, std::ptr::null(), slot, key, &mut out) };
        (status == KERNEL_ELEM_OK).then_some(out)
    }

    fn read_num(interp: &mut Interpreter, slot: u32, key: i64) -> Option<f64> {
        let mut out = 0f64;
        // SAFETY: as `read_int`.
        let status = unsafe { kernel_hash_read_num(interp, std::ptr::null(), slot, key, &mut out) };
        (status == KERNEL_ELEM_OK).then_some(out)
    }

    fn store_int(interp: &mut Interpreter, slot: u32, key: i64, value: i64) -> u32 {
        // SAFETY: a live interpreter; `code` is unused.
        unsafe { kernel_hash_store_int(interp, std::ptr::null(), slot, key, value) }
    }

    const HASH_SRC: &str = "my %h = 1 => 2; %h{3} = 4;";

    fn plain_hash() -> Value {
        hash_of(&[
            ("1", Value::int(10)),
            ("-2", Value::num(2.5)),
            ("3", Value::str_from("x")),
        ])
    }

    #[test]
    fn a_plain_declared_hash_slot_qualifies() {
        let (code, slot, name_idx) = compile(HASH_SRC);
        assert!(kernel_hash_slot_ok(&code, slot));
        let interp = interp_with(&code, slot, plain_hash());
        // The process latches are shared by every test in this binary, so the
        // positive answer is only required while they are all clear.
        let latches_clear = crate::vm::vm_jit::local_read_unspoiled()
            && !crate::runtime::shared_store::atomic_lane_entries_exist()
            && !crate::value::container_lock::multi_mutator_threads_live();
        if latches_clear {
            assert!(kernel_hash_entry_ok(&interp, &code, slot, name_idx, false));
            assert!(kernel_hash_entry_ok(&interp, &code, slot, name_idx, true));
        }
        assert!(kernel_hash_node_id(&interp, slot).is_some());
        let not_hash = interp_with(&code, slot, Value::int(1));
        assert!(!kernel_hash_entry_ok(
            &not_hash, &code, slot, name_idx, false
        ));
        assert!(kernel_hash_node_id(&not_hash, slot).is_none());
    }

    #[test]
    fn reads_take_only_a_present_value_of_the_asked_kind() {
        let (code, slot, _) = compile(HASH_SRC);
        let mut interp = interp_with(&code, slot, plain_hash());
        assert_eq!(read_int(&mut interp, slot, 1), Some(10));
        assert_eq!(read_num(&mut interp, slot, -2), Some(2.5));
        // The other kind, a Str value, a missing key: all declined.
        assert_eq!(read_num(&mut interp, slot, 1), None);
        assert_eq!(read_int(&mut interp, slot, 3), None);
        assert_eq!(read_int(&mut interp, slot, 4), None);
    }

    #[test]
    fn stores_overwrite_and_insert_by_decimal_key() {
        let (code, slot, _) = compile(HASH_SRC);
        let mut interp = interp_with(&code, slot, plain_hash());
        assert_eq!(store_int(&mut interp, slot, 1, 7), KERNEL_ELEM_OK);
        assert_eq!(read_int(&mut interp, slot, 1), Some(7));
        assert_eq!(store_int(&mut interp, slot, -40, 8), KERNEL_ELEM_OK);
        assert_eq!(read_int(&mut interp, slot, -40), Some(8));
        // SAFETY: as `store_int`.
        let status = unsafe { kernel_hash_store_num(&mut interp, std::ptr::null(), slot, 5, 0.5) };
        assert_eq!(status, KERNEL_ELEM_OK);
        assert_eq!(read_num(&mut interp, slot, 5), Some(0.5));
        let ValueView::Hash(hash) = interp.locals[slot as usize].view() else {
            panic!("slot holds a hash");
        };
        assert!(hash.map.contains_key("-40"), "keyed by the decimal Str");
        assert_eq!(hash.map.len(), 5);
    }

    #[test]
    fn a_slot_that_no_longer_holds_a_hash_declines_every_access() {
        let (code, slot, _) = compile(HASH_SRC);
        let mut interp = interp_with(&code, slot, Value::int(3));
        assert_eq!(read_int(&mut interp, slot, 1), None);
        assert_eq!(store_int(&mut interp, slot, 1, 1), KERNEL_ELEM_DECLINE);
        assert!(matches!(
            interp.locals[slot as usize].view(),
            ValueView::Int(3)
        ));
    }

    const GRID_SRC: &str = "my @g = [1, 2], [3, 4]; @g[0] = 5;";

    fn read2(interp: &mut Interpreter, slot: u32, i: i64, j: i64) -> Option<i64> {
        let mut out = 0i64;
        // SAFETY: a live interpreter, a writable `out`; `code` is unused.
        let status =
            unsafe { kernel_elem2_read_int(interp, std::ptr::null(), slot, i, j, &mut out) };
        (status == KERNEL_ELEM_OK).then_some(out)
    }

    #[test]
    fn two_level_reads_take_a_present_inner_element() {
        let (code, slot, _) = compile(GRID_SRC);
        let row = |items: Vec<Value>| match Value::real_array(items).view() {
            ValueView::Array(node, _) => Value::array_with_kind(node.clone(), ArrayKind::ItemArray),
            _ => unreachable!(),
        };
        let grid = Value::real_array(vec![
            row(vec![Value::int(1), Value::int(2)]),
            row(vec![Value::num(3.5)]),
            Value::int(9),
        ]);
        let mut interp = interp_with(&code, slot, grid);
        assert_eq!(read2(&mut interp, slot, 0, 1), Some(2));
        assert_eq!(kernel_elem2_kind_hint(&interp, slot), Some(SlotKind::Int));
        let mut out = 0f64;
        // SAFETY: as `read2`.
        let status =
            unsafe { kernel_elem2_read_num(&mut interp, std::ptr::null(), slot, 1, 0, &mut out) };
        assert_eq!((status, out), (KERNEL_ELEM_OK, 3.5));
        // An inner index out of range, a non-array row, an outer index out of
        // range, a negative index, a Num read as an Int: all declined.
        assert_eq!(read2(&mut interp, slot, 0, 2), None);
        assert_eq!(read2(&mut interp, slot, 2, 0), None);
        assert_eq!(read2(&mut interp, slot, 3, 0), None);
        assert_eq!(read2(&mut interp, slot, -1, 0), None);
        assert_eq!(read2(&mut interp, slot, 1, 0), None);
    }
}
