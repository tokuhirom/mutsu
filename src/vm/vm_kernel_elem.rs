//! Array element reads and stores for loop kernels (ADR-19288 D5; campaign B,
//! #19294).
//!
//! A loop kernel runs whole iterations over machine words. A body that reads
//! or writes `@a[$i]` used to decline the kernel outright, so the loop around
//! the access kept paying the interpreter's per-iteration cost (2,000-3,000
//! instructions against rakupp's ~150). These helpers let a kernel do the
//! access itself, through a call, under the kernel's own contract:
//!
//! - **status 0 = OK, 1 = DECLINE.** A declined access reruns its iteration
//!   on the generic path (D5), which is the authority on every shape these
//!   helpers do not serve. There is no error status: nothing here can throw.
//! - **A store checks everything before it writes**, so a DECLINE leaves no
//!   mutation and the rerun sees exactly the state the kernel saw.
//! - **No user code, no polling, no GC allocation.** A read copies a machine
//!   word out of an element; a store writes an `Int`/`Num` value word.
//!
//! The checks are split by when their answer can change:
//!
//! - [`kernel_elem_slot_ok`]: settled by the chunk (the slot is a plain user
//!   `@` lexical the chunk declares, not `our`/`state`, no attribute key). The
//!   kernel's typing asks it once, at compile time.
//! - [`kernel_elem_entry_ok`]: settled by process latches and by-name variable
//!   records (no thread clone, no second mutator thread, no user
//!   `postcircumfix:<[ ]>`, no `$CALLER::`/atomic spoiler; for a store, not
//!   readonly, no `is default`, no constraint, no `:=` pair or alias that
//!   moves the store). Each of them changes only when code runs -- a `BEGIN`,
//!   an `EVAL`, a thread start, a declaration -- and a kernel runs none, so
//!   the kernel asks once per entry for each element slot it touches.
//! - the per-access helpers: what the container holds right now (its kind and
//!   metadata, the index, the element).
//!
//! Every condition mirrors one the interpreter's own fast lanes already rely
//! on: the aggregate `GetLocal` fast path (#19299) plus
//! `Interpreter::index_fast_path_element` for a read, and the lean
//! `IndexAssignLocal` store (`vm_var_assign_element_lean`, #19306) for a
//! store. Those lanes are the generic path's answer for these shapes, so a
//! kernel access equals the generic one whenever it does not decline.

use super::vm_var_assign_element_lean::LeanStoreNode;
use super::*;
use crate::value::ArrayData;

/// The access succeeded.
pub(super) const KERNEL_ELEM_OK: u32 = 0;
/// The access is not one this lane serves; rerun the iteration generically.
pub(super) const KERNEL_ELEM_DECLINE: u32 = 1;

/// Whether local slot `slot` of `code` can be a kernel element slot at all:
/// the chunk-settled half of the conditions.
// Cost: O(1) (memoized per chunk by `local_read_class`).
pub(super) fn kernel_elem_slot_ok(code: &CompiledCode, slot: u32) -> bool {
    code.lean_element_store_slot(slot as usize)
}

/// Whether the kernel about to run may serve element accesses to the `@`
/// local in `slot` (`name_idx` is the store's `IndexAssignLocal` name
/// operand; a read does not consult it). Asked once per kernel entry for each
/// element slot; every answer here stays true until code runs, and a kernel
/// runs none.
// Cost: O(1) expected: a few latch loads, plus a few symbol-keyed probes for a
// store.
pub(super) fn kernel_elem_entry_ok(
    interp: &Interpreter,
    code: &CompiledCode,
    slot: u32,
    name_idx: u32,
    store: bool,
) -> bool {
    let idx = slot as usize;
    if !kernel_elem_slot_ok(code, slot) || idx >= interp.locals.len() {
        return false;
    }
    // The aggregate `GetLocal` fast path's dynamic half: no `$CALLER::`
    // alias, sigilless attribute alias or atomic variable that could answer
    // this name instead of the slot.
    let unspoiled = crate::vm::vm_jit::local_read_unspoiled();
    if !unspoiled
        && (!crate::vm::vm_jit::local_read_unspoiled_by_any_name()
            || code
                .locals
                .get(idx)
                .is_none_or(|name| Interpreter::atomic_name_possible(name)))
    {
        return false;
    }
    // The process latches both fast lanes check: a thread clone reads the
    // shared snapshot, an atomic lane answers `@` reads, a second mutator
    // thread needs the container stripes, shared vars sync by name, and a
    // user `postcircumfix:<[ ]>` outranks the core subscript.
    if interp.is_thread_clone()
        || crate::runtime::shared_store::atomic_lane_entries_exist()
        || crate::value::container_lock::multi_mutator_threads_live()
        || interp.threads.shared_vars_active
        || interp.dispatch.skip_postcircumfix_overload
        || interp.rw_return_context
        || interp.user_subscript_routine_may_answer(true)
    {
        return false;
    }
    let Some((LeanStoreNode::Array(items), cell)) = interp.lean_store_slot_node(idx) else {
        return false;
    };
    if !store {
        return true;
    }
    let node = crate::gc::Gc::as_ptr(&items) as usize;
    interp.lean_store_variable_ok(code, idx, name_idx)
        && interp.lean_store_alias_agrees(code.const_sym(name_idx), node, cell.as_ref())
}

/// The array node the slot holds, in its own word or inside its binding cell,
/// when the node is one the lane serves: an ordinary `Array` with no native
/// backing, element type, `is default` or shape.
// Cost: O(1) (one refcount bump; one uncontended cell lock for a cell).
#[inline(always)]
fn plain_array_node(interp: &Interpreter, slot: u32) -> Option<crate::gc::Gc<ArrayData>> {
    let word = interp.locals.get(slot as usize)?;
    let items = match word.view() {
        ValueView::Array(items, ArrayKind::Array | ArrayKind::ItemArray) => items.clone(),
        ValueView::ContainerRef(cell) => {
            if word.container_ref_is_itemized() {
                return None;
            }
            match cell.lock().ok()?.view() {
                ValueView::Array(items, ArrayKind::Array | ArrayKind::ItemArray) => items.clone(),
                _ => return None,
            }
        }
        _ => return None,
    };
    if items.has_native_backing()
        || items.has_type_meta()
        || items.default.is_some()
        || items.shape.is_some()
    {
        return None;
    }
    Some(items)
}

/// The identity of the array node the slot holds, when the lane serves it:
/// the kernel entry compares a stored array against every other element
/// array, so a store never reaches an array the region also reads through
/// another name.
// Cost: O(1).
pub(super) fn kernel_elem_node_id(interp: &Interpreter, slot: u32) -> Option<usize> {
    plain_array_node(interp, slot).map(|items| crate::gc::Gc::as_ptr(&items) as usize)
}

/// The kind of the first element of the slot's array, which a kernel
/// compiles its reads of that array for (a read of another kind declines).
// Cost: O(1).
pub(super) fn kernel_elem_kind_hint(
    interp: &Interpreter,
    slot: u32,
) -> Option<crate::opcode::kernel_slot::SlotKind> {
    use crate::opcode::kernel_slot::SlotKind;
    match read_element(interp, slot, 0)?.view() {
        ValueView::Int(_) => Some(SlotKind::Int),
        ValueView::Num(_) => Some(SlotKind::Num),
        _ => None,
    }
}

/// The element at `index` of the slot's array, when it is present (not a hole,
/// in range, non-negative index).
// Cost: O(1).
#[inline(always)]
fn read_element(interp: &Interpreter, slot: u32, index: i64) -> Option<Value> {
    let i = usize::try_from(index).ok()?;
    let items = plain_array_node(interp, slot)?;
    items.get(i).cloned()
}

/// `@a[$i]` for an element that is a plain `Int`.
///
/// # Safety
/// `interp` and `code` are the live interpreter and chunk the kernel entry
/// received; `out` points to a writable `i64`. No other borrow of the
/// interpreter is live during the call.
// Cost: O(1).
pub(super) unsafe extern "C" fn kernel_elem_read_int(
    interp: *mut Interpreter,
    _code: *const CompiledCode,
    slot: u32,
    index: i64,
    out: *mut i64,
) -> u32 {
    // SAFETY: see the function's safety contract.
    let vm = unsafe { &*interp };
    match read_element(vm, slot, index).as_ref().map(Value::view) {
        Some(ValueView::Int(n)) => {
            // SAFETY: `out` is writable (contract).
            unsafe { *out = n };
            KERNEL_ELEM_OK
        }
        _ => KERNEL_ELEM_DECLINE,
    }
}

/// `@a[$i]` for an element that is a plain `Num`.
///
/// # Safety
/// As [`kernel_elem_read_int`], with `out` a writable `f64`.
// Cost: O(1).
pub(super) unsafe extern "C" fn kernel_elem_read_num(
    interp: *mut Interpreter,
    _code: *const CompiledCode,
    slot: u32,
    index: i64,
    out: *mut f64,
) -> u32 {
    // SAFETY: see the function's safety contract.
    let vm = unsafe { &*interp };
    match read_element(vm, slot, index).as_ref().map(Value::view) {
        Some(ValueView::Num(f)) => {
            // SAFETY: `out` is writable (contract).
            unsafe { *out = f };
            KERNEL_ELEM_OK
        }
        _ => KERNEL_ELEM_DECLINE,
    }
}

/// Write `value` at `index` of the slot's array when the slot takes a plain
/// store there (in range, or exactly one past the end), as the lean
/// `IndexAssignLocal` store does. Nothing is written on a decline.
// Cost: O(1) amortized.
#[inline(always)]
fn store_element(interp: &Interpreter, slot: u32, index: i64, value: Value) -> u32 {
    let Ok(i) = usize::try_from(index) else {
        return KERNEL_ELEM_DECLINE;
    };
    let Some(items) = plain_array_node(interp, slot) else {
        return KERNEL_ELEM_DECLINE;
    };
    if !Interpreter::element_slot_takes_plain_store(&items, i) {
        return KERNEL_ELEM_DECLINE;
    }
    // SAFETY: audited aliased in-place container write (see
    // `value::aliased_mut`), the one the lean `IndexAssignLocal` store makes.
    // No borrow into this node is live across the write: `items` is a `Gc`
    // handle and the view and cell guard it came from are dropped; no second
    // mutator thread exists (`kernel_elem_entry_ok` checked
    // `multi_mutator_threads_live`, and only code the kernel does not run
    // could start one), so this thread is the only writer of the node.
    let data: &mut ArrayData = unsafe { crate::value::gc_contents_mut(&items) };
    data.store_element(i, value);
    KERNEL_ELEM_OK
}

/// `@a[$i] = $v` for an `Int` value.
///
/// # Safety
/// `interp` and `code` are the live interpreter and chunk the kernel entry
/// received, and `kernel_elem_entry_ok` answered true for this slot at this
/// kernel entry. No other borrow of the interpreter is live during the call.
// Cost: O(1) amortized.
pub(super) unsafe extern "C" fn kernel_elem_store_int(
    interp: *mut Interpreter,
    _code: *const CompiledCode,
    slot: u32,
    index: i64,
    value: i64,
) -> u32 {
    // SAFETY: see the function's safety contract.
    let vm = unsafe { &*interp };
    store_element(vm, slot, index, Value::int(value))
}

/// `@a[$i] = $v` for a `Num` value.
///
/// # Safety
/// As [`kernel_elem_store_int`].
// Cost: O(1) amortized.
pub(super) unsafe extern "C" fn kernel_elem_store_num(
    interp: *mut Interpreter,
    _code: *const CompiledCode,
    slot: u32,
    index: i64,
    value: f64,
) -> u32 {
    // SAFETY: see the function's safety contract.
    let vm = unsafe { &*interp };
    store_element(vm, slot, index, Value::num(value))
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

    /// An interpreter whose root frame holds `value` in every slot of `code`.
    fn interp_with(code: &CompiledCode, slot: u32, value: Value) -> Interpreter {
        let mut interp = Interpreter::new();
        let mut slots = vec![Value::NIL; code.locals.len()];
        slots[slot as usize] = value;
        interp.locals.install_root_frame(&slots);
        interp
    }

    fn read_int(interp: &mut Interpreter, slot: u32, index: i64) -> Option<i64> {
        let mut out = 0i64;
        // SAFETY: a live interpreter, a writable `out`; `code` is unused.
        let status =
            unsafe { kernel_elem_read_int(interp, std::ptr::null(), slot, index, &mut out) };
        (status == KERNEL_ELEM_OK).then_some(out)
    }

    fn read_num(interp: &mut Interpreter, slot: u32, index: i64) -> Option<f64> {
        let mut out = 0f64;
        // SAFETY: as `read_int`.
        let status =
            unsafe { kernel_elem_read_num(interp, std::ptr::null(), slot, index, &mut out) };
        (status == KERNEL_ELEM_OK).then_some(out)
    }

    fn store_int(interp: &mut Interpreter, slot: u32, index: i64, value: i64) -> u32 {
        // SAFETY: a live interpreter; `code` is unused.
        unsafe { kernel_elem_store_int(interp, std::ptr::null(), slot, index, value) }
    }

    fn elements(interp: &Interpreter, slot: u32) -> Vec<Value> {
        match interp.locals[slot as usize].view() {
            ValueView::Array(items, _) => items.items().to_vec(),
            _ => panic!("slot holds an array"),
        }
    }

    const SRC: &str = "my @a = 1, 2; @a[0] = 5;";

    fn plain() -> Value {
        Value::real_array(vec![Value::int(10), Value::num(2.5), Value::str_from("x")])
    }

    #[test]
    fn a_plain_declared_array_slot_qualifies() {
        let (code, slot, name_idx) = compile(SRC);
        assert!(kernel_elem_slot_ok(&code, slot));
        let interp = interp_with(&code, slot, plain());
        // The process latches are shared by every test in this binary (a
        // test elsewhere may have declared an atomic or started a thread), so
        // the positive answer is only required while they are all clear.
        let latches_clear = crate::vm::vm_jit::local_read_unspoiled()
            && !crate::runtime::shared_store::atomic_lane_entries_exist()
            && !crate::value::container_lock::multi_mutator_threads_live();
        if latches_clear {
            assert!(kernel_elem_entry_ok(&interp, &code, slot, name_idx, false));
            assert!(kernel_elem_entry_ok(&interp, &code, slot, name_idx, true));
        }
        // A slot that holds no array never qualifies, whatever the latches.
        let not_array = interp_with(&code, slot, Value::int(1));
        assert!(!kernel_elem_entry_ok(
            &not_array, &code, slot, name_idx, false
        ));
    }

    #[test]
    fn a_scalar_slot_does_not_qualify() {
        let (stmts, _) = crate::parse_dispatch::parse_source("my $x = 1;").expect("parses");
        let (code, _) = crate::compiler::Compiler::new().compile(&stmts);
        assert!(!kernel_elem_slot_ok(&code, 0));
    }

    #[test]
    fn reads_take_only_an_element_of_the_asked_kind() {
        let (code, slot, _) = compile(SRC);
        let mut interp = interp_with(&code, slot, plain());
        assert_eq!(read_int(&mut interp, slot, 0), Some(10));
        assert_eq!(read_num(&mut interp, slot, 1), Some(2.5));
        // The other kind, a Str, out of range, negative: all declined.
        assert_eq!(read_num(&mut interp, slot, 0), None);
        assert_eq!(read_int(&mut interp, slot, 1), None);
        assert_eq!(read_int(&mut interp, slot, 2), None);
        assert_eq!(read_int(&mut interp, slot, 3), None);
        assert_eq!(read_int(&mut interp, slot, -1), None);
    }

    #[test]
    fn stores_write_in_range_and_append_only() {
        let (code, slot, _) = compile(SRC);
        let mut interp = interp_with(&code, slot, plain());
        assert_eq!(store_int(&mut interp, slot, 1, 7), KERNEL_ELEM_OK);
        // One past the end appends; the next read sees the new length.
        assert_eq!(store_int(&mut interp, slot, 3, 8), KERNEL_ELEM_OK);
        assert_eq!(read_int(&mut interp, slot, 3), Some(8));
        // SAFETY: as `store_int`.
        let status = unsafe { kernel_elem_store_num(&mut interp, std::ptr::null(), slot, 0, 1.5) };
        assert_eq!(status, KERNEL_ELEM_OK);
        assert_eq!(read_num(&mut interp, slot, 0), Some(1.5));
        let before = elements(&interp, slot);
        // A gap and a negative index decline and change nothing.
        assert_eq!(store_int(&mut interp, slot, 9, 1), KERNEL_ELEM_DECLINE);
        assert_eq!(store_int(&mut interp, slot, -1, 1), KERNEL_ELEM_DECLINE);
        let after = elements(&interp, slot);
        assert_eq!(before.len(), after.len());
        assert_eq!(after.len(), 4);
    }

    #[test]
    fn a_slot_that_no_longer_holds_an_array_declines_every_access() {
        let (code, slot, _) = compile(SRC);
        let mut interp = interp_with(&code, slot, Value::int(3));
        assert_eq!(read_int(&mut interp, slot, 0), None);
        assert_eq!(store_int(&mut interp, slot, 0, 1), KERNEL_ELEM_DECLINE);
        assert!(matches!(
            interp.locals[slot as usize].view(),
            ValueView::Int(3)
        ));
    }

    #[test]
    fn a_list_value_in_the_slot_declines() {
        let (code, slot, _) = compile(SRC);
        let mut interp = interp_with(&code, slot, Value::array(vec![Value::int(1)]));
        assert_eq!(read_int(&mut interp, slot, 0), None);
        assert_eq!(store_int(&mut interp, slot, 0, 1), KERNEL_ELEM_DECLINE);
    }
}
