//! #8997: a reduction / hyper / meta operator's spelling is fixed by the
//! source, so executing the opcode must not re-derive anything from it.
//!
//! The compiler lowers the spelling once (`mutsu::compiled_operator`), and the
//! `infix:<op>` name an operator resolves through is memoized per operator, so
//! the number of `Symbol::intern` calls a loop makes must not grow with the
//! number of times the operator runs.

fn interns_for(src: &str) -> u64 {
    let mut warm = mutsu::Interpreter::new();
    warm.run(src).expect("warm program runs");
    drop(warm);
    let before = mutsu::symbol::intern_calls();
    let mut interp = mutsu::Interpreter::new();
    interp.run(src).expect("program runs");
    mutsu::symbol::intern_calls() - before
}

/// Interns added per extra iteration, between a 100- and a 1100-iteration run.
fn slope(program: impl Fn(usize) -> String) -> f64 {
    let low = interns_for(&program(100));
    let high = interns_for(&program(1100));
    (high as i64 - low as i64) as f64 / 1000.0
}

#[test]
fn builtin_reduction_does_not_re_derive_its_operator() {
    let per_run = slope(|n| format!("my $t = 0; for ^{n} {{ $t = [+] 1, 2, 3 }}; say $t;"));
    eprintln!("[+] reduction: {per_run:.3} interns per execution");
    assert!(
        per_run <= 0.01,
        "[+] re-derives its operator per execution ({per_run:.3}); see #8997"
    );
}

/// A user-defined infix reduction still resolves `infix:<myop>` through the
/// interpreter, so its cost is dominated by the two by-name routine calls the
/// fold makes — measured at 20 interns per execution both before and after
/// #8997, all of them inside `call_user_routine_direct`. That by-name dispatch
/// is #7766's subject, not this one; what #8997 owns is that the *operator
/// name* is derived once, which `compiled_operator::infix_names`' own unit
/// test pins. The assertion here is only that the reduction does not re-derive
/// it per *fold step*: a longer operand list must add call cost and nothing
/// else.
#[test]
fn user_defined_reduction_does_not_re_derive_its_infix_name_per_step() {
    let two_steps =
        interns_for("sub infix:<myop>($a, $b) { $a + $b }; say [myop] (1, 2, 3) xx 200;");
    let four_steps =
        interns_for("sub infix:<myop>($a, $b) { $a + $b }; say [myop] (1, 2, 3, 4, 5) xx 200;");
    // Each extra element is one more call to `infix:<myop>`; the operator name
    // itself must contribute nothing on top of that.
    let per_call = (four_steps as i64 - two_steps as i64) as f64 / (2.0 * 200.0);
    let baseline = two_steps as f64 / (2.0 * 200.0);
    eprintln!("[myop] reduction: {per_call:.3} interns per extra step, {baseline:.3} per step");
    assert!(
        per_call <= baseline + 0.01,
        "a user-defined reduction re-derives `infix:<myop>` per step \
         ({per_call:.3} vs {baseline:.3}); see #8997"
    );
}

#[test]
fn meta_operator_does_not_re_derive_its_operator() {
    let per_run =
        slope(|n| format!("my $t = 0; for ^{n} {{ $t = ((1, 2) Z+ (3, 4)).elems }}; say $t;"));
    eprintln!("Z+ meta-op: {per_run:.3} interns per execution");
    assert!(
        per_run <= 0.01,
        "a Z meta-op re-derives its operator per execution ({per_run:.3}); see #8997"
    );
}

#[test]
fn hyper_operator_does_not_re_derive_its_operator() {
    let per_run =
        slope(|n| format!("my $t = 0; for ^{n} {{ $t = ((1, 2) >>+<< (3, 4))[0] }}; say $t;"));
    eprintln!(">>+<< hyper op: {per_run:.3} interns per execution");
    assert!(
        per_run <= 0.01,
        "a hyper op re-derives its operator per execution ({per_run:.3}); see #8997"
    );
}

#[test]
fn a_reduction_over_a_longer_list_does_not_cost_per_element() {
    // The operator is decoded once per *execution*, so a longer operand list
    // must not add any operator work at all.
    let low = interns_for("say [+] (1..100);");
    let high = interns_for("say [+] (1..1100);");
    let per_element = (high as i64 - low as i64) as f64 / 1000.0;
    eprintln!("[+] fold: {per_element:.3} interns per element");
    assert!(
        per_element <= 0.01,
        "a reduction re-derives its operator per element ({per_element:.3}); see #8997"
    );
}
