use Test;

# GH-7646: `let`/`temp` resolve their saves at the end of the enclosing BLOCK,
# and a routine body IS one. Only the bare-block (`OpCode::LetBlock`) and the
# value-position `do { ... }` (GH-7635) lowerings used to implement that; a
# routine body is compiled by the closure/sub-body path instead, which emits no
# `LetBlock`, so its saves were never resolved by the frame that owned them.
# They are resolved at frame teardown now, against the value the frame produced.
#
# Every assertion below was measured against `raku` first.

plan 28;

# --- a named sub resolves its own saves ---------------------------------

{
    my $x = 1;
    sub f-let-fail() { let $x = 2; Nil }
    f-let-fail();
    is $x, 1, 'sub that yields an undefined value restores a `let`';
}

{
    my $x = 1;
    sub f-let-ok() { let $x = 2; 42 }
    f-let-ok();
    is $x, 2, 'sub that yields a defined value commits a `let`';
}

{
    my $x = 1;
    sub f-temp() { temp $x = 2; 42 }
    f-temp();
    is $x, 1, '`temp` in a sub restores even when the sub succeeds';
}

{
    my $x = 1;
    sub f-tail-let() { let $x = 2; }
    my $r = f-tail-let();
    is $r, 2, 'a tail `let $x = V` yields V, like any other assignment';
    is $x, 2, 'and that defined value commits the `let`';
}

# --- explicit `return` is a frame exit too ------------------------------

{
    my $x = 1;
    sub r-let-ok() { let $x = 2; return 42 }
    r-let-ok();
    is $x, 2, '`return` of a defined value commits a `let`';
}

{
    my $x = 1;
    sub r-let-fail() { let $x = 2; return Nil }
    r-let-fail();
    is $x, 1, '`return` of an undefined value restores a `let`';
}

{
    my $x = 1;
    sub r-temp() { temp $x = 2; return 42 }
    r-temp();
    is $x, 1, '`temp` restores across an explicit `return`';
}

# --- an exception unwinding the frame always restores -------------------

{
    my $x = 1;
    sub d-let() { let $x = 2; die 'boom' }
    try d-let();
    is $x, 1, 'a `die` restores a `let`';
}

{
    my $x = 1;
    sub fl-let() { let $x = 2; fail 'boom' }
    my $f = fl-let();
    is $x, 1, 'a `fail` restores a `let`';
    $f.so;  # defuse the Failure
}

# --- the save belongs to the frame that recorded it ---------------------

{
    my $x = 1;
    sub inner-let() { let $x = 2; Nil }
    sub outer-let() { inner-let(); 99 }
    outer-let();
    is $x, 1, "the callee's `let` is resolved by the callee, not the caller";
}

{
    my $x = 1;
    sub inner-ok() { let $x = 2; 7 }
    sub outer-nil() { inner-ok(); Nil }
    outer-nil();
    is $x, 2, "a successful callee's `let` survives an unsuccessful caller";
}

# --- methods -------------------------------------------------------------

{
    my $x = 1;
    class LetMethodFail { method m() { let $x = 2; Nil } }
    LetMethodFail.m;
    is $x, 1, 'method that yields an undefined value restores a `let`';
}

{
    my $x = 1;
    class LetMethodOk { method m() { let $x = 2; 42 } }
    LetMethodOk.m;
    is $x, 2, 'method that yields a defined value commits a `let`';
}

{
    my $x = 1;
    class TempMethod { method m() { temp $x = 2; 42 } }
    TempMethod.m;
    is $x, 1, '`temp` in a method restores';
}

{
    my $x = 1;
    class LetMethodReturn { method m() { let $x = 2; return Nil } }
    LetMethodReturn.m;
    is $x, 1, 'method `return` of an undefined value restores a `let`';
}

# --- closures ------------------------------------------------------------

{
    my $x = 1;
    my $b = -> { let $x = 2; Nil };
    $b();
    is $x, 1, 'pointy block that fails restores a `let`';
}

{
    my $x = 1;
    my $b = -> { let $x = 2; 42 };
    $b();
    is $x, 2, 'pointy block that succeeds commits a `let`';
}

{
    my $x = 1;
    my $b = sub { temp $x = 2; 42 };
    $b();
    is $x, 1, '`temp` in an anonymous sub restores';
}

{
    my $x = 1;
    my $b = { let $x = 2; Nil };
    $b();
    is $x, 1, 'invoked bare block that fails restores a `let`';
}

# --- a `try` / implicit-CATCH region is a block too ----------------------

{
    my $x = 1;
    try { temp $x = 2; };
    is $x, 1, '`temp` in a `try` block restores at the block end';
}

{
    my $x = 1;
    try { let $x = 2; Nil };
    is $x, 1, '`try` block that fails restores a `let`';
}

{
    my $x = 1;
    try { let $x = 2; 5 };
    is $x, 2, '`try` block that succeeds commits a `let`';
}

{
    my $x = 1;
    sub catch-temp() { temp $x = 2; CATCH { default { } }; 7 }
    catch-temp();
    is $x, 1, 'a routine body carrying a CATCH still restores its `temp`';
}

{
    my $x = 1;
    sub catch-let() { let $x = 2; CATCH { default { } }; Nil }
    catch-let();
    is $x, 1, 'a routine body carrying a CATCH restores a failed `let`';
}

# --- container targets ---------------------------------------------------

{
    my @a = 1, 2;
    sub arr-let() { let @a = 3, 4; Nil }
    arr-let();
    is @a.join(','), '1,2', 'a failed `let` restores an array';
}

{
    my %h = a => 1;
    sub hash-let() { let %h = b => 2; Nil }
    hash-let();
    is %h.keys.sort.join(','), 'a', 'a failed `let` restores a hash';
}

# --- the value of a `let` statement --------------------------------------

{
    my $x = 1;
    is (do { let $x = 5 }), 5, 'a tail `let` is the value of a `do` block';
}
