use Test;

# ADR-0059 Slice 2: `return-rw` hands the caller a first-class container, for a
# bare scalar lexical and for a list of them -- not only for a subscript.

plan 39;

# --- A. binding a returned scalar container ---------------------------------
{
    my $v = 1;
    sub f() { return-rw $v }
    my $r := f();
    $r = 5;
    is $v, 5, 'binding a returned scalar container writes through to the source';
    $v = 7;
    is $r, 7, 'the binding sees a later write to the source (one shared cell)';
}

# --- A2. a container that outlives its declaring scope ----------------------
{
    sub g() { return-rw my $x = 1 }
    my $r := g();
    $r = 5;
    is $r, 5, 'a returned inline-declared container outlives the callee frame';
    my $r2 := g();
    $r2 = 9;
    is $r2, 9, 'a second call yields its own container';
    is $r, 5, 'and does not disturb the first';
}

# --- B. return-rw of several values returns containers, not values ----------
{
    my $a = 1;
    my $b = 2;
    sub h() { return-rw $a, $b }
    (h())[0] = 9;
    is $a, 9, 'element write through a directly-subscripted return-rw list';
    (h())[1] = 8;
    is $b, 8, 'and through the second element';
}

{
    my $a = 1;
    my $b = 2;
    sub h2() { return-rw $a, $b }
    my @r := h2();
    @r[0] = 9;
    is $a, 9, 'element write through a bound return-rw list';
    is @r[1], 2, 'the untouched element still reads its source';
    $b = 4;
    is @r[1], 4, 'and tracks a later write to it';
}

# --- CONTROL: the subscript operand, which already worked -------------------
{
    my @a = 1, 2, 3;
    sub e() { return-rw @a[0] }
    my $r := e();
    $r = 9;
    is-deeply @a, [9, 2, 3], 'return-rw of an array element still binds the element';
}

# --- return-rw of a hash element -------------------------------------------
{
    my %h = a => 1;
    sub k() { return-rw %h<a> }
    my $r := k();
    $r = 5;
    is %h<a>, 5, 'return-rw of an existing hash element writes through';
}

{
    my %h;
    sub k2() { return-rw %h<new> }
    k2() = 3;
    is %h<new>, 3, 'return-rw of a missing hash key autovivifies on write';
}

# --- return-rw of an attribute ---------------------------------------------
{
    class C {
        has $.n is rw = 1;
        method slot() { return-rw $!n }
    }
    my $c = C.new;
    $c.slot() = 42;
    is $c.n, 42, 'return-rw of an attribute writes through to the instance';
}

# --- return-rw of a state variable -----------------------------------------
{
    sub counter() { state $s = 0; return-rw $s }
    counter() = 10;
    is counter(), 10, 'return-rw of a state variable writes through';
    my $r := counter();
    $r = 11;
    is counter(), 11, 'and the state cell is shared with a binding';
}

# --- assignment / compound forms still work --------------------------------
{
    my $v = 1;
    sub f2() { return-rw $v }
    f2() = 5;
    is $v, 5, 'assignment through a return-rw call still writes the source';
    f2() += 3;
    is $v, 8, 'compound assignment through a return-rw call';
    f2()++;
    is $v, 9, 'postfix increment through a return-rw call';
    ++f2();
    is $v, 10, 'prefix increment through a return-rw call';
}

# --- cell invisibility: the container must not surface as a value ----------
{
    my $v = 1;
    sub f3() { return-rw $v }
    is f3(), 1, 'say/Str context reads the value';
    is f3().raku, '1', '.raku of the returned container is the value';
    is f3().gist, '1', '.gist of the returned container is the value';
    is f3().Str, '1', '.Str of the returned container is the value';
    is f3().elems, 1, '.elems of the returned container is 1';
    is f3() + 1, 2, 'arithmetic decontainerizes';
    ok f3() == 1, 'numeric comparison decontainerizes';
    is f3().succ, 2, 'method dispatch runs on the inner value';
    my $copy = f3();
    $copy = 99;
    is $v, 1, 'plain assignment from a return-rw call copies, it does not alias';
    is-deeply [f3()], [1], 'a bracket array holds the value';
    is (f3(), 2)[0], 1, 'list context holds the value';
    sub what-of($x) { $x.WHAT }
    is what-of(f3()), Int, 'parameter binding decontainerizes';
}

# --- cell invisibility: the SOURCE container must not change shape ---------
{
    my $v = 1;
    my @a = 1, 2;
    my %h = a => 1;
    sub fv() { return-rw $v }
    sub fa() { return-rw @a[0] }
    sub fh() { return-rw %h<a> }
    fv(); fa(); fh();
    is $v.raku, '1', 'the source scalar still renders as its value';
    is @a.raku, '[1, 2]', 'the source array still renders without a cell';
    is %h.raku, '{:a(1)}', 'the source hash still renders without a cell';
    is @a.elems, 2, 'the source array still reports its own length';
    is $v.WHAT, Int, 'the source scalar still reports its own type';
}

# --- return (not return-rw) is still a value copy --------------------------
{
    my $v = 1;
    sub plain() { return $v }
    my $r := plain();
    dies-ok { $r = 5 }, 'a plain `return` of a variable is not assignable';
    is $v, 1, 'and the source is untouched';
}
