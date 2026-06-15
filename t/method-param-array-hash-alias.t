# A plain `@`/`%` positional parameter of a METHOD binds the caller's container
# by alias (Raku readonly-container semantics): `.push`, element assignment, and
# whole-container `=` all propagate to the caller — exactly as they already do
# for SUB parameters. The method fast path previously bypassed the exit-time
# writeback (only `is rw` forced the full path), so method array/hash params
# silently failed to alias their argument.
use Test;

plan 14;

# --- array param: .push propagates (instance method) ---
{
    class A1 { method add(@a) { @a.push(99) } }
    my @o = 1, 2, 3;
    A1.new.add(@o);
    is @o.join(','), '1,2,3,99', 'method array param .push propagates to caller';
}

# --- array param: .push propagates (class/type-object method) ---
{
    class A2 { method add(@a) { @a.push(7) } }
    my @o = 1, 2;
    A2.add(@o);
    is @o.join(','), '1,2,7', 'class-method array param .push propagates';
}

# --- array param: whole-container assign propagates ---
{
    class A3 { method reset(@a) { @a = (8, 8) } }
    my @o = 1, 2, 3;
    A3.new.reset(@o);
    is @o.join(','), '8,8', 'method array param whole-assign propagates';
}

# --- array param: element assignment propagates ---
{
    class A4 { method setfirst(@a) { @a[0] = 42 } }
    my @o = 1, 2, 3;
    A4.new.setfirst(@o);
    is @o.join(','), '42,2,3', 'method array param element assign propagates';
}

# --- hash param: key assignment propagates ---
{
    class H1 { method put(%h) { %h<x> = 9 } }
    my %o = a => 1;
    H1.new.put(%o);
    is %o<x>, 9, 'method hash param key assign propagates';
    is %o.elems, 2, 'method hash param hash grew';
}

# --- hash param: whole-container assign propagates ---
{
    class H2 { method reset(%h) { %h = (z => 5) } }
    my %o = a => 1, b => 2;
    H2.new.reset(%o);
    is %o.keys.sort.join(','), 'z', 'method hash param whole-assign propagates';
}

# --- is copy: does NOT propagate ---
{
    class CP { method f(@a is copy) { @a.push(99) } }
    my @o = 1, 2;
    CP.new.f(@o);
    is @o.join(','), '1,2', 'method array param is copy does not propagate';
}

# --- a literal argument has no caller variable: no crash, value seen inside ---
{
    class Lit { method total(@a) { @a.push(10); @a.elems } }
    is Lit.new.total([1, 2, 3]), 4, 'method array param works with a literal arg';
}

# --- read-only use does not corrupt the caller ---
{
    class RO { method sum(@a) { my $s = 0; $s += $_ for @a; $s } }
    my @o = 1, 2, 3, 4;
    is RO.new.sum(@o), 10, 'method array param read-only use returns correctly';
    is @o.join(','), '1,2,3,4', 'read-only use leaves the caller unchanged';
}

# --- multiple params, mixed sigils ---
{
    class Mix { method go($x, @a, %h) { @a.push($x); %h<k> = $x } }
    my @o = 1;
    my %m = a => 0;
    Mix.new.go(5, @o, %m);
    is @o.join(','), '1,5', 'mixed-sigil method: array param aliased';
    is %m<k>, 5, 'mixed-sigil method: hash param aliased';
}

# --- nested: a method calling another method, both aliasing ---
{
    class Nest {
        method outer(@a) { self.inner(@a); @a.push(2) }
        method inner(@a) { @a.push(1) }
    }
    my @o;
    Nest.new.outer(@o);
    is @o.join(','), '1,2', 'nested method calls both alias the same caller array';
}
