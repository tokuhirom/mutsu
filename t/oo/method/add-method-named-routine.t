use Test;

# `.^add_method(name, $code)` must install a callable method whatever shape the
# code object arrives in. mutsu used to silently install an EMPTY one whenever
# `$code` was a NAMED, separately-declared routine: the method was registered
# (`.^can`, `.^lookup`, `.^methods`, its `.name` and `.signature` were all
# right) and calling it returned `Nil` with no error.
#
# `todo/deep/direct-metamodel-classhow-new-type-immutable-error.md` recorded
# this as "a method-dispatch gap on a `new_type`-minted Package". Measured
# 2026-09-06 against raku v2026.07, it has nothing to do with `new_type`: an
# ordinary `class C { }` shows it identically, and what actually decides the
# outcome is whether the code object is anonymous or a named declaration.
#
# The value itself was always fine -- calling the same `my method m() { 42 }`
# directly through its variable returned 42. `add_method` builds its `MethodDef`
# from the Sub's AST `body` plus `compiled_code`, and a Sub built from a DECLARED
# routine carries its bytecode in `compiled_routine` instead (ADR-0019 C6c
# stopped the declaration plan shipping an executable AST); `MethodDef` has no
# field for it, so it now takes the routine's own `CompiledCode`.

plan 16;

# What already works: every anonymous shape.
{
    class A1 { }
    A1.^add_method('m', method () { 1 });
    A1.^compose;
    is A1.m(), 1, 'an anonymous `method` installs and runs';
}
{
    class A2 { }
    A2.^add_method('m', my method () { 2 });
    A2.^compose;
    is A2.m(), 2, 'an anonymous `my method` installs and runs';
}
{
    class A3 { }
    A3.^add_method('m', anon method m3() { 3 });
    A3.^compose;
    is A3.m(), 3, 'an `anon method` with a name installs and runs';
}
{
    class A4 { }
    A4.^add_method('m', -> $s { 4 });
    A4.^compose;
    is A4.m(), 4, 'a pointy block installs and runs';
}

# Registration is right even for the shapes that do not run.
{
    class A8 { }
    A8.^add_method('m', my method m8() { 8 });
    A8.^compose;
    is A8.^can('m').elems, 1, 'a named `my method` IS registered';
    ok A8.^lookup('m').defined, 'and IS findable by lookup';
}

# What does not run.
{
    class A5 { }
    A5.^add_method('m', my method m5() { 5 });
    A5.^compose;
    is A5.m(), 5, 'a named `my method` installs and runs';
}
{
    class A6 { }
    A6.^add_method('m', my method m6(A6:) { 6 });
    A6.^compose;
    is A6.m(), 6, 'a named `my method` with a type invocant installs and runs';
}
{
    # The doc's own MOP example (`Language/mop.rakudoc`), which is what the
    # ticket was filed from. It is the row above, not a `new_type` problem.
    constant A7 := Metamodel::ClassHOW.new_type(name => 'A7');
    A7.^add_method('m', my method m7(A7:) { 7 });
    A7.^compose;
    is A7.m(), 7, 'the MOP example from Language/mop.rakudoc runs';
}

# A smaller, separate divergence measured alongside: the installed method keeps
# the name it was ADDED under, where raku keeps the routine's own name.
{
    class A9 { }
    A9.^add_method('m', my method m9() { 9 });
    A9.^compose;
    todo 'mutsu reports the added name, raku the routine name';
    is A9.^lookup('m').name, 'm9', 'lookup reports the routine name';
}

# Shapes that exercise the routine calling convention through the method ABI.
{
    class B1 { has $.n = 7 }
    B1.^add_method('get', my method g1() { self.n });
    B1.^compose;
    is B1.new.get(), 7, 'a routine-backed method reads the invocant\'s attributes';
}
{
    class B2 { }
    B2.^add_method('add', my method g2($a, $b) { $a + $b });
    B2.^compose;
    is B2.add(3, 4), 7, 'positional parameters bind';
}
{
    class B3 { }
    B3.^add_method('nam', my method g3(:$k = 9) { $k });
    B3.^compose;
    is B3.nam(:k(5)), 5, 'a named parameter binds';
    is B3.nam(), 9, 'and its default applies';
}
{
    class B5 { has $.v = 3 }
    B5.^add_method('d', my method g5(B5:D:) { self.v });
    B5.^compose;
    is B5.new.d(), 3, 'a :D invocant constraint binds';
}
{
    class B7 { }
    B7.^add_method('r', my method g7() { return 11; 99 });
    B7.^compose;
    is B7.r(), 11, 'an explicit `return` inside the routine body works';
}
