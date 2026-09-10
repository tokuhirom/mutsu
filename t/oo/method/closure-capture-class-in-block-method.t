use Test;

# A block/sub-literal's closure capture set must include a free variable
# referenced ONLY from inside a METHOD body of a `class`/`role` declared inside
# that block. This is the class/method sibling of
# t/closure-capture-nested-named-sub.t: a method body is compiled through
# `Compiler::compile_method_body` (ADR-0019 D3-8a) and installed into the type's
# method table by `RegisterDecl`, so -- exactly like a nested named `sub`, and
# unlike a nested anonymous closure -- it has no runtime closure-creation op and
# never lands in `closure_compiled_codes`. Without an explicit compile-time
# channel, the enclosing block's own `free_var_syms` never learns which outer
# lexicals its types' method bodies read.
#
# Each case below is verified against `raku` (v2026.06) as well as mutsu.
#
# IMPORTANT, same two rules as t/closure-capture-nested-named-sub.t:
#
# 1) Every case uses variable/class/role names unique to that case, because a
#    bare `{ ... }` test-scope block compiles INLINE into the surrounding
#    mainline code and a reused name can share the same top-level local slot.
#
# 2) Cases are NOT wrapped in a bare `{ ... }` scope block -- every case is flat
#    top-level code. A plain top-level `{ ... }` statement compiles via
#    `OpCode::BlockScope`, whose conservative env-sync gate unconditionally
#    syncs every local the block's body touches into the name-keyed env, for
#    every such block EXCEPT the textually-last one in the compilation unit.
#    That blanket sync papers over exactly this bug, so wrapping the cases would
#    silently turn nearly all of these assertions into no-ops that pass on the
#    unfixed compiler. Keep every case flat.
#
# 3) This file declares NO top-level named `sub` at all. A single mainline
#    `sub foo() {...}` anywhere in the compilation unit makes the mainline
#    `CompiledCode` sync its locals into the name-keyed env wholesale (a named
#    sub body can reference any of them and has no closure env of its own), and
#    that sync makes EVERY case in this file pass even on the unfixed compiler.
#    Confirmed by reverting the fix locally and re-running with/without a dummy
#    `sub masker() { 1 }` while writing this. Where a case needs an outer
#    routine, bind an anonymous `sub {...}` to a `my &name` instead.

plan 21;

# 1) The core repro: an outer scalar read ONLY inside a method body of a class
#    declared inside the block, with the block invoked as a stored Callable.
my $mc1 = 42;
my &blk1 = { my class C1 { method go() { $mc1 } }; C1.new.go };
is blk1(), 42, 'block value captures a var read only by a class method body';

# 2) Array and 3) hash outer containers read from a method body.
my @mc2 = (1, 2, 3);
my &blk2 = { my class C2 { method go() { @mc2.join(',') } }; C2.new.go };
is blk2(), '1,2,3', 'class method body reads an outer array';

my %mc3 = (a => 1);
my &blk3 = { my class C3 { method go() { %mc3<a> } }; C3.new.go };
is blk3(), 1, 'class method body reads an outer hash';

# 4) An outer lexical Callable (`&`-sigil lane) called from a method body.
#    NOTE: deliberately a `my &x = sub {...}` binding rather than a top-level
#    `sub helper4() {...}` declaration -- see rule 3 in the header.
my &helper4 = sub { 'from-helper4' };
my &blk4 = { my class C4 { method go() { helper4() } }; C4.new.go };
is blk4(), 'from-helper4', 'class method body calls an outer lexical Callable';

# 5) A `role` declared inside the block -- roles compose through a separate
#    registration path (`RegisterRole`) from classes.
my $mc5 = 42;
my &blk5 = {
    my role R5 { method go() { $mc5 } }
    my class C5 does R5 { }
    C5.new.go;
};
is blk5(), 42, 'role method body composed into a class reads an outer var';

# 6) `submethod`.
my $mc6 = 42;
my &blk6 = { my class C6 { submethod go() { $mc6 } }; C6.new.go };
is blk6(), 42, 'submethod body reads an outer var';

# 7) `multi method` -- each candidate compiles separately and must independently
#    contribute its free vars.
my $mc7 = 42;
my &blk7 = {
    my class C7 {
        multi method go(Int $x) { "int:$mc7:$x" }
        multi method go(Str $x) { "str:$mc7:$x" }
    }
    C7.new.go(5) ~ '/' ~ C7.new.go('a');
};
is blk7(), 'int:42:5/str:42:a', 'each multi method candidate contributes its own free vars';

# 8) Private method `method !p()`.
my $mc8 = 42;
my &blk8 = {
    my class C8 {
        method !secret() { $mc8 }
        method go() { self!secret }
    }
    C8.new.go;
};
is blk8(), 42, 'private method body reads an outer var';

# 9) The class declared two block levels deep.
my $mc9 = 42;
my &blk9 = { my &inner9 = { my class C9 { method go() { $mc9 } }; C9.new.go }; inner9() };
is blk9(), 42, 'class two block levels deep still reaches the outer var';

# 10) An outer var read only from a method PARAMETER's default value.
my $mc10 = 42;
my &blk10 = { my class C10 { method go($x = $mc10) { $x } }; C10.new.go };
is blk10(), 42, 'method parameter default value reads an outer var';

# 11) An outer var read only from an ATTRIBUTE's default value.
my $mc11 = 42;
my &blk11 = { my class C11 { has $.a = $mc11; }; C11.new.a };
is blk11(), 42, 'attribute default value reads an outer var';

# 12) `BUILD` and 13) `TWEAK` bodies.
my $mc12 = 42;
my &blk12 = {
    my class C12 {
        has $.a;
        submethod BUILD() { $!a = $mc12 }
    }
    C12.new.a;
};
is blk12(), 42, 'BUILD body reads an outer var';

my $mc13 = 42;
my &blk13 = {
    my class C13 {
        has $.a is rw;
        submethod TWEAK() { $!a = $mc13 }
    }
    C13.new.a;
};
is blk13(), 42, 'TWEAK body reads an outer var';

# 14) A method that WRITES the outer var: the write must propagate back out
#     (shared-cell capture, not a by-value snapshot).
my $mc14 = 0;
my &blk14 = { my class C14 { method go() { $mc14 = 7 } }; C14.new.go };
blk14();
is $mc14, 7, 'class method body write to an outer var is visible after the call';

# 15) Read-then-write in the same method body.
my $mc15 = 10;
my &blk15 = { my class C15 { method go() { $mc15 = $mc15 + 5; $mc15 } }; C15.new.go };
is blk15(), 15, 'class method body read-modify-write returns the updated value';
is $mc15, 15, 'class method body read-modify-write is visible to the outer scope';

# 16) NEGATIVE: `self` inside a method must be the invocant, never a same-named
#     outer lexical.
my $self = 'outer-self';
my &blk16 = { my class C16 { method go() { self.^name } }; C16.new.go };
like blk16(), /C16/, 'self in a method body is the invocant, not an outer $self';

# 17) NEGATIVE: `$.attr` / `$!attr` resolve as attributes, not as captures of a
#     same-named outer lexical.
my $a17 = 'outer-a17';
my &blk17 = {
    my class C17 {
        has $.a17 = 'attr';
        method go() { $!a17 ~ '|' ~ $.a17 }
    }
    C17.new.go;
};
is blk17(), 'attr|attr', 'attribute access is not confused with an outer lexical';

# 18) NEGATIVE: a method PARAMETER of the same name shadows the outer lexical.
my $mc18 = 42;
my &blk18 = { my class C18 { method go($mc18) { $mc18 } }; C18.new.go(100) };
is blk18(), 100, 'method parameter shadows a same-named outer lexical';

# 19) NEGATIVE: a method's own `my` declaration shadows the outer lexical.
my $mc19 = 42;
my &blk19 = { my class C19 { method go() { my $mc19 = 7; $mc19 } }; C19.new.go };
is blk19(), 7, 'method my-declaration shadows a same-named outer lexical';

# 20) NEGATIVE: `%_` inside a method is the implicit named-args hash (its own
#     lexical, resolved through the method's own store), never something the
#     declaring block has to capture on the method's behalf.
my &blk20 = { my class C20 { method go() { %_<k> // 'none' } }; C20.new.go(k => 'named') };
is blk20(), 'named', 'the implicit %_ inside a method is the named-args hash';
