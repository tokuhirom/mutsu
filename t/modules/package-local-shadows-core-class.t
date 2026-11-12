use Test;

plan 6;

# A class declared in a package shadows a core class of the same name there,
# including the core types the runtime registers as classes (`Compiler`,
# `VM`, `Kernel`, `Lock`).

module M {
    class Compiler { has $.x; method compile($a) { "compiled $a with $!x" } }
    class VM { method zz { 'my VM' } }
    class Lock { method zz { 'my Lock' } }
    our sub go() { Compiler.new(:x(1)).compile(5) }
    our sub vm() { VM.zz }
    our sub lock() { Lock.zz }
    our sub name() { Compiler.^name }
}

is M::go(), 'compiled 5 with 1', 'Compiler inside the module is the local class';
is M::vm(), 'my VM', 'VM too';
is M::lock(), 'my Lock', 'Lock too';
is M::name(), 'M::Compiler', 'with the qualified name';
is Compiler.^name, 'Compiler', 'outside the module the core class remains';
isa-ok VM.new, VM, 'core VM still constructs';
