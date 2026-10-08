use Test;

# From the Fortran::Grammar suite (via IO::Glob): a method body runs under
# GLOBAL, so a qualified name relative to the class (`G::Mt`) must be resolved
# against the running method's class.

plan 3;

class Outer {
    class G { class Mt { has $.v } }
    method plain { G::Mt.new.^name }
    method with-args { G::Mt.new(v => 1).v }
    method type-obj { G::Mt.^name }
}

is Outer.new.plain, 'Outer::G::Mt', 'nested qualified type resolves in a method';
is Outer.new.with-args, 1, '.new with arguments on it';
is Outer.new.type-obj, 'Outer::G::Mt', 'type object name';
