use Test;

# Regression: a class-BODY-level `my atomicint $x` read via `⚛++` inside an
# attribute's default-value expression, evaluated at instance-construction
# time, must correctly increment starting from the declared initial value.
#
# The attribute default-value expression compiles as its own standalone
# bytecode chunk with NO local slots (`Compiler::new_decl_chunk_compiler`),
# so a bare free-variable name in it resolves through the environment the
# declaration registers in -- for a class-body `my`, that is the per-package
# "static" store `package_lexicals`, not `env`. The atomic-by-name builtins
# (`__mutsu_atomic_post_inc_var` et al.) previously consulted only `env`
# (and a process-global name-keyed legacy lane), missing `package_lexicals`
# entirely -- so the FIRST read saw an uninitialized placeholder (printing
# as the bare `(atomicint)` type object) and every instance after that got
# the value the PREVIOUS instance should have gotten (off-by-one).
#
# See todo/tickets/class-level-atomicint-attribute-default-first-instance-wrong.md
# (now resolved) and news/2026-08/ for the fix.

plan 4;

subtest 'two instances, non-native attribute' => {
    plan 2;
    class Foo {
        my atomicint $current-id = 1;
        has $.id = $current-id⚛++;
    }
    my $a = Foo.new;
    my $b = Foo.new;
    is $a.id, 1, 'first instance gets the declared initial value';
    is $b.id, 2, 'second instance gets the next value';
}

subtest 'four instances stay in lock-step (no shifted off-by-one)' => {
    plan 4;
    class Bar {
        my atomicint $current-id = 1;
        has $.id = $current-id⚛++;
    }
    my @ids = (Bar.new, Bar.new, Bar.new, Bar.new).map(*.id);
    is @ids[0], 1, 'instance 1';
    is @ids[1], 2, 'instance 2';
    is @ids[2], 3, 'instance 3';
    is @ids[3], 4, 'instance 4';
}

subtest 'native int attribute follows the same pattern' => {
    plan 3;
    class Baz {
        my atomicint $current-id = 1;
        has int $.id = $current-id⚛++;
    }
    my @ids = (Baz.new, Baz.new, Baz.new).map(*.id);
    is @ids[0], 1, 'instance 1';
    is @ids[1], 2, 'instance 2';
    is @ids[2], 3, 'instance 3';
}

subtest 'plain (non-atomic) Int class-body static keeps working' => {
    plan 2;
    class Qux {
        my Int $current-id = 1;
        has $.id = $current-id++;
    }
    my $a = Qux.new;
    my $b = Qux.new;
    is $a.id, 1, 'first instance';
    is $b.id, 2, 'second instance';
}
