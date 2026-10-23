use Test;

# ADR-14632 §2.5: a named element store whose target the compiler resolved to
# a local slot compiles to `IndexAssignLocal`; `IndexAssignExprNamed` is left
# for a target with no slot (a dynamic, a free variable).

plan 4;

sub ops(Str $code) {
    my $proc = run($*EXECUTABLE, '--dump-bytecode', '-e', $code, :out, :err);
    $proc.out.slurp(:close).lines.grep(*.contains('IndexAssign')).list
}

my @local = ops('sub f() { my @a; @a[0, 1] = 1, 2; my %h; %h<a b> = 1, 2 }');
is @local.grep(*.contains('IndexAssignLocal')).elems, 2,
    'element stores into a routine\'s own @a and %h compile to IndexAssignLocal';
is @local.grep(*.contains('IndexAssignExprNamed')).elems, 0,
    'no slot-resolved store compiles to IndexAssignExprNamed';

my @named = ops('@*ARGS[0, 1] = 1, 2');
is @named.grep(*.contains('IndexAssignExprNamed')).elems, 1,
    'a store into a dynamic compiles to IndexAssignExprNamed';
nok @named.first(*.contains('IndexAssignExprNamed')).contains('target_slot'),
    'IndexAssignExprNamed carries no slot';
