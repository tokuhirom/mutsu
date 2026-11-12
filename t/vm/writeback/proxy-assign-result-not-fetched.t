use Test;

plan 3;

# Assigning through an rw method that answers a write-only Proxy runs its
# STORE alone: the assignment does not FETCH the Proxy it stored into.

my @log;
my \Type = Metamodel::ClassHOW.new_type: :name<T>;
Type.^add_method: 'a', method () is rw {
    Proxy.new:
        FETCH => method { @log.push('fetch'); die 'write-only' },
        STORE => method (\data) { @log.push("store {data}") };
}
Type.^compose;

Type.a = 2;
is-deeply @log, ['store 2'], 'a sunk assignment does not FETCH';

@log = ();
Type.a = 3 for ^1;
is-deeply @log, ['store 3'], 'nor as a statement-modifier body';

@log = ();
for ^2 { Type.a = $_ }
is-deeply @log, ['store 0', 'store 1'], 'nor as a loop body';
