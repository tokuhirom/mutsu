use Test;

plan 4;

# Assigning through an rw method that answers a write-only Proxy runs its
# STORE alone: the sunk assignment, and a block ending in that assignment
# called from a `for` loop, do not FETCH it (Red's legacy migrations).

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
my @blocks = { .a = 3 }, { .a = 4 };
.(Type) for @blocks;
is-deeply @log, ['store 3', 'store 4'], 'blocks called from a for loop do not FETCH';

@log = ();
for @blocks -> &b { b(Type) }
is-deeply @log, ['store 3', 'store 4'], 'nor when called by name';

@log = ();
lives-ok { .(Type) for @blocks }, 'no FETCH dies';
