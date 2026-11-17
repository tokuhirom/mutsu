use Test;

# ADR-17216 slice 2b: a type declaration registers in the shared registry
# under a lock of its own declaration site, not one process-wide lock. A
# process-wide lock held while the declaration runs user code (a class body)
# deadlocks against any user lock: thread A holds `$L` and declares a class,
# thread B is inside another class's body and takes `$L`. raku runs both.

plan 2;

{
    my $L = Lock.new;
    my $a-has-L = Promise.new;
    my $b-in-decl = Promise.new;
    my $a = start {
        $L.protect: {
            $a-has-L.keep;
            await $b-in-decl;
            EVAL 'class :: { method m { 1 } }';
        }
        'a';
    }
    my $b = start {
        await $a-has-L;
        EVAL 'class :: { $b-in-decl.keep; $L.protect({ 1 }); method n { 2 } }';
        'b';
    }
    await Promise.anyof(Promise.allof($a, $b), Promise.in(20));
    is ($a.status, $b.status).join(','), 'Kept,Kept',
        'a class body taking a user lock does not deadlock another declaring thread';
}

# The same declaration site run on several threads at once: each run
# registers its class whole (Red's `my $*OUT = class :: { method put ... }`).
{
    sub with-anon-class($i) {
        my $c = class :: {
            method put(|) { 'put' }
            method print(|) { 'print' }
        };
        $c.put ~ $c.print ~ $i;
    }
    my @r = await (^8).map: -> $i { start { (^25).map({ with-anon-class($i) }).tail } };
    is @r.join(','), (^8).map({ "putprint$_" }).join(','),
        'an anonymous class declared on many threads at once registers whole';
}
