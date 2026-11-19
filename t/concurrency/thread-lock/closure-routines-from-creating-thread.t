use Test;

# A closure resolves its by-name calls through the routines of the scope it
# was written in, whichever thread runs it (#18135). A worker started early
# (here at BEGIN, as a module's mainline does during `use`) and fed closures
# through a Channel used to resolve them through its own copy of the routine
# tables, taken at its spawn: a routine declared later was "Unknown function",
# and a multi with a candidate added later dispatched over the stale set.

plan 6;

my Channel $C;
BEGIN { $C .= new; start { loop { my $s = $C.receive; $s() } } }

sub run-in-worker(&code) {
    my $p = Promise.new;
    my $v = $p.vow;
    $C.send({ CATCH { default { $v.break($_) } }; $v.keep(code()) });
    await $p
}

sub bar { 'bar' }
multi sub m(Int) { 'int' }
multi sub m(Str) { 'str' }
my sub baz { 'baz' }

is run-in-worker({ bar() }), 'bar', 'a sub declared after the worker started';
is run-in-worker({ m(1) ~ m('a') }), 'intstr', 'a multi sees every candidate';
is run-in-worker({ baz() }), 'baz', 'a my sub of the creating scope';

{
    sub inner { 'inner' }
    is run-in-worker({ inner() }), 'inner', 'a sub of an enclosing block';
}

# A routine the closure declares while it runs stays its own: the worker's
# next closure does not see it.
run-in-worker({ EVAL 'sub only-in-eval { 1 }'; 1 });
is run-in-worker({ (try EVAL 'only-in-eval()') // 'absent' }), 'absent',
    'a declaration made during one call does not leak into the next';

# Closures created on the worker and run there keep resolving there.
is run-in-worker({ my &f = { bar() }; f() }), 'bar', 'a closure created during the call';
