use lib $?FILE.IO.parent.add('lib').Str;
use Test;
use ContainerSlotLexical;

# ADR-0039 §10.1: the re-measured container-lexical matrix.
#
# ADR-0039 §4.3 claimed slice 1's exclusion list (`our`, `state`, `is export`,
# `$*dynamic`, `::`-qualified, type-constrained, anonymous container) was "the
# list of things slice 2 must subsume". Measured on 2026-09-06, every one of
# those rows already agrees with `raku` — they were excluded from slice 1's
# *store*, not from correctness. This file pins that so the claim cannot rot
# back into a to-do item, and so a future slice-2 attempt has its acceptance
# corpus ready.
#
# One row per declarator, in BOTH shapes that make the collision likely: a
# module's own file-scope container seen from its own routines while the
# consumer holds a same-named one, and a mainline named sub's container across
# a shadowing inner block. Every assertion is byte-identical under `raku`.
#
# Everything that needs a MAINLINE binding is written at file scope on purpose:
# a `my` inside a block is a different (block) binding, and the shapes under
# test are specifically about a mainline lexical seen from a named sub.

plan 43;

# --- module file scope: the module's own containers are not the caller's -----

my @items = <x y z>;
my %hs = (mine => 1);
my-push("c");
my-hset("m");
is my-peek(), 'a,b,c', 'module my @ sees its own binding';
is my-hpeek(), 'k,m', 'module my % sees its own binding';
is @items.join(","), 'x,y,z', "consumer's my @ is untouched";
is %hs.keys.sort.join(","), 'mine', "consumer's my % is untouched";

my @ours = <x y z>;
my %ourh = (mine => 1);
our-push("c");
our-hset("m");
is our-peek(), 'a,b,c', 'module our @ sees the package binding';
is our-hpeek(), 'k,m', 'module our % sees the package binding';
is @ours.join(","), 'x,y,z', "consumer's same-named my @ is untouched";
is %ourh.keys.sort.join(","), 'mine', "consumer's same-named my % is untouched";
is @ContainerSlotLexical::ours.join(","), 'a,b,c',
    'the package-qualified mirror agrees';

my @st = <x y z>;
state-push("c");
is state-peek(), 'a,b,c', 'module state @ sees its own binding';
is @st.join(","), 'x,y,z', "consumer's same-named my @ is untouched (state)";

my @ti = <x y z>;
typed-push(9);
is typed-peek(), '1,2,9', 'module type-constrained @ sees its own binding';
is @ti.join(","), 'x,y,z', "consumer's same-named untyped @ is untouched";

# The module's scalar-held container (`my $anon = [...]`) belongs here too: it
# takes the SCALAR unit-lexical lane (ADR-0024) rather than the `@`/`%` one, and
# it used to collide with a consumer's same-named `my $anon` because the
# sigil-less array mutators wrote the raw `env` entry instead of going through
# `env_root_descended_mut`. The full mutator matrix for that lane lives in
# `t/module-scalar-held-container-lexical.t`.
my $anon = [<x y z>];
anon-push("c");
is anon-peek(), 'a,b,c', 'module my $-held array sees its own binding';
is $anon.join(","), 'x,y,z', "consumer's same-named my \$ array is untouched";

my @*csldyn = <a b>;
sub dyn-wrapper() {
    my @*csldyn = <p q>;
    dyn-push("c");
    dyn-peek();
}
is dyn-wrapper(), 'p,q,c', 'a dynamic container resolves dynamically, not lexically';
is @*csldyn.join(","), 'a,b', 'the outer dynamic binding is untouched';

# --- mainline: a named sub keeps its own binding across a shadowing block ----

my @names = <a b>;
my %hn = (k => 'v');
sub add-name($v) { @names.push($v) }
sub read-names() { @names.join(",") }
sub hset($k) { %hn{$k} = 1 }
sub hread() { %hn.keys.sort.join(",") }
add-name("c");
is read-names(), 'a,b,c', 'mainline named sub mutates the mainline @';
{
    my @names = <x y z>;
    my %hn = (mine => 1);
    add-name("d");
    hset("m");
    is @names.join(","), 'x,y,z', 'the shadowing block @ is untouched by the sub';
    is read-names(), 'a,b,c,d', 'the sub still sees the mainline @';
    is %hn.keys.sort.join(","), 'mine', 'the shadowing block % is untouched';
    is hread(), 'k,m', 'the sub still sees the mainline %';
}
is read-names(), 'a,b,c,d', 'the mainline @ kept both pushes';
is hread(), 'k,m', 'the mainline % kept its key';

our @oc = <a b>;
our %od = (k => 'v');
sub oadd($v) { @oc.push($v) }
sub ohset($k) { %od{$k} = 1 }
sub opeek() { @oc.join(",") }
sub ohpeek() { %od.keys.sort.join(",") }
oadd("c");
is opeek(), 'a,b,c', 'a mainline our @ is mutated by its named sub';
{
    my @oc = <x y z>;
    my %od = (mine => 1);
    oadd("d");
    ohset("m");
    is @oc.join(","), 'x,y,z', 'the shadowing block my @ is untouched (our)';
    is opeek(), 'a,b,c,d', 'the our @ still sees its own binding';
    is %od.keys.sort.join(","), 'mine', 'the shadowing block my % is untouched (our)';
    is ohpeek(), 'k,m', 'the our % still sees its own binding';
}

module Q { our @arr = <a b>; our %hsh = (k => 'v'); }
sub qadd($v) { @Q::arr.push($v) }
sub qhset($k) { %Q::hsh{$k} = 1 }
qadd("c");
qhset("m");
is @Q::arr.join(","), 'a,b,c', 'a ::-qualified @ resolves to the package';
is %Q::hsh.keys.sort.join(","), 'k,m', 'a ::-qualified % resolves to the package';
{
    my @arr = <x y z>;
    my %hsh = (mine => 1);
    qadd("d");
    qhset("n");
    is @arr.join(","), 'x,y,z', 'a same-named block my @ does not capture the qualified write';
    is @Q::arr.join(","), 'a,b,c,d', 'the qualified @ kept both pushes';
    is %hsh.keys.sort.join(","), 'mine', 'a same-named block my % is untouched';
    is %Q::hsh.keys.sort.join(","), 'k,m,n', 'the qualified % kept both keys';
}

my Int @tc = 1, 2;
my Int %td = (k => 5);
sub tadd($v) { @tc.push($v) }
sub tset($k) { %td{$k} = 1 }
tadd(3);
is @tc.join(","), '1,2,3', 'a type-constrained mainline @ is mutated by its sub';
{
    my @tc = <x y z>;
    my %td = (mine => 1);
    tadd(4);
    tset("m");
    is @tc.join(","), 'x,y,z', 'the untyped shadow @ is untouched';
    is %td.keys.sort.join(","), 'mine', 'the untyped shadow % is untouched';
}
is @tc.join(","), '1,2,3,4', 'the typed @ kept both pushes';
is %td.keys.sort.join(","), 'k,m', 'the typed % kept its key';

# --- §8.2's cross-thread rows, closed by §8.6: a routine-local container must
#     not escape into an unrelated caller once any `start` has run ------------

sub work($tag) {
    my @esc = ($tag,);
    await start { 1 };
    @esc.push("$tag-2");
}
my @esc = <x y z>;
work('A');
is-deeply @esc, ["x", "y", "z"], 'a callee-local container does not escape into the caller';
@esc.push('MINE');
work('B');
is-deeply @esc, ["x", "y", "z", "MINE"], 'and stays intact across a second call';

sub takes(@list is copy) { await start { 1 }; @list.push('R') }
my @list = <x y z>;
takes(<p q>);
is-deeply @list, ["x", "y", "z"], 'a non-slurpy @ parameter does not escape the call';
