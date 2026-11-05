use Test;

plan 3;

# A routine reached through the light call path (positional scalar parameters
# only) declares its own `%`/`@` lexical and spawns a thread. On return the
# caller's same-named variable must not be answered with the callee's
# published lane entry (#15020).

sub emit { start try { 1 } }

sub create-hash($x) {
    my %pars;
    %pars{$x} = 1;
    emit();
    %pars
}
my %pars;
%pars<a> = 5;
create-hash("num1");
%pars<z> = 1;
is %pars.keys.sort.join(","), "a,z", "caller's %pars survives a spawning light callee";

sub create-array($x) {
    my @p;
    @p.push($x);
    await start { 1 };
    @p
}
my @p = 1;
create-array(7);
@p.push(3);
is-deeply @p, [1, 3], "caller's @p survives a spawning light callee";

sub rec($n) {
    my %h;
    %h<n> = $n;
    if $n > 0 { await start { 1 }; rec($n - 1) }
    %h<m> = $n;
    %h.sort.map(*.value).join(",")
}
is rec(2), "2,2", "a recursive frame keeps its own %h across a nested spawning call";
