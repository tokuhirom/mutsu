use Test;

plan 6;

# A method (fast method-call path) that declares its own `%pars` / `@pars` and
# spawns a thread must not leave its entry answering the caller's same-named
# variable once it returns (#15020).

sub emit { start try { 1 } }

class K {
    method plain { my %pars; %pars<callee> = 1; emit(); %pars }
    method awaiting { my %pars; %pars<callee> = 1; await start { 1 }; %pars }
    method with-arg(Int $x) { my %pars = callee => $x; emit(); %pars }
    method array { my @pars; @pars.push(1); emit(); @pars }
}

{
    my %pars; %pars<a> = 5;
    K.new.plain;
    %pars<z> = 1;
    is %pars.keys.sort.join(","), "a,z", "plain method";
}
{
    my %pars; %pars<a> = 5;
    K.new.awaiting;
    %pars<z> = 1;
    is %pars.keys.sort.join(","), "a,z", "method that awaits";
}
{
    my %pars; %pars<a> = 5;
    K.new.with-arg(3);
    %pars<z> = 1;
    is %pars.keys.sort.join(","), "a,z", "method with a positional argument";
}
{
    my @pars = 5;
    K.new.array;
    @pars.push(2);
    is @pars.join(","), "5,2", "array variable";
}
{
    my %pars; %pars<a> = 5;
    my $r = K.new.plain;
    is $r.keys.join(","), "callee", "the method still returns its own hash";
    is %pars.keys.join(","), "a", "and the caller's is untouched";
}
