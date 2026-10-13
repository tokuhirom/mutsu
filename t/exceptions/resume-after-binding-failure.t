use Test;

plan 6;

# `.resume` of an exception raised by the parameter binder continues after the
# failing call, in a `try` body and in a mainline CATCH, any number of times.
sub f(Int $x) { $x }
my $s = "a";

my @log;
try {
    f($s);
    @log.push("mid");
    CATCH { default { @log.push("caught " ~ .^name); .resume } }
}
is @log.join(","), "caught X::TypeCheck::Binding::Parameter,mid",
    "resume after a binding failure in a try body continues";

my @main;
sub g(UInt $x) { $x }
sub h($v --> UInt) { $v }
{
    g(-1);
    @main.push("one");
    g(-1);
    @main.push("two");
    h(-2);
    @main.push("three");
    CATCH { default { @main.push(.^name); .resume } }
}
is @main.elems, 6, "every failure in the block is handled and resumed";
is @main[0], "X::TypeCheck::Binding::Parameter", "first failure is the binder's";
is @main[1], "one", "the code after the first failing call runs";
is @main[2], "X::TypeCheck::Binding::Parameter", "a second failure is caught again";
is @main[4], "X::TypeCheck::Return", "a return-type failure after a resume is caught";
