use v6;
use Test;

# A `for` loop's declared parameter types (single- or multi-param) were
# accepted by the parser and then silently ignored at run time -- neither
# form raised a binding error, unlike a routine call.
# (todo/tickets/for-loop-multi-param-types-unenforced.md)

plan 9;

sub run-single(@items) {
    my @seen;
    my $err = '';
    my $class = '';
    {
        for @items -> Int $x { @seen.push($x) }
        CATCH { default { $err = .Str; $class = .^name } }
    }
    (@seen.join(","), $err, $class);
}

sub run-multi(@items) {
    my @seen;
    my $err = '';
    {
        for @items -> Str $k, Int $v { @seen.push("$k=$v") }
        CATCH { default { $err = .Str } }
    }
    (@seen.join("|"), $err);
}

# -- single param --

my ($ok_seen, $ok_err, $ok_class) = run-single([1, 2]);
is $ok_seen, "1,2", "a matching single-param type binds normally";

my ($bad_seen, $bad_err, $bad_class) = run-single([1, "two"]);
ok $bad_err ~~ /'Type check failed in binding to parameter'/ && $bad_err ~~ /"'\$x'"/,
    "single-param: mismatched type raises a binding error";
ok $bad_err ~~ /'expected Int'/ && $bad_err ~~ /'got Str'/,
    "single-param: the error names both types";
is $bad_class, 'X::TypeCheck::Binding::Parameter',
    "single-param: the exception class matches raku's";

# -- multi param --

my ($multi_ok_seen, $multi_ok_err) = run-multi(["a", 1, "b", 2]);
is $multi_ok_seen, "a=1|b=2", "matching multi-param types bind normally";

my ($multi_bad_seen, $multi_bad_err) = run-multi(["a", 1, "b", "two"]);
is $multi_bad_seen, "a=1", "multi-param: the loop ran for every item before the bad one";
ok $multi_bad_err ~~ /'Type check failed in binding to parameter'/ && $multi_bad_err ~~ /"'\$v'"/,
    "multi-param: mismatched type raises a binding error for the right param";
ok $multi_bad_err ~~ /'expected Int'/ && $multi_bad_err ~~ /'got Str'/ && $multi_bad_err ~~ /'"two"'/,
    "multi-param: the error names both types and the offending value";

# A default value for a short chunk is exempt from the check (it never came
# from the source), matching how a routine call skips an unpassed optional.
my @default-seen;
for ("a",) -> Str $k, Int $v = 99 { @default-seen.push("$k=$v") }
is @default-seen.join(","), "a=99", "a defaulted (unfilled) slot is not type-checked";
