use Test;

# A method's typed `$_ is copy` parameter keeps its type constraint, as a
# sub's does and as any other parameter name does (#19150). The constraint
# does not leak to the caller's `$_` once the method returns.

plan 6;

class W {
    method m(Int $_ is copy) { $_ = "str"; "assigned" }
    method ok(Int $_ is copy) { $_ = 5; $_ }
    method inc(Int $_ is copy) { $_ + 1 }
    method other(Int $x is copy) { $x = "s" }
}

throws-like { W.new.m(1) }, X::TypeCheck::Assignment,
    'assigning a Str to a typed $_ is copy dies';
is W.new.ok(1), 5, 'assigning an Int to it works';
throws-like { W.new.other(1) }, X::TypeCheck::Assignment,
    'a typed $x is copy is enforced too';

sub s(Int $_ is copy) { $_ = "str" }
throws-like { s(1) }, X::TypeCheck::Assignment, 'and so is a sub\'s typed $_ is copy';

$_ = 3;
is W.new.inc(1), 2, 'the method sees its own $_';
$_ = "free after call";
is $_, "free after call", 'the caller\'s $_ is not constrained after the call';
