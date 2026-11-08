use Test;

plan 6;

# A `$*x` read before a `my $*x` declared in a NESTED block is an ordinary
# not-found dynamic lookup (a Failure), not Nil (#17113).
sub a2 { my $r = $*M.^name; if True { my $*M = "inner"; return ($r, $*M) } }
is-deeply a2(), ("Failure", "inner"), 'read before an if-block declaration';

sub a4 { my $i := $*P; my $r = $i.^name; if True { my $*P = "inner" }; $r }
is a4(), "Failure", 'bound read before an if-block declaration';

sub a6 { my $r = $*Q.^name; { my $*Q = "x" }; $r }
is a6(), "Failure", 'read before a bare-block declaration';

# The dynamic is still visible where declared.
sub a7 { { my $*R = "in"; return $*R } }
is a7(), "in", 'the nested declaration is readable in its block';

# A caller's declaration still wins.
sub a8 { my $r = $*S; if True { my $*S = "inner" }; $r }
{ my $*S = "outer"; is a8(), "outer", "a caller's binding is found"; }

# Same-scope post-declaration stays a compile-time error.
throws-like { EVAL 'sub p { say $*T; my $*T = 1 }; p()' }, X::Dynamic::Postdeclaration,
    'same-scope post-declaration still dies';
