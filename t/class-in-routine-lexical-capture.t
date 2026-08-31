use Test;

plan 12;

sub make-reader() {
    my $captured = 2;
    class Reader { method value() { $captured } }
    Reader.new;
}
is make-reader().value, 2, 'a class method captures its declaring routine lexical';

sub make-constrained-reader() {
    my $allowed = 2;
    class ConstrainedReader { method value($value where $allowed) { $value } }
    ConstrainedReader.new;
}
lives-ok { is make-constrained-reader().value(2), 2 },
    'a class method where constraint captures its declaring routine lexical';

my $captured = 99;
is make-reader().value, 2,
    'a returned class method capture overrides a same-named caller lexical';

sub read-after-write() {
    my $value = 1;
    class LiveReader { method value() { $value } }
    $value = 3;
    LiveReader.new.value;
}
is read-after-write(), 3,
    'a class method shares its lexical while the declaring routine is still active';

# --- the ROLE twin -------------------------------------------------------
# A role declared in a routine has the same problem, and the class-side pass
# cannot reach it: role methods are stored on the `RoleDef`, and the class pass
# deliberately skips `role_origin.is_some()` candidates (a composed copy closes
# over the ROLE's declaration site, not the composing class's). The capture is
# recorded at role registration instead, so `compose_role_into_class`'s
# `md.clone()` carries it into every composing class.

sub make-role-reader() {
    my $captured = 4;
    role RoleReader { method value() { $captured } }
    class ComposedReader does RoleReader { }
    ComposedReader.new;
}
is make-role-reader().value, 4,
    'a composed role method captures its declaring routine lexical';

sub role-read-after-write() {
    my $value = 6;
    role LiveRoleReader { method value() { $value } }
    class LiveComposed does LiveRoleReader { }
    my $o = LiveComposed.new;
    $value = 7;
    $o.value;
}
is role-read-after-write(), 7,
    'a composed role method shares the lexical while the routine is still active';

sub make-parameterized-role-reader() {
    my $captured = 8;
    role ParamReader[::T] { method value() { $captured } }
    class ParamComposed does ParamReader[Int] { }
    ParamComposed.new;
}
is make-parameterized-role-reader().value, 8,
    'a PARAMETERIZED role method keeps its capture through type substitution';

sub make-punned-role-reader() {
    my $captured = 9;
    role PunnedReader { method value() { $captured } }
    PunnedReader.new;
}
is make-punned-role-reader().value, 9,
    'a punned role method captures its declaring routine lexical too';

my $captured-outer = 99;
sub shadowing-role-reader() {
    my $captured-outer = 'inner';
    role ShadowReader { method value() { $captured-outer } }
    class ShadowComposed does ShadowReader { }
    ShadowComposed.new;
}
is shadowing-role-reader().value, 'inner',
    'the routine lexical wins over a same-named caller lexical';

# A role that COMPOSES another role must not re-capture: the composed copy
# closes over the SOURCE role's declaration site, which that role recorded
# itself. Re-capturing at the composing role bound its own enclosing lexicals
# over the source role's parameters — `role A [:$a = 1, :$b = $a * 2]` composed
# into `role B does A[:a(1)]` read a file-scope `my $a` instead
# (roast/S14-roles/parameterized-mixin.t 27-28).
my $a = 0;
my $b = 0;
{
    role ParamDefaults[:$a = 1, :$b = $a * 2] { method pair { $a ~ "-" ~ $b } }
    role ComposesOne does ParamDefaults[:a(1)] { }
    role ComposesTwo does ParamDefaults[:a(2)] { }
    is ComposesOne.new.pair, '1-2',
        'a role composed into a role keeps the source role parameters';
    is ComposesTwo.new.pair, '2-4',
        'and a second composition keeps its own';
}
