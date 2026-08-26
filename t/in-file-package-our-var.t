use Test;

# An in-file `module M { our $x }` body compiles INLINE into the enclosing
# frame, and mutsu allocates one local slot per NAME per compiled code object
# (`Compiler::declare_local` is get-or-create; the shadow-slot campaign is what
# would change that). So the package block's `our $x` and a mainline `my $x`
# land on ONE slot -- and `our_locals`, a slot-keyed map recorded at compile
# time, then linked the mainline lexical to `$M::x`.
#
# Three chokepoints leaked through that shared slot:
#   * `sync_our_package_var_from_local` pushed a mainline `my` write out to
#     `$M::x` (both sigils);
#   * `sync_our_local_from_qualified` pulled an external `@M::l = ...` write
#     back onto the mainline lexical;
#   * the package-block exit re-exported the block's `our` alias over a
#     same-named outer lexical.
#
# The fixes are lexical, not name-based: a DECLARATION never reverse-syncs, and
# an `our` link is only honoured while its owning package is in scope.

plan 38;

# --- the core collision: mainline `my` AFTER the package block --------------
module M {
    our $x = 'our';
    our @y = 'oury';
    our %h = (k => 'ourh');
}
my $x = 'top';
my @y = 'topy';
my %h = (k => 'toph');

is $M::x, 'our', 'in-file module keeps its our $x against a mainline my $x';
is @M::y.join(','), 'oury', 'in-file module keeps its our @y';
is %M::h<k>, 'ourh', 'in-file module keeps its our %h';
is $x, 'top', 'mainline my $x is its own variable';
is @y.join(','), 'topy', 'mainline my @y is its own variable';
is %h<k>, 'toph', 'mainline my %h is its own variable';

# A plain assignment to the mainline lexical afterwards must not reach the
# package either -- the declaration is not the only write chokepoint.
$x = 'top2';
@y = 'topy2';
%h<k> = 'toph2';
is $M::x, 'our', 'later assignment to the mainline my $x leaves $M::x alone';
is @M::y.join(','), 'oury', 'later assignment to the mainline my @y leaves @M::y alone';
is %M::h<k>, 'ourh', 'later assignment to the mainline my %h leaves %M::h alone';
is $x, 'top2', 'the mainline my $x took the assignment';

# --- mainline `my` BEFORE the package block --------------------------------
my $b = 'bt';
my @bl = 'btl';
module BeforeMod {
    our $b = 'bo';
    our @bl = 'bol';
}
is $BeforeMod::b, 'bo', 'our $x declared after a same-named mainline my';
is @BeforeMod::bl.join(','), 'bol', 'our @y declared after a same-named mainline my';
is $b, 'bt', 'the earlier mainline my $x survives the package block';
is @bl.join(','), 'btl', 'the earlier mainline my @y survives the package block';

# --- writing the package variable from outside, after the block closed ------
$M::x = 'ext';
@M::y = 'exty';
is $M::x, 'ext', 'external write to $M::x lands on the package variable';
is @M::y.join(','), 'exty', 'external write to @M::y lands on the package variable';
is $x, 'top2', 'external write to $M::x does not reach the mainline my $x';
is @y.join(','), 'topy2', 'external write to @M::y does not reach the mainline my @y';

# --- class and package declarators, not just `module` -----------------------
class KCls { our $c = 'k'; }
package PPkg { our $c = 'pk'; }
my $c = 'mainc';
is $KCls::c, 'k', 'class body our $c survives a mainline my $c';
is $PPkg::c, 'pk', 'package body our $c survives a mainline my $c';
is $c, 'mainc', 'the mainline my $c is untouched by class/package our';

# --- a nested in-file package ----------------------------------------------
module Outer {
    module Inner { our $z = 'inner'; }
    our $z = 'outer';
}
my $z = 'mainz';
is $Outer::Inner::z, 'inner', 'nested in-file package keeps its own our $z';
is $Outer::z, 'outer', 'the enclosing package keeps its own our $z';
is $z, 'mainz', 'the mainline my $z is neither of them';

# --- two in-file modules owning the same bare name --------------------------
module TwinA { our $n = 'A'; our @l = 'al'; }
module TwinB { our $n = 'B'; our @l = 'bl'; }
my $n = 'main';
my @l = 'mainl';
is "$TwinA::n$TwinB::n$n", 'ABmain', 'two modules keep separate our $n';
is @TwinA::l.join(','), 'al', 'two modules keep separate our @l (first)';
is @TwinB::l.join(','), 'bl', 'two modules keep separate our @l (second)';
is @l.join(','), 'mainl', 'the mainline my @l is neither of them';

# --- a routine inside the block reading/writing its own our $x --------------
# The shape the neighbouring bare-name resolution fix cares about: it must not
# regress, and it must not be confused by the mainline lexical of the same name.
module Rout {
    our $s = 'r';
    our sub bump() { $s ~= '!' }
    our sub peek() { $s }
}
my $s = 'lex';
is Rout::peek(), 'r', 'a module routine reads its own our $s';
Rout::bump();
is Rout::peek(), 'r!', 'a module routine writes its own our $s';
is $Rout::s, 'r!', 'the package-qualified name sees the routine write';
is $s, 'lex', 'the mainline my $s is untouched by the module routine';

# --- assignment to the our variable from inside its own package block -------
module Inside {
    our $q = 'd';
    $q = 'd2';
}
is $Inside::q, 'd2', 'an assignment inside the block still reaches the package var';

# --- mainline-scope `our`, $GLOBAL::, dynamics, topic, plain globals --------
our $g = 'G';
is "$g $GLOBAL::g", 'G G', 'a mainline our $g and $GLOBAL::g are one variable';
$GLOBAL::g = 'G2';
is "$g $GLOBAL::g", 'G2 G2', 'a $GLOBAL:: write is seen through the lexical alias';

my $*dyn = 'dyn';
is $*dyn, 'dyn', 'dynamic variables are unaffected';
for 'topic' { is $_, 'topic', 'the topic is unaffected' }
$PlainPkg::glob = 'pg';
is $PlainPkg::glob, 'pg', 'a plain package global is unaffected';
