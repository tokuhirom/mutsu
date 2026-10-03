use Test;

# `Routine.prec` answers an operator's precedence hash (#11324). The expected
# hashes are Rakudo 2026.09's.

plan 19;

sub p(&r) { &r.prec.sort.map({ .key ~ '=' ~ .value }).join(' ') }

# Built-in operators.
is p(&infix:<+>), 'assoc=left dba=additive prec=t=', 'infix:<+>';
is p(&infix:<~>), 'assoc=left dba=concatenation prec=r=', 'infix:<~>';
is p(&infix:<==>), 'assoc=chain dba=chaining diffy=1 iffy=1 prec=m=', 'infix:<==> carries its flags';
is p(&infix:<&&>), 'assoc=left dba=tight-and iffy=1 prec=l= thunky=.t', 'infix:<&&>';
is p(&infix:«<=»), 'assoc=chain dba=chaining diffy=1 iffy=1 prec=m=', 'infix:«<=»';
is p(&prefix:<->), 'assoc=unary dba=symbolic-unary prec=v=', 'prefix:<->';
is p(&postfix:<++>), 'assoc=unary dba=autoincrement prec=x=', 'postfix:<++>';
isa-ok &infix:<==>.prec<iffy>, Int, 'a flag is the integer 1';
isa-ok &infix:<+>.prec, Hash, '.prec is a Hash';

# User operators: the category default, then the traits.
my sub infix:<plain>($a, $b) { }
my sub infix:<cat>(**@a) is assoc<list> is equiv(&[~]) { @a.elems }
my sub infix:<tight>($a, $b) is tighter(&infix:<cat>) { }
my sub infix:<tighter>($a, $b) is tighter(&[tight]) { }
my sub infix:<loose>($a, $b) is looser(&[==]) { }
my sub infix:<angle>($a, $b) is tighter<+> { }
my sub infix:<last>($a, $b) is tighter(&[+]) is looser(&[*]) { }
my sub prefix:<neg>($a) { }
is p(&infix:<plain>), 'assoc=left dba=default-infix prec=t=', 'default infix';
is p(&infix:<cat>), 'assoc=list dba=concatenation prec=r=', 'is equiv copies, is assoc overrides';
is p(&infix:<tight>), 'assoc=left dba=concatenation prec=r@=',
    'is tighter inserts @ and resets assoc to left';
is p(&infix:<tighter>), 'assoc=left dba=concatenation prec=r@@=', 'relative to a user operator';
is p(&infix:<loose>), 'assoc=left dba=chaining diffy=1 iffy=1 prec=m:=', 'is looser inserts :';
is p(&infix:<angle>), 'assoc=left dba=additive prec=t@=', 'is tighter<+>';
is p(&infix:<last>), 'assoc=left dba=multiplicative prec=u:=', 'the last precedence trait wins';
is p(&prefix:<neg>), 'assoc=unary dba=default-prefix prec=v=', 'default prefix';

# A multi candidate shares the precedence its operator was declared with.
my multi sub infix:<mm>($a, $b) is tighter(&[+]) { }
my multi sub infix:<mm>(Int $a, $b) { }
is p(&infix:<mm>), 'assoc=left dba=additive prec=t@=', 'multi operator';

# A routine that is not an operator has an empty hash.
sub foo { }
is &foo.prec.elems, 0, 'plain sub';
