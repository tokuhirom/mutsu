use Test;

# ADR-0064: `.VAR` hands back the CONTAINER, and a container is transparent
# for ordinary method dispatch -- every method that is not a property of the
# container itself is answered by the value the container holds.

plan 62;

my @real = 1, [3, 4];
my %h    = a => [1, 2];

# --- an Array/Hash element's container ------------------------------------
is @real[1].VAR.^name,  'Scalar',   'element .VAR is a Scalar container';
is @real[1].VAR.raku,   '[3, 4]',   '.raku shows the CONTAINED value (deconted)';
is @real[1].VAR.gist,   '$[3, 4]',  '.gist shows the CONTAINER (itemized)';
is @real[1].VAR.elems,  2,          '.elems delegates to the contained value';
is @real[1].VAR[0].raku,'$[3, 4]',  'a Scalar is not Positional: [0] is the one item';
is @real[0].VAR.raku,   '1',        'a plain element delegates too';
is %h<a>.VAR.raku,      '[1, 2]',   'a hash element behaves the same';
is @real[1].VAR.name,   '@real',    'the container is named after the variable';
ok @real[1].VAR.defined,            'a container object is always defined';

# --- a variable's own container -------------------------------------------
my $z = [3, 4];
is $z.VAR.^name, 'Scalar',  '$-variable .VAR is a Scalar container';
is $z.VAR.raku,  '[3, 4]',  '$-variable .VAR.raku is the contained value';
is $z.VAR.gist,  '$[3, 4]', '$-variable .VAR.gist is the container';
is $z.VAR.elems, 2,         '$-variable .VAR.elems delegates';
is $z.VAR.name,  '$z',      '$-variable .VAR.name is the variable';

my $n = 42;
is $n.VAR.raku,     '42', 'a plain Int container delegates .raku';
is $n.VAR.gist,     '42', 'a plain Int container delegates .gist';
is $n.VAR[0].raku,  '42', 'a plain Int container is one item';

my $u;
is  $u.VAR.raku, 'Any', 'an undefined container still reports its value';
ok  $u.VAR.defined,     'the container is defined even when the value is not';
nok $u.VAR.Bool,        '.Bool comes from the contained value';

# An `@`/`%` variable's `.VAR` IS the container, so it delegates everything.
is @real.VAR.^name, 'Array',        '@-variable .VAR is the Array itself';
is @real.VAR.raku,  '[1, [3, 4]]',  '@-variable .VAR.raku is the Array raku';
is @real.VAR.gist,  '[1 [3 4]]',    '@-variable .VAR.gist is the Array gist';
is @real.VAR.elems, 2,              '@-variable .VAR.elems is the Array elems';
is %h.VAR.^name,    'Hash',         '%-variable .VAR is the Hash itself';

# The descriptor tracks later assignments (it is a container, not a snapshot).
my $m = 1;
$m.VAR;
$m = 2;
is $m.VAR.raku, '2', 'the container reports the CURRENT value';

# --- `is default` / `of` reach the element's container --------------------
my @nat is default(0) = 1, 2;
is @nat[0].VAR.default, 0, 'an element inherits the container is default';
my %hd is default(9) = a => 1;
is %hd<a>.VAR.default, 9, 'a hash element inherits is default';
my Int @ti = 1, 2;
is @ti[0].VAR.default.gist, '(Int)', 'a typed array element defaults to its type';
is @ti[0].VAR.of.gist,      '(Int)', 'a typed array element .of is the element type';
my @plain = 1, 2;
is @plain[0].VAR.default.gist, '(Any)', 'an untyped element defaults to Any';
is @plain[0].VAR.of.gist,      '(Mu)',  'an untyped element .of is Mu';

# --- multi-dimensional subscripts take the same path ----------------------
my @sh[2;2];
@sh[0;0] = 7;
is @sh[0;0].VAR.^name, 'Scalar',  'a shaped element is a Scalar container too';
is @sh[0;0].VAR.raku,  '7',       'a shaped element delegates .raku';
is @sh[0;0].VAR.name,  'element', "a shaped array's element containers are anonymous";

# --- a SLICE hands back a List of containers, and .VAR on a List is identity
my @a = 1, 2, 3;
my @i = 0, 1;
is @a[0,1].VAR.^name,  'List', 'a comma slice .VAR is the List';
is @a[0..1].VAR.^name, 'List', 'a range slice .VAR is the List';
is @a[*].VAR.^name,    'List', 'a whatever slice .VAR is the List';
is @a[@i].VAR.^name,   'List', 'a variable slice .VAR is the List';
is %h<a a>.VAR.^name,  'List', 'a hash slice .VAR is the List';
is @a[0].VAR.^name,    'Scalar', 'a single subscript is still an element';
my @e = (1, 2), 3;
is @e[0].VAR.^name,    'Scalar', 'an element that HOLDS a list is still an element';

# --- a subscript whose parent container has NO NAME ------------------------
# raku answers `.VAR` from the parent regardless of whether it has a name, and
# an anonymous element container is called `element`.
my %d; %d<a><b> = 1;
is %d<a><b>.VAR.^name,   'Scalar',  'a chained hash subscript is a Scalar container';
is %d<a><b>.VAR.name,    'element', 'an anonymous element container is named "element"';
is %d<a><b>.VAR.raku,    '1',       'a chained subscript delegates .raku';
is %d<a><b>.VAR.default.gist, '(Any)', 'an anonymous container defaults to Any';
is %d<a><b>.VAR.of.gist, '(Mu)',    'an anonymous container .of is Mu';
nok %d<a><b>.VAR.dynamic,           'an anonymous container is not dynamic';
my @g; @g[0][1] = 2;
is @g[0][1].VAR.^name,   'Scalar',  'a chained array subscript is a Scalar container';
is @g[0][1].VAR.name,    'element', 'and its container is anonymous too';

# ...but only when the parent really is a container. A `List` parent stores
# values, not containers, so `.VAR` on its element is the element.
my @ar2 = 1, [2, 3];
is @ar2[1][0].VAR.^name, 'Scalar', 'an Array inside an Array has Scalar elements';
my @arr = 1, (2, 3);
is @arr[1][0].VAR.^name, 'Int',    'a List inside an Array has bare elements';
is [1, 2][0].VAR.^name,  'Scalar', 'an array literal has Scalar elements';
is (1, 2)[0].VAR.^name,  'Int',    'a list literal has bare elements';
is %h<a a>.VAR.^name,    'List',   'a slice of a named container is still a List';

# The parent expression is evaluated exactly once, even when it has side
# effects — `Dup` is what buys that.
my $calls = 0;
sub bump() { $calls++; my @x = 1, 2; @x }
is bump()[0].VAR.^name, 'Scalar', 'a call result subscript is a Scalar container';
is $calls, 1, 'the parent expression ran exactly once';

# --- a native array's elements are positional refs, not Scalars -----------
my int  @ni = 1, 2;
my num  @nn = 1e0, 2e0;
my str  @ns = "a", "b";
my uint @nu = 1, 2;
is @ni[0].VAR.^name, 'IntPosRef',  'a native int element is an IntPosRef';
is @nn[0].VAR.^name, 'NumPosRef',  'a native num element is a NumPosRef';
is @ns[0].VAR.^name, 'StrPosRef',  'a native str element is a StrPosRef';
is @nu[0].VAR.^name, 'UIntPosRef', 'a native uint element is a UIntPosRef';
dies-ok { @ni[0].VAR.of }, 'a positional ref has none of Scalar container properties';
