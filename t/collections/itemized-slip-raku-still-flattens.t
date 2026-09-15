use Test;

# A `Slip` held in a `$` scalar container renders the container in `.raku`
# (`my $x = slip(5, 6); $x.raku` is `$(slip(5, 6))`), exactly as an itemized
# Array/Hash/List/Seq does — but unlike every one of those, it MUST keep
# flattening: a Slip is the value that splices itself into the surrounding
# list even out of an item container.
#
# So the itemization cannot be the generic `Value::scalar` wrapper (that
# wrapper IS the "stop flattening" marker); it is a second nanbox kind tag
# over the same element `Arc`, which `ValueView::Slip` hides. Every assertion
# below pairs a rendering check with the flattening check that the rendering
# must not have cost.

plan 27;

# --- `.raku` renders the `$` container for a scalar-stored Slip ---
my $x = slip(5, 6);
is $x.raku,                 '$(slip(5, 6))',  'scalar-stored Slip .raku shows the $ container';
is slip(5, 6).raku,         'slip(5, 6)',     'a bare Slip .raku shows no container';
is slip(3).raku,            'slip(3,)',       'one-element bare Slip keeps its trailing comma';
my $one = slip(3);
is $one.raku,               '$(slip(3,))',    'one-element itemized Slip keeps both markers';

# --- ... and the itemized Slip still flattens everywhere ---
is (1, $x, 2).elems,        4,                'a $-held Slip still flattens into a list';
is (1, $x, 2).raku,         '(1, 5, 6, 2)',   'a $-held Slip splices into a list literal';
my @a = 1, $x, 2;
is @a.elems,                4,                'a $-held Slip still splices into an array assign';
is @a.raku,                 '[1, 5, 6, 2]',   'the spliced elements are the Slip contents';
is $x.elems,                2,                'the itemized Slip is still a 2-element Slip';
is $x.^name,                'Slip',           'itemization does not change the type';

# --- `.item` / `$( )` itemize a bare Slip, and must not stop it flattening ---
is slip(5, 6).item.raku,    '$(slip(5, 6))',  '.item on a Slip renders the $ container';
is $(slip(5, 6)).raku,      '$(slip(5, 6))',  '$( ) on a Slip renders the $ container';
is item(slip(5, 6)).raku,   '$(slip(5, 6))',  'the item() routine agrees with the method';
is (1, slip(5, 6).item, 2).elems,    4,       '.item does not stop a Slip flattening';
is (1, $(slip(5, 6)), 2).elems,      4,       '$( ) does not stop a Slip flattening';
is (1, item(slip(5, 6)), 2).elems,   4,       'item() does not stop a Slip flattening';
my @b = 1, slip(5, 6).item, 2;
is @b.raku,                 '[1, 5, 6, 2]',   'an .item-ed Slip still splices into an array';
is slip(5, 6).item.^name,   'Slip',           '.item leaves the value a Slip';

# --- the empty Slip is `Empty`, container or not ---
is slip().raku,             'Empty',          'the empty Slip renders as Empty';
my $e = slip();
is $e.raku,                 'Empty',          'a scalar-stored empty Slip is still just Empty';

# --- `.gist` / `.Str` never show the container ---
is $x.gist,                 '(5 6)',          'an itemized Slip .gist drops the container';
is slip(5, 6).gist,         '(5 6)',          'a bare Slip gists the same way';
is $x.Str,                  '5 6',            'an itemized Slip .Str is unaffected';

# --- decontainerizing routes drop the itemization again ---
is $x<>.raku,               'slip(5, 6)',     '$x<> decontainerizes an itemized Slip';
is $x.Slip.raku,            'slip(5, 6)',     '.Slip hands out the value, not the container';
my $again = $x.Slip;
is $again.raku,             '$(slip(5, 6))',  'storing it back into a $ itemizes it again';

# --- a `:=` bind installs the value, not a Scalar container ---
my $bound := slip(5, 6);
is $bound.raku,             'slip(5, 6)',     'a :=-bound Slip shows no container';

done-testing;
