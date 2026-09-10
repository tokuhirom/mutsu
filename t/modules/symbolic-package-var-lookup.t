use Test;

# A sigil variable can name a package variable through a leading `::`, either
# statically (`@::Pkg::name`) or symbolically (`@::Pkg::('name')`, where the last
# segment is a runtime string). The leading `::` is optional, so `@::Pkg::x`
# names the same variable as `@Pkg::x`.

plan 9;

my package R {
    our @arr = 1, 2, 3;
    our %map = a => 10, b => 20;
}

# --- arrays ---
is-deeply @R::arr,          [1, 2, 3], 'plain qualified @R::arr';
is-deeply @::R::arr,        [1, 2, 3], 'leading-:: static @::R::arr';
is-deeply @::R::('arr'),    [1, 2, 3], 'symbolic @::R::(\'arr\')';
is @::R::('arr')[1],        2,         'indexing a symbolic array lookup';
{
    my $which = 'arr';
    is-deeply @::R::("$which"), [1, 2, 3], 'symbolic lookup from a runtime string';
}

# --- hashes ---
is-deeply %R::map,        {a => 10, b => 20}, 'plain qualified %R::map';
is-deeply %::R::map,      {a => 10, b => 20}, 'leading-:: static %::R::map';
is-deeply %::R::('map'),  {a => 10, b => 20}, 'symbolic %::R::(\'map\')';
is %::R::('map')<b>,      20,                 'indexing a symbolic hash lookup';
