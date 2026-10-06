use Test;

# A mixin type object renamed with `.^set_name` (the `^parameterize` idiom:
# `my $w := c.^mixin(Role[t]); $w.^set_name("Box[{t.^name}]"); $w`) names the
# instances built from it too: in Rakudo an instance's type IS that type
# object. Regression (#11805): the type object and `.^name` of an instance
# answered the new name, but the `.gist`/`.raku` of an instance still rendered
# the synthesized composition (`Box+{BoxOf[Int]}.new`).

plan 17;

role BoxOf[::T] { method of { T } }
class Box {
    method ^parameterize(Mu \c, Mu \t) {
        my $w := c.^mixin(BoxOf[t]);
        $w.^set_name("Box[{t.^name}]");
        $w
    }
}

# --- the type object and the instance agree ------------------------------------
my $obj = Box[Int].new;
is Box[Int].^name,     'Box[Int]', 'the type object reports the new name';
is $obj.^name,         'Box[Int]', "an instance's .^name reports it";
is $obj.WHAT.^name,    'Box[Int]', "an instance's .WHAT.^name reports it";
is $obj.HOW.name($obj), 'Box[Int]', "HOW.name on an instance reports it";
is $obj.of.^name,      'Int',      'the composed role still works';
ok $obj.WHAT === Box[Int], "an instance's type is the renamed type object";
ok $obj ~~ Box[Int],       'an instance smartmatches its type';

# --- .gist / .raku of an instance -----------------------------------------------
is $obj.gist, 'Box[Int].new', "an instance's .gist renders the new name";
is $obj.raku, 'Box[Int].new', "an instance's .raku renders the new name";
is "$obj.WHAT.gist()", '(Box[Int])', "the type object's .gist renders it";

# --- each parameterization keeps its own name -----------------------------------
my $str = Box[Str].new;
is $str.^name, 'Box[Str]', 'another parameterization has its own name';
is $str.raku,  'Box[Str].new', 'and renders by it';
is $obj.^name, 'Box[Int]', 'the first one is unaffected';

# --- an unrenamed mixin keeps the synthesized composition name -------------------
role Plain { }
class Base { has $.x }
my $plain = Base.new(x => 1) but Plain;
is $plain.^name, 'Base+{Plain}', 'an unrenamed mixin keeps its composed name';
is $plain.raku,  'Base+{Plain}.new(x => 1)', 'an unrenamed instance renders the composed name';

# --- a rename made on the instance side reaches the composition ------------------
role Tag { }
my $tagged = Base.new(x => 2) but Tag;
$tagged.^set_name('Tagged');
is $tagged.^name, 'Tagged', 'an instance-side .^set_name is reported by .^name';
is $tagged.raku,  'Tagged.new(x => 2)', 'and by .raku';
