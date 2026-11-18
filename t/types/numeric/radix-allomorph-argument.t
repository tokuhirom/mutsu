use Test;

# `:N(...)` takes a Str, and an allomorph is one: it converts the allomorph's
# string, not its number (Compress::Zlib::Raw builds a test fixture with
# `<78 9c ...>.map({ :16($_) })`, where `<78>` is an IntStr).

plan 7;

is :16(<78>), 120, 'an IntStr converts its string';
is-deeply <78 9c>.map({ :16($_) }).List, (120, 156), 'word-list elements, IntStr and Str';
is :16(<1e2>), 482, 'a NumStr converts its string, not its value';
is :16(<10.5>), 16.3125, 'a RatStr too';
my $s = <ff>;
is :16($s), 255, 'a Str word';
is :2(<101> but role { }), 5, 'an allomorph with a role mixed in';
throws-like { :16(42) }, X::Numeric::Confused, 'an Int is still refused';
