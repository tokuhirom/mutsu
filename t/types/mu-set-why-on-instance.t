use Test;

# Origin: Random::Names (ecosystem) calls `$str.set_why(...)` on plain
# instances and reads the pod back through `.WHY`.
plan 6;

my role Link { has $.LINK }

my $name = "foo";
$name.set_why("desc" but Link("https://example.com"));
isa-ok $name.WHY, Str, '.WHY after set_why on a Str is a Str';
is $name.WHY.LINK, "https://example.com", 'mixed-in attribute survives';
is $name.WHY.Str, "desc", 'string value survives';

class Foo {}
Foo.new.set_why("hi");
is Foo.new.WHY, "hi", 'set_why is type-wide for a user class instance';
is Foo.WHY, "hi", 'the type object sees it too';
isnt 42.WHY.gist, "hi", 'unrelated types are untouched';
