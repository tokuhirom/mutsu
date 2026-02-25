use Test;

plan 3;

my $c = class { };
is $c.WHAT.gist, "()", "anonymous class WHAT.gist does not leak internal name";
is $c.WHAT, "()", "anonymous class WHAT stringifies as anonymous type object";

my $obj = $c.new;
is $obj.WHAT.gist, "()", "instance WHAT.gist for anonymous class is anonymous";
