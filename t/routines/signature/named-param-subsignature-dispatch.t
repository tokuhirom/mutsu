use Test;

# XML::Class traits: `:$x! (Str:D $name, :$over-ride!)` must only be applicable
# to a value that unpacks to it; a bare Str does not.
plan 8;

multi sub m(Str:D :$x!) { "str" }
multi sub m(:$x! (Str:D $name, :$over-ride!)) { "sub $name" }

is m(x => "plain"), "str", "a bare Str picks the Str candidate";
is m(x => ("item", :over-ride)), "sub item", "a list with a named part unpacks";

multi sub o(Str :$x) { "plain" }
multi sub o(:$x! (Str $a, $b?)) { "sub" }
is o(x => "abc"), "plain", "optional second part: bare Str is not unpacked";
is o(x => ("abc", "d")), "sub", "two positionals unpack";

sub one(:$x! (Str $a, $b?)) { "sub $a" }
throws-like { one(x => "abc") }, Exception, "bare Str does not unpack in a plain sub";
sub pos($x (Str $a, $b?)) { "sub $a" }
throws-like { pos("abc") }, Exception, "bare Str does not unpack a positional sub-signature";
throws-like { pos(5) }, Exception, "bare Int does not either";
is pos(("abc",)), "sub abc", "a one-element list does";
