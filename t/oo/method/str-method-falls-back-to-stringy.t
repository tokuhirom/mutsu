use Test;

plan 13;

class OnlyStringy {
    has $.t;
    method Stringy { "S:" ~ $!t }
}

my $only-stringy = OnlyStringy.new(t => "q");
my $default-str = $only-stringy.Str;

# An explicit `.Str` call keeps Mu.Str's object representation when the class
# provides only Stringy. The exact address is runtime-dependent, so compare
# its stable class-name prefix and keep the Stringy result separate.
ok $default-str.starts-with("OnlyStringy"),
    '.Str keeps the default object representation';
nok $default-str eq $only-stringy.Stringy,
    '.Str does not fall back to Stringy';

# Both join forms implement the `.Str` method operation.
is ($only-stringy,).join(""), $default-str,
    'method join stringifies an element through .Str';
is join("", $only-stringy), $default-str,
    'routine join stringifies an element through .Str';

# `%s` also requests `.Str` explicitly.
is sprintf("%s", $only-stringy), $default-str,
    'sprintf %s stringifies through .Str';

# String context remains Stringy-first.
is ~$only-stringy, "S:q", 'prefix ~ still uses Stringy';
is "$only-stringy", "S:q", 'interpolation still uses Stringy';
is $only-stringy.Stringy, "S:q", 'Stringy remains independently callable';

class Both {
    method Str { "STR" }
    method Stringy { "STRINGY" }
}

my $both = Both.new;
is $both.Str, "STR", 'an explicit Str override wins';
is ($both,).join(""), "STR", 'join uses Str when both methods exist';

# The same distinction applies when an object is wrapped in an empty role
# mixin. A mixin must not reintroduce the reverse fallback.
my $mixed = OnlyStringy.new(t => "q") but role { };
nok $mixed.Str eq $mixed.Stringy,
    'a role mixin does not make Str fall back to Stringy';
is ($mixed,).join(""), $mixed.Str,
    'method join keeps the mixin Str semantics';
is sprintf("%s", $mixed), $mixed.Str,
    'sprintf %s keeps the mixin Str semantics';
