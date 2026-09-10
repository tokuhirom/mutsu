use v6;
use Test;

# Raku's BUILDALL runs one build step per MRO class: a custom BUILD replaces
# the default named-arg → attribute auto-assignment only for the attributes of
# the class that declares it. A parent class without its own BUILD still
# auto-assigns — and its `is required` attributes are satisfied by named args.
# X::DBDish::DBError::Pg (`has $.sqlstate is required` computed in BUILD, with
# required parent attrs passed as named args) is this exact shape.
plan 5;

class Base { has $.msg is required; has $.extra = 'dflt'; }
class Kid is Base {
    has $.state is required;
    submethod BUILD(:$x) { $!state = $x * 2 }
}

my $k = Kid.new(msg => 'm', x => 21);
is $k.state, 42, "the child's BUILD computed its own attribute";
is $k.msg, 'm', "the parent's attribute auto-assigned from the named arg";
is $k.extra, 'dflt', "the parent's defaulted attribute kept its default";

# The child's required attribute must still be enforced post-BUILD.
class Kid2 is Base {
    has $.state is required;
    submethod BUILD() { }
}
dies-ok { Kid2.new(msg => 'm') },
    "a required attribute the BUILD leaves unset still dies";

# A custom BUILD still takes over ITS OWN class's named-arg mapping.
class P { has $.x = 42; submethod BUILD() { } }
is P.new(:666x).x, 42,
    "a named arg for a BUILD-owning class's attribute is not auto-assigned";

done-testing;
