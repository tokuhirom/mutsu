use v6;
use lib 't/lib';
use Test;
use AnonDoesRole;

plan 5;

# An anonymous class (`class :: does R`) that composes a role imported by its
# short alias (`Searchable`, registered as `AnonDoesRole::Searchable`) must
# resolve the alias to the qualified role — just like a named class does. The
# body `DoesDecl` emitted for the anonymous class previously looked the role up
# by its short name only and failed with "Unknown role".

# top-level anonymous class composing an imported role
{
    my $c = class :: does Searchable { method search(*@) { "TOP" } };
    is $c.search("x"), "TOP", "top-level anon class does imported role";
}

# anonymous class declared inside a sub body
{
    sub make { class :: does Searchable { method search(*@) { "SUB" } } }
    is make().search("x"), "SUB", "anon class does imported role inside sub body";
}

# anonymous class declared inside a method body
{
    class Holder {
        method plugin { class :: does Searchable { method search(*@) { "METHOD" } } }
    }
    is Holder.new.plugin.search("x"), "METHOD",
        "anon class does imported role inside method body";
}

# `(BaseClass but role :: {...})` whose role method body declares an anon class
# doing an imported role — the zef recommendation-manager mock pattern.
{
    my $obj = (Repo but role :: {
        method plugins(*@) {
            [ class :: does Searchable { method search(*@) { "MIXED" } }, ]
        }
    }).new;
    my @p = $obj.plugins;
    is @p.elems, 1, "but-role method returns the plugin list";
    is @p[0].search("x"), "MIXED", "anon class inside but-role method does imported role";
}
