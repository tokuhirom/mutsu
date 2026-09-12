use Test;

# A `subset`'s `where` predicate closes over its DECLARATION scope, not
# whatever package happens to be dynamically current when the predicate
# first runs. A subset declared inside a class body, whose predicate refers
# to a sibling type (here a `grammar`) declared in that same class body,
# must still resolve that sibling when the predicate runs as part of a
# `multi method new` candidate's signature check during dispatch -- i.e.
# BEFORE any method body of the class has been entered and pushed the class
# onto the method-class stack. Previously the predicate's sibling reference
# was only visible when it happened to run from inside one of the class's
# own methods; checked during constructor candidate matching it hit
# "Undeclared name", every positional `new` candidate silently failed its
# constraint, and dispatch fell through to the default (named-arguments-only)
# constructor -- exactly the `Net::Netmask` failure in #8003.
plan 2;

{
    class C {
        grammar IP_Addr {
            token TOP { ^ <ipv4> $ }
            token ipv4 { <d8> ** 4 % '.' }
            token d8 { \d+ }
        }
        our subset IPv4 of Str where { IP_Addr.subparse($_, :rule<ipv4>) };

        has $.ip;
        multi method new(IPv4 $ip) { self.bless(:$ip) }
    }
    is C.new("10.0.0.1").ip, "10.0.0.1",
        'multi method new matches a class-scoped subset whose where predicate calls a sibling grammar';
}

# The subset's own smartmatch (outside any dispatch) must keep working too.
{
    class D {
        grammar Only5 {
            token TOP { ^ '5' $ }
        }
        our subset Five of Str where { Only5.subparse($_) };
        method check($s) { so ($s ~~ Five) }
    }
    ok D.new.check("5"), 'subset predicate referencing a sibling class-scoped grammar still smartmatches';
}
