use Test;

# A grammar's cursor IS an instance of the grammar (#9803). A method a rule
# calls as a subrule (`<.acc>`) writes the attributes of the cursor of the rule
# invocation making the call, and when that rule returns the cursor is its
# Match. Every expectation below was measured against raku.

plan 23;

# The repro from the issue.
{
    grammar G {
        has $.inv;
        token TOP { <t> }
        token t { a <.acc> }
        method acc { $!inv = True; self }
    }
    my $m = G.parse("a");
    is-deeply $m<t>.inv, True, 'an attribute written in a method a token calls is on that token\'s Match';
    ok !$m.inv.defined, 'the start rule\'s own cursor was never written';
    is $m.inv.^name, 'Any', 'an unset declared attribute reads as its type object, not Nil';
}

# Every rule invocation owns its cursor, so two matches of one token differ,
# and each starts from the uninitialised attribute (`$!n++` gives 1, not 2).
{
    grammar K {
        has $.n;
        token TOP { <t> <t> }
        token t { a <.bump> }
        method bump { $!n++; self }
    }
    my $m = K.parse("aa");
    is $m<t>[0].n, 1, 'the first invocation counted once';
    is $m<t>[1].n, 1, 'the second invocation started over';
    ok !$m.n.defined, 'the caller\'s cursor saw neither';
}

# Calls in one rule invocation share its cursor.
{
    grammar L {
        has $.cnt;
        token TOP { a <.bump> b <.bump> c <.bump> }
        method bump { $!cnt++; self }
    }
    is L.parse("abc").cnt, 3, 'three calls from the start rule accumulate on its cursor';
}

# A declared attribute no method wrote reads as its uninitialised value, and a
# `= default` is not applied: a cursor is created, not built.
{
    grammar U {
        has Int $.i;
        has Str $.s;
        has @.l;
        has %.h;
        has $.plain = 42;
        token TOP { <t> }
        token t { a }
    }
    my $m = U.parse("a");
    is $m.i.^name, 'Int', 'a typed scalar reads as its type object';
    is $m.s.^name, 'Str', 'another typed scalar';
    is-deeply $m.l, [], 'an @ attribute reads as an empty array';
    is-deeply $m.h, {}, 'a % attribute reads as an empty hash';
    is $m<t>.plain.^name, 'Any', 'a default initializer is not applied to a cursor';
}

# Attributes of every sigil written by one method.
{
    grammar F {
        has Int $.i;
        has Str $.s;
        has @.l;
        has %.h;
        token TOP { <t> }
        token t { a <.fill> }
        method fill { $!i = 7; $!s = 'x'; @!l.push(1, 2); %!h<k> = 'v'; self }
    }
    my $t = F.parse("a")<t>;
    is $t.i, 7, 'a scalar';
    is $t.s, 'x', 'a string';
    is-deeply $t.l, [1, 2], 'an array';
    is-deeply $t.h, {:k<v>}, 'a hash';
}

# Only the branch that matched leaves its cursor on the Match.
{
    grammar B {
        has $.who;
        token TOP { <t> }
        token t { [ a <.one> x | a <.two> b ] }
        method one { $!who = 'one'; self }
        method two { $!who = 'two'; self }
    }
    is B.parse("ab")<t>.who, 'two', 'the winning branch\'s call is the one the Match carries';
}

# Proto candidates each own one.
{
    grammar P {
        has $.kind;
        token TOP { <item>+ }
        proto token item {*}
        token item:sym<a> { a <.ka> }
        token item:sym<b> { b <.kb> }
        method ka { $!kind = 'A'; self }
        method kb { $!kind = 'B'; self }
    }
    is-deeply P.parse("abba")<item>.map(*.kind).List, ('A', 'B', 'B', 'A'),
        'each proto candidate\'s Match carries its own cursor';
}

# The start rule can be chosen, and a subparse works the same way.
{
    grammar R {
        has $.v;
        token TOP { <t> }
        token t { a <.acc> }
        method acc { $!v = 'set'; self }
    }
    is R.parse("a", :rule<t>).v, 'set', ':rule makes the chosen rule the root cursor';
    is R.subparse("ab")<t>.v, 'set', 'a subparse carries it too';
}

# The documented example (Language/grammars.rakudoc, "Attributes in grammars"):
# `invalid` is local to each component. `field` is a rule the compiled engine
# hands to the walk (its `<-crlf>` class), so this also covers that path.
{
    grammar HTTPRequest {
        has Bool $.invalid;

        token TOP {
            <type> <.ns> <path> <.ns> 'HTTP/1.1' <.crlf>
            [ <field> <.crlf> ]+
            <.crlf>
            $<body>=.*
        }

        token type {
            | [ GET | POST | OPTIONS | HEAD | PUT | DELETE | TRACE | CONNECT ] <.accept>
            | <-[\/]>+ <.error>
        }

        token path {
            | '/' [[\w+]+ % \/] [\.\w+]? <.accept>
            | '*' <.accept>
            | \S+ <.error>
        }

        token field {
            | $<name>=\w+ <.ns> ':' <.ns> $<value>=<-crlf>* <.accept>
            | <-crlf>+ <.error>
        }

        method error(--> ::?CLASS:D) {
            $!invalid = True;
            self;
        }

        method accept(--> ::?CLASS:D) {
            $!invalid = False;
            self;
        }

        token crlf { \x[0d] \x[0a] }
        token ns { [ ' ' | <[\t]> ]* }
    }

    my $crlf = "\x[0d]\x[0a]";
    my $header = "GOT /index.html HTTP/1.1{$crlf}Host: docs.raku.org{$crlf}{$crlf}body";
    my $m = HTTPRequest.parse($header);
    is "type(\"$m.<type>\")={$m.<type>.invalid}", 'type("GOT ")=True',
        'the rejected request type is invalid';
    is "path(\"$m.<path>\")={$m.<path>.invalid}", 'path("/index.html")=False',
        'the accepted path is valid';
}

# The same example through a rule the walk evaluates.
{
    grammar W {
        has Bool $.invalid;
        token TOP { [ <field> <.nl> ]+ }
        token field { | $<name>=\w+ <.ns> ':' <.ns> $<value>=<-nl>* <.accept> | <-nl>+ <.error> }
        method error(--> ::?CLASS:D) { $!invalid = True; self }
        method accept(--> ::?CLASS:D) { $!invalid = False; self }
        token nl { \n }
        token ns { [ ' ' | <[\t]> ]* }
    }
    my $m = W.parse("Host: docs\nB: x\n");
    is-deeply $m<field>.map(*.invalid).List, (False, False),
        'a rule handed to the walk keeps its cursor as well';
}
