use Test;

# Grammar actions run on the Match node the matcher stored, without
# materializing it first (#18207). Everything an action can observe of its
# node must stay the same: its captures through `$/`, `$<x>` and `$0`, in the
# action itself and in closures inside it, the children's `.made`, the `.made`
# it leaves, and the dispatch order of named, positional and silent children.

plan 14;

grammar Expr {
    token TOP { <term>+ % <op> }
    token term { <num> | '(' <TOP> ')' }
    token num { (\d+) ['.' (\d+)]? }
    token op { <[+*]> }
}

{
    my @order;
    my class A {
        method num($/) { @order.push("num:$/"); make $1 ?? "$0.$1" !! ~$0 }
        method term($/) { @order.push("term:$/"); make ($<num> // $<TOP>).made }
        method op($/) { @order.push("op:$/"); make ~$/ }
        method TOP($/) {
            @order.push("TOP:$/");
            make $<term>».made.join(' ') ~ ' / ' ~ $<op>».made.join;
        }
    }
    my $m = Expr.parse('1+(2.5*3)', :actions(A));
    is $m.made, '1 2.5 3 / * / +', 'nested made values reach the top';
    is-deeply @order,
        ['num:1', 'term:1', 'op:+', 'num:2.5', 'term:2.5', 'op:*', 'num:3', 'term:3',
         'TOP:2.5*3', 'term:(2.5*3)', 'TOP:1+(2.5*3)'],
        'actions run in reduction order';
    is ~$m<term>[1]<TOP><term>[0]<num>[1], '5', 'the finished tree keeps its captures';
}

{
    my class B {
        method num($/) { make { int => ~$0, frac => ($1 // '').Str } }
        method term($/) {
            my &get = { $<num>.made<int> };
            my &pos = { $/<num>[0].Str };
            make get() ~ '|' ~ pos();
        }
        method TOP($/) { make $<term>».made }
    }
    my $m = Expr.parse('12+3.4', :actions(B));
    is-deeply $m.made.List, ('12|12', '3|3'), 'closures in an action see its captures';
}

{
    my @seen;
    my class C {
        method num($m) { @seen.push($m.Str); $m.make($m.Int) }
        method TOP($/) { make [+] $<term>».<num>».made }
    }
    my $m = Expr.parse('1+2+3', :actions(C));
    is $m.made, 6, 'an action whose signature does not name $/';
    is-deeply @seen, ['1', '2', '3'], 'its argument is the node';
}

{
    my class D {
        method num($/) { make ~$/ }
        method term($/) {
            # `$<op>` belongs to the parent: it must not be visible here.
            make ($<op>:exists) ?? 'leaked' !! $<num>.made;
        }
        method TOP($/) { make $<term>».made.join(',') }
    }
    is Expr.parse('7*8', :actions(D)).made, '7,8', 'a child action does not see its parent\'s captures';
}

grammar Alias {
    token TOP { <first=word> ' ' <word> <.sp> }
    token word { \w+ }
    token sp { ' '* }
}

{
    my @calls;
    my class E {
        method word($/) { @calls.push(~$/); make uc ~$/ }
        method sp($/) { @calls.push('sp') }
        method TOP($/) { make $<first>.made ~ '-' ~ $<word>».made.join(',') }
    }
    my $m = Alias.parse('ab cd ', :actions(E));
    is $m.made, 'AB-AB,CD', 'an aliased capture reads its action\'s made';
    is-deeply @calls, ['ab', 'cd', 'sp'], 'a silent subrule\'s action runs in order';
}

grammar Proto {
    token TOP { <item>+ % ',' }
    proto token item {*}
    token item:sym<n> { \d+ }
    token item:sym<w> { <[a..z]>+ }
}

{
    my class F {
        method item:sym<n>($/) { make 'N' ~ $/ }
        method item:sym<w>($/) { make 'W' ~ $/ }
        method TOP($/) { make $<item>».made.join(' ') }
    }
    is Proto.parse('1,ab,22', :actions(F)).made, 'N1 Wab N22', 'proto variants dispatch to their sym methods';
}

grammar Dyn {
    token TOP { :my $*depth = 0; <level> }
    token level { '[' { $*depth++ } <level>? ']' }
}

{
    my @depths;
    my class G {
        method level($/) { @depths.push($*depth) }
    }
    Dyn.parse('[[[]]]', :actions(G));
    is-deeply @depths, [3, 3, 3], 'a rule\'s :my $* variable is visible to the actions';
}

{
    my class H {
        method num($/) { make +$/ }
        method TOP($/) { make [*] $<term>».<num>».made }
    }
    my $m = Expr.parse('2*3*4', :actions(H));
    is $m.made, 24, 'the top action\'s make';
    is ~$/, '2*3*4', '$/ after .parse is the parse result';
    is $/.made, 24, 'and carries the made value';
}
