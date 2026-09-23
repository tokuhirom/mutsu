use Test;

# Issue #9053. Two parts of Rakudo's LTM declarative-prefix semantics:
#
# 1. A fate (a non-declarative atom) ends only the NFA path that reaches it.
#    The prefix is the furthest place ANY path reaches a fate or the end of
#    the pattern, not the first fate a depth-first walk happens to meet.
# 2. A negated `[...]` class that Rakudo compiles into two or more
#    alternatives (`<-[Z \n]>`, `<-[a..c x]>`, `<-[\d x]>`) becomes
#    `[<?conj> .]`, which the NFA cannot build: it is a fate. A negated class
#    with a single alternative (`<-[\n]>`, `<-[a..c]>`, `<-[\t \r x]>`) stays
#    declarative. `\n` is its own alternative (it also matches `\r\n`); a
#    `\x0a` escape is a plain entry.
# 3. A package-qualified subrule call (`<Other::x>`, even `<Own::x>`) is a
#    fate: the NFA looks the rule up as a cursor method and finds none.

plan 14;

# The reduction from CSS::Module::CSS3::Selectors' `negation-expr`.
grammar Sel {
    token nonascii { <-[Z \n]> }
    token nmstrt   { <[a..z]> }
    token nmchar   { <[a..z]> | <char=.nonascii> }
    token Id       { <nmstrt> <nmchar>* }
    token Ident    { <Id> }
    token element-name { <Id> }
    proto rule _any {*}
    rule _any:sym<ident>  { <.Ident> }
    rule any-arg   {<_any>}
    rule no-namespace {<?>}
    rule namespace-prefix {[<prefix=.Id>|<prefix=.no-namespace>]'|'}
    rule qname     {<namespace-prefix>? <element-name>}
    rule negation-expr {[<qname> | <any-arg> ]+}
    rule negation-rev  {[<any-arg> | <qname> ]+}
}

{
    my $m = Sel.subparse('p', :rule<negation-expr>);
    is $m<qname>.elems, 1, 'tied prefixes: the first-declared qname wins';
    is $m<any-arg>.elems, 0, '... and any-arg is not taken';
    $m = Sel.subparse('p', :rule<negation-rev>);
    is $m<any-arg>.elems, 1, 'declared the other way round, any-arg wins the tie';
}

# qname reaches offset 2 only through a fate inside the optional
# namespace-prefix (the `<-[Z \n]>` branch of nmchar after "pq"). A walk that
# stopped at the first fate it met measured it as 1 and tried x1 first.
{
    my @log;
    my grammar Order {
        token nonascii { <-[Z \n]> }
        token nmchar   { <[a..z]> | <char=.nonascii> }
        token Id       { <[a..z]> <nmchar>* }
        rule no-namespace {<?>}
        rule namespace-prefix {[<prefix=.Id>|<prefix=.no-namespace>]'|'}
        rule qname     {<namespace-prefix>? <Id>}
        token x1 { . { @log.push: 'x1' } <!> }
        token x2 { .. { @log.push: 'x2' } <!> }
        token q1 { [ <x1> | <qname> { @log.push: 'q' } ] }
        token q2 { [ <x2> | <qname> { @log.push: 'q' } ] }
    }
    Order.subparse('pq', :rule<q1>);
    is-deeply @log, ['q'], 'qname (prefix 2) outranks a prefix-1 sibling';
    @log = ();
    Order.subparse('pq', :rule<q2>);
    is-deeply @log, ['x2', 'q'], '... and ties with a prefix-2 sibling declared first';
}

# Which negated classes end the declarative prefix. `a` repeats the class
# twice; it wins over the one-character `b` only when the class is declarative.
my $n = 0;
sub class-is-declarative(Str $class, Str $input) {
    use MONKEY-SEE-NO-EVAL;
    my $name = 'Cls' ~ $n++;
    my $g = EVAL "my grammar $name \{ token a \{ $class $class \}; token b \{ . \}; "
        ~ "token TOP \{ [<b> | <a>] \} \}; $name";
    so $g.subparse($input)<a>;
}

ok !class-is-declarative('<-[Z \n]>', 'pq'),   '<-[Z \n]> (two alternatives) is a fate';
ok !class-is-declarative('<-[a..c x]>', '!!'), '<-[a..c x]> is a fate';
ok !class-is-declarative('<-[\d x]>', '!!'),   '<-[\d x]> is a fate';
ok !class-is-declarative('<-[\n \r]>', '!!'),  '<-[\n \r]> is a fate';
ok  class-is-declarative('<-[\n]>', '!!'),     '<-[\n]> stays declarative';
ok  class-is-declarative('<-[a..c]>', '!!'),   '<-[a..c]> stays declarative';
ok  class-is-declarative('<-[\x0a x]>', '!!'), '<-[\x0a x]> stays declarative (a hex escape is a plain entry)';

grammar QOther { token x { .. } }
grammar QOwn {
    token x { .. }
    token b { . }
    token t-other { [ <b> | <QOther::x> ] }
    token t-own   { [ <b> | <QOwn::x> ] }
}
is ~QOwn.subparse('pq', :rule<t-other>), 'p', 'a qualified subrule call from another grammar is a fate';
is ~QOwn.subparse('pq', :rule<t-own>),   'p', 'a qualified call naming the grammar itself is a fate too';
