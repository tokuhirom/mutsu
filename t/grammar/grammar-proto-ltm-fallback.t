use Test;

plan 3;

grammar ProtoLtmFallback {
    proto rule item {*}
    rule item:sym<unicode-range> {
        :i 'U+' [
            $<from>=[<.xdigit>**1..6] '-' $<to>=[<.xdigit>**1..6]
            || $<mask>=[<.xdigit>|'?']**1..6
        ]
    }
    rule item:sym<ident> { [<.alpha>|<.xdigit>]+ <!before '('> }
}

my $match = ProtoLtmFallback.subparse('u+2??a', :rule<item>);
ok $match && $match<mask>,
    'a stopped LTM measurement keeps proto candidates in declaration order';

grammar StoppedLtmToken {
    proto token term {*}
    token term:sym<unicode-range> {
        :i 'U+' [
            $<from>=[<.xdigit>**1..6] '-' $<to>=[<.xdigit>**1..6]
            || $<mask>=[<.xdigit>|'?']**1..6
        ]
    }
    token term:sym<ident> { [<.alpha>|<.xdigit>]+ <!before '('> }
    token TOP { <term> }
}

$match = StoppedLtmToken.parse('U+2??a');
ok $match && ~$match eq 'U+2??a',
    'a stopped LTM measurement does not let a shorter proto token win';
ok $match<term><mask>,
    'the proto token selected after the stopped measurement exposes its capture';
