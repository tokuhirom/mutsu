use v6;
use Test;

plan 4;

grammar ProtoRuleFallback {
    rule TOP { <p> }
    proto rule p {*}
    rule p:sym<a> { 'x' 'zzz' }
    rule p:sym<b> { 'x' 'yyy' }
}

my $match = ProtoRuleFallback.subparse('x yyy', :rule<p>);
ok $match, 'subparse tries the next ranked proto candidate after a failed one';
is ~$match, 'x yyy', 'the fallback proto candidate consumes the input';

my $parsed = ProtoRuleFallback.parse('x yyy', :rule<p>);
ok $parsed, 'parse tries the next ranked proto candidate after a failed one';
is ~$parsed, 'x yyy', 'the fallback proto candidate satisfies the full parse';
