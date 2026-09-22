unit module Regex::Hyphen::Fixture;

my $vowel = 'аеиоуыэюя';
my $NO-VOWEL = '<-[' ~ $vowel ~ ']>';
my $VOWEL = '<[' ~ $vowel ~ ']>';
my $PERFECTIVEGROUND = ' ((ив|ивши|ившись|ыв|ывши|ывшись) | ((<?after <[ая]>>) (в|вши|вшись))) $ ';
my $REFLEXIVE = ' (с <[яь]>) $ ';
my $ADJECTIVE = ' (ее|ие|ые|ое|ими|ыми|ей|ий|ый|ой|ем|им|ым|ом|его|ого|ему|ому|их|ых|ую|юю|ая|яя|ою|ею) $ ';
my $PARTICIPLE = ' ((ивш|ывш|ующ)|((<?after <[ая]>>) (ем|нн|вш|ющ|щ))) $ ';
my $VERB = ' ((ила|ыла|ена|ейте|уйте|ите|или|ыли|ей|уй|ил|ыл|им|ым|ен|ило|ыло|ено|ят|ует|уют|ит|ыт|ены|ить|ыть|ишь|ую|ю)|((<?after <[ая]>>) (ла|на|ете|йте|ли|й|л|ем|н|ло|но|ет|ют|ны|ть|ешь|нно))) $ ';
my $NOUN = ' (а|ев|ов|ие|ье|е|иями|ями|ами|еи|ии|и|ией|ей|ой|ий|й|иям|ям|ием|ем|ам|ом|о|у|ах|иях|ях|ы|ь|ию|ью|ю|ия|ья|я) $ ';
my $RVRE = rx / ^ (.*? <$VOWEL> ) (.*) $ /;
my $DERIVATIONAL = rx / <$NO-VOWEL> <$VOWEL>+ <$NO-VOWEL>+ <$VOWEL> .* <?after 'о'> 'сть'? $ /;

proto derivational($wordSpec) is export {*}

multi derivational(@words --> List) {
    @words.map({ derivational($_) }).List
}

multi derivational(Str:D $word --> Str) {
    my ($start, $RV) = |($word.lc ~~ $RVRE);
    return $word unless $RV;
    unless $RV ~~ s/ <{$PERFECTIVEGROUND}> // {
        $RV ~~ s/ <{$REFLEXIVE}> //;
        if ($RV ~~ s/ <{$ADJECTIVE}> //) {
            $RV ~~ s/ <{$PARTICIPLE}> //;
        } else {
            $RV ~~ s/ <{$NOUN}> // unless $RV ~~ s/ <{$VERB}> //;
        }
    }
    $RV ~~ s/и$//;
    $RV ~~ s/ость?$// if $RV ~~ $DERIVATIONAL;
    unless ($RV ~~ s/ь$//) {
        $RV ~~ s/ейше?//;
        $RV ~~ s/нн$/н/;
    }
    $word.substr(0, $start.chars) ~ $word.substr($start.chars, $RV.Str.chars)
}
