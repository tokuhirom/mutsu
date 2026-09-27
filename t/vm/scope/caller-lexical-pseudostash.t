use Test;
use lib 't/lib';
use P5unlink;

plan 4;

'tmp/caller-lexical-pseudostash-file'.IO.spurt;
with 'tmp/caller-lexical-pseudostash-file' {
    use isms 'Perl5';
    ok .IO.e, 'the caller topic file exists before unlink';
    is unlink, 1,
        'CALLER::LEXICAL::<$_> reads the caller lexical topic';
    nok .IO.e, 'the caller topic was unlinked';
}

sub caller-lexical-name() {
    CALLER::LEXICAL::<$name>
}

my $name = 'p5unlink-name';
is caller-lexical-name(), 'p5unlink-name',
    'CALLER::LEXICAL::<$name> reads a caller lexical';
