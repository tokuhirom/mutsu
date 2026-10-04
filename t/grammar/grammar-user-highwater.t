use Test;

plan 5;

grammar UserHighWater {
    rule TOP { 'a' 'b' 'c' }
    method ws() {
        if self.pos > $*HIGHWATER {
            $*HIGHWATER = self.pos;
        }
        callsame;
    }
}

{
    my $*HIGHWATER = 0;
    nok UserHighWater.parse('a b x'), 'the parse fails after reaching two spaces';
    is $*HIGHWATER, 3, 'a failed parse preserves the user ws method high-water mark';
}

{
    my $*HIGHWATER = 8;
    UserHighWater.parse('a b x');
    is $*HIGHWATER, 8, 'parse diagnostics do not overwrite a larger user mark';
}

grammar Plain { token TOP { 'ab' } }
{
    my $*HIGHWATER = 0;
    nok Plain.parse('ax'), 'the plain grammar fails';
    is $*HIGHWATER, 0, 'a grammar without a writer leaves the user variable alone';
}
