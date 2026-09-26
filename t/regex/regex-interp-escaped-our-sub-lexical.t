use Test;
# A regex literal inside an `our sub` interpolates a lexical of the block or
# package the sub was declared in, even when the sub is called after that
# block has exited. Plain reads of the lexical already resolved through the
# sub's persisted scope; the regex capture read only `env`, where the
# lexical no longer lives. Found via App::Moneymoor's `parse-pence`
# (`unit module` + `my Str $current-decimal` + `/ ... $current-decimal ... /`).

plan 4;

{
    my $dec = '.';
    our sub block-sub($s) { so $s ~~ / ^ \d+ $dec \d+ $ / }
}
ok OUR::block-sub('12.34'), 'bare-block lexical interpolates after the block exits';

module M {
    my $mark = ',';
    our sub mod-sub($s) {
        $s ~~ / ^ (\d+) [ $mark (\d ** 1..2) ]? $ / ?? "$0|{$1 // ''}" !! 'no'
    }
}
is M::mod-sub('12,5'), '12|5', 'module lexical interpolates with captures';
is M::mod-sub('12.5'), 'no', 'module lexical is not a metachar';

package P {
    my $sep = '-';
    our sub pkg-sub($s) { so $s ~~ / ^ a $sep b $ / }
}
ok P::pkg-sub('a-b'), 'package-block lexical interpolates';
