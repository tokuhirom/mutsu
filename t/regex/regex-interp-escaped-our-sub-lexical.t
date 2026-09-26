use Test;
# A regex literal inside an `our sub` interpolates a lexical of the module or
# package the sub was declared in, even when the sub is called after that
# package block has exited. Plain reads of the lexical already resolved through the
# sub's persisted scope; the regex capture read only `env`, where the
# lexical no longer lives. Found via App::Moneymoor's `parse-pence`
# (`unit module` + `my Str $current-decimal` + `/ ... $current-decimal ... /`).

use lib $?FILE.IO.parent(2).add('lib');
use RegexUnitLexical;

plan 7;

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

is whole-part('12.34'), 12, 'kebab-case compunit lexical interpolates';
nok whole-part('12x34').defined, 'and matches only its own value';

# A module routine called from regex code reads its own file-scope lexical.
ok 'cost GBP12' ~~ / $(money(12)) /, 'a $(...) call sees the callee module lexical';
ok 'cost GBP12' ~~ / <{ money(12) }> /, 'a <{...}> call sees the callee module lexical';
