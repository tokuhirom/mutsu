use Test;
plan 1;

module RegexScope {
    my regex notmark { <-[⋅]> }
    role Scanner {
        method scan(Str:D $text) {
            $text ~~ / ^ (<notmark>*) /
        }
    }
}

my $scanner = '' but RegexScope::Scanner;
is $scanner.scan('abc⋅123').Str, 'abc',
    'a package lexical regex subrule is visible from a nested role method';
