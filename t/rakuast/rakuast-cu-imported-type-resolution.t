use Test;
use lib 't/lib-cu-imported-type';
use CuTypes :DEFAULT, :types;
use CuReader :DEFAULT, :types;

# A type a `use` imported resolves in `$*CU` by its short name, also when a
# module's routine builds `$*CU` (static's CHECK-time analysis: `* ~~
# CheckFailure` after `use static :DEFAULT, :types` died with "'CheckFailure'
# has not been resolved").

plan 4;

my %direct;
my %via-module;
CHECK {
    sub walk($n) {
        %direct{$n.name.canonicalize} = (try $n.compile-time-value.^name) // "unresolved"
            if $n ~~ RakuAST::Type::Simple;
        $n.visit-children(&walk);
    }
    walk($*CU);
    %via-module = type-values();
}

my $a = 1 ~~ Tagged;
my $b = 1 ~~ Plain;
my $c = 1 ~~ ReaderType;

is %direct<Tagged>, 'CuTypes::Tagged', 'a type imported under a tag';
is %direct<Plain>, 'CuTypes::Plain', 'a type imported by default';
is %direct<ReaderType>, 'CuReader::ReaderType', 'a type of the module that reads $*CU';
is %via-module<ReaderType>, 'CuReader::ReaderType', '... also when that module builds $*CU';
