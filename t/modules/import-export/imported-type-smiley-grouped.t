use Test;
use NativeCall;

# Math::DistanceFunctionish uses a grouped smartmatch against CArray:D.
my $c = CArray[int].new;
ok $c ~~ CArray:D, 'an imported type keeps its smiley in a smartmatch';
ok !(1 ~~ CArray:D), 'a grouped smartmatch against an imported type parses';
ok (CArray:D) === CArray:D, 'parentheses retain the type smiley';

done-testing;
