use Test;

class StrictKeys {
    has Int %.entries{Str};
    has Int %.numbers{Int(Str)};
}
my $object = StrictKeys.new;
$object.entries<a> = 1;
dies-ok { $object.entries{42} = 2 }, 'accessor stores reject a mismatched key';
is $object.entries.elems, 1, 'a rejected accessor store leaves the hash unchanged';
my $alias := $object.entries;
dies-ok { $alias{42} = 3 }, 'scalar aliases retain the object hash key constraint';
is $alias.elems, 1, 'a rejected alias store leaves the hash unchanged';
sub entries { $object.entries }
dies-ok { entries(){42} = 4 }, 'computed targets retain the object hash key constraint';
is entries().elems, 1, 'a rejected computed store leaves the hash unchanged';
$object.numbers{'7'} = 5;
is $object.numbers{7}, 5, 'accessor stores coerce a key before indexing';
isa-ok $object.numbers.keys.head, Int, 'the original key records the coerced value';
dies-ok { $object.numbers{'invalid'} = 6 }, 'invalid numeric keys fail coercion';
is $object.numbers.elems, 1, 'failed coercion leaves the hash unchanged';
my %named{Str};
dies-ok { %named{42} = 7 }, 'named hashes use the same key validation';
is %named.elems, 0, 'a rejected named store leaves the hash unchanged';
my SetHash[Int] $set = SetHash[Int].new;
$set<7> = True;
ok $set<7>, 'QuantHash stores retain numeric allomorph acceptance';
my %any-keys{Any};
dies-ok { %any-keys{Mu.new} = 1 }, 'Any key constraints reject Mu instances';
is %any-keys.elems, 0, 'a rejected Mu key leaves the hash unchanged';
my %mu-keys{Mu};
%mu-keys{Mu} = 2;
is %mu-keys{Mu}, 2, 'Mu key constraints accept Mu type objects';
done-testing;
