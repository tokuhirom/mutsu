use Test;

# Regression from Game::Entities: dotted subscripts inside a topical block
# preserve semicolon-separated dimensions.
my $set = [ [], [], [] ];
$set[2].push: 'component';
$set[1].push: 0;
$set[0; 7] = 0;

given $set {
    .[2; .[0; 7]] = 'replacement';
}

is $set[2; $set[0; 7]], 'replacement',
    'a topical multidimensional subscript assignment keeps its dimensions';

done-testing;
