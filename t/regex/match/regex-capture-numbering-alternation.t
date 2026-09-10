use Test;

plan 9;

if 'abcd' ~~ / a [ b (.) || (x) (y) ] (.) / {
    is ~$0, 'c', 'short sequential-alternation branch keeps its capture';
    ok !$1.defined, 'short sequential-alternation branch reserves a Nil capture';
    is ~$2, 'd', 'capture after sequential alternation follows its widest branch';
} else {
    flunk 'short sequential-alternation branch matches';
    skip 'short sequential-alternation capture checks', 3;
}

if 'axyd' ~~ / a [ b (.) || (x) (y) ] (.) / {
    is ~$0, 'x', 'wide sequential-alternation branch captures at $0';
    is ~$1, 'y', 'wide sequential-alternation branch captures at $1';
    is ~$2, 'd', 'following capture stays at $2 after wide branch';
} else {
    flunk 'wide sequential-alternation branch matches';
    skip 'wide sequential-alternation capture checks', 3;
}

if 'abcd' ~~ / a [ b (.) | (x) (y) ] (.) / {
    is ~$0, 'c', 'short ordinary-alternation branch keeps its capture';
    ok !$1.defined, 'short ordinary-alternation branch reserves a Nil capture';
    is ~$2, 'd', 'capture after ordinary alternation follows its widest branch';
} else {
    flunk 'short ordinary-alternation branch matches';
    skip 'ordinary-alternation capture checks', 3;
}
