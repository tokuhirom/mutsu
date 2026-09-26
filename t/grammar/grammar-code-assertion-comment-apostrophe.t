use Test;

# Regression from Timezones::ZoneInfo::State (Timezone::Simple). The comment
# contains both an apostrophe and a brace-like punctuation mark: the parser and
# the runtime regex scanner must agree that neither starts syntax.
plan 2;

grammar CommentedAssertion {
    token TOP {
        <.digit>
        <?{
            # It's valid to mention punctuation like } in a code assertion.
            True
        }>
    }
}

ok CommentedAssertion.parse('7'), 'code assertions ignore apostrophes in comments';

ok '7' ~~ / <.digit> <?{ # It's valid here too: }
    True
}> /, 'runtime code assertion scanner ignores comment punctuation';
