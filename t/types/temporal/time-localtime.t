use v6.*;
use Test;

use lib 't/lib';

plan 5;

{
    use TimeLocaltimeFixture;

    is localtime.sec, 7,
        'an imported localtime routine wins over the native same-named term';
    ok (localtime() ~~ Time::localtime),
        'the imported localtime routine returns its module type';
    for 1..1 {
        ok OUTER::MY::<&localtime>:exists,
            'an imported localtime routine is visible in the lexical pseudo-stash';
    }
}

{
    use TimeLocaltimeFixture :FIELDS;

    my $field = '$tm_sec';
    for 1..1 {
        ok OUTER::MY::<<$field>>:exists,
            'an imported scalar field is visible in the lexical pseudo-stash';
    }
    is localtime.sec, 7,
        'the imported routine still works when the field tag is requested';
}
