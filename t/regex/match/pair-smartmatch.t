use Test;

plan 3;

# MoarVM::Remote matches event pairs such as :stdout(/ alive /) and
# :event(Proc) in its test helpers.
my $topic = "stdout" => "alive";
ok $topic ~~ :stdout(/ alive /), 'a Pair smartmatches a named Pair pattern';

given $topic {
    when :stdout(/ alive /) {
        is .value, 'alive', 'a matching when keeps the Pair as the topic';
    }
    default {
        flunk 'a matching Pair when clause was not selected';
    }
}

given :event(42) {
    when :event(Int) {
        pass 'a Pair pattern smartmatches its value against a type';
    }
    default {
        flunk 'a Pair type pattern was not selected';
    }
}
