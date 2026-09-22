use Test;

class ReturnType::Version-info { }

class ReturnTypeHolder {
    method version-info returns ReturnType::Version-info {
        ReturnType::Version-info.new
    }
}

my $value = ReturnTypeHolder.new.version-info;
isa-ok $value, ReturnType::Version-info,
    'a qualified hyphenated type is accepted after the returns trait';

done-testing;
