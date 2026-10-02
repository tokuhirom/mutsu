my %EXPORT;

module ExportHashKeyRoutines {
    BEGIN {
        %EXPORT<&delay-it> := sub delay-it(&code) { code() };
    }
}

sub EXPORT { %EXPORT }
