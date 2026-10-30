unit module MonkeyClosureRegex;
use MONKEY-SEE-NO-EVAL;

class Router {
    has @.seen;
    method note($m) { @!seen.push(~$m) }
}

sub make-regex(Router $r, Str $src) is export {
    my $note = $r;
    my token R { ^ <{$src}> };
    &R
}
