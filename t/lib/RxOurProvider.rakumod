our $rx-word = regex { <[a..z]>+ };
sub rx-check(Str $s) is export {
    $s ~~ m:i{^$rx-word$} ?? "word:$s" !! "other:$s"
}
