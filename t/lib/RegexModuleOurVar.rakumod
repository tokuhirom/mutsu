unit module RegexModuleOurVar;

# A file-scope `our` regex filled in at BEGIN time, interpolated as `<$NAMED>`
# by this module's own routines (the shape of CSS::Minifier::Util).
our $NAMED is export;
BEGIN { $NAMED = rx:i/ red | blue / }

sub apply(&op, $v) { op($v) }

sub recolor(Str $v --> Str) is export {
    apply(-> $s is copy { $s ~~ s:g/ <$NAMED> /C/; $s }, $v)
}

sub has-named(Str $v --> Bool) is export { so $v ~~ / <$NAMED> / }
