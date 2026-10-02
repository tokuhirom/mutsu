# `my $x .= $callable` parses (CSS::TagSet loads)

A declaration with a mutating assignment whose method is a variable
(`my $actions .= $module.actions.new`, the indirect `.$callable` form) died with
"Two terms in a row". The declaration form now builds the same indirect method call the
statement form already did and sinks any postfix chain after it, as Rakudo does. This
unblocks loading `CSS::Stylesheet`, and with it `CSS::TagSet::{XHTML,Pango,TaggedPDF}`
(found via the ecosystem roulette). The suites still fail further in, in a separate issue.
