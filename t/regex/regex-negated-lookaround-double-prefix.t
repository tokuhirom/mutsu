use Test;

# `<?!x>` is the zero-width negative assertion spelled with both prefixes; it
# means `<!x>`. Found via the CSS::Minifier distribution, whose unit stripper
# matches `0 <$UNITS> <?!alpha>`.

plan 7;

is ~('0px;' ~~ / 0 'px' <?!alpha> /), '0px', '<?!alpha> passes before a non-letter';
nok '0pxa' ~~ / 0 'px' <?!alpha> /, '<?!alpha> fails before a letter';
is ~('a;' ~~ / a <?!before \w> /), 'a', '<?!before ...> passes';
nok 'ab' ~~ / a <?!before b> /, '<?!before ...> fails';
nok 'ba' ~~ / <?!after b> a /, '<?!after ...> fails';
is ~('ca' ~~ / <?!after b> a /), 'a', '<?!after ...> passes';
nok "a'" ~~ / a <?!before "'"> /, 'a quoted body inside <?!before ...>';
