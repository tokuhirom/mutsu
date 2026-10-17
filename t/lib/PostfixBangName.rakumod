unit module PostfixBangName;

sub postfix:<!mx>($value) is export { "imported $value" }
