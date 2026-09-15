Title: RakuAST preserves indexed regex subrule arguments

Argumented regex subrules with ordinary indexed expressions, such as
`<word(@values[$index])>`, can now be lowered from RakuAST back to the existing
regex parser. Their array and index values remain live at match time.
