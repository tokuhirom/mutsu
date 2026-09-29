# Regex class union with a negated left operand

`<-[\s] + [x]>` and `<-[b] + [c]>` used to match nothing but the right operand, because
the leading negated part was treated as a subtraction from the positive part. A class
that starts with a negated part now begins from every character except it, and a later
`+` part is united with that complement; later `-` parts still subtract from the result
(`<-[b] + [c] - [a]>`). Fixes #9907.
