# `.origin` of plain lookarounds and word boundaries in regexes

Two regex spellings used to end the origin walk of a regex body:
- a lookaround over a plain named assertion (`<?alpha>`, `<!ws>`);
- the word boundary `<|w>` and its negation `<!|w>`.

Everything after them was then left without a range.

Both are now read as written, as on rakudo 2026.11:
- the lookaround runs over `<?alpha>`, and its `Assertion::Named` and `Name` over the name;
- `<|w>` is an `Assertion::Named` over its four characters, with no range on its name.

The atoms after them are placed too (#15780).
