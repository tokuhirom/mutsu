# A lookaround body with an inline adverb has a source tree

`<?before [:i 'not(']>` (and `<!before :i x>`, `<?after [:i x]>`) lost its regex source tree because the
lookaround body check did not accept an inline adverb (`InternalModifier`), so a rule using it died at
module load on the RakuAST frontend with "rule declaration without a source tree".
`CSS::Module::CSS3::Selectors`' `negation-expr` is the real instance. The tree is read only by the
RakuAST boundary; the matcher keeps the rule's text, as for the other RakuAST-only constructs.
