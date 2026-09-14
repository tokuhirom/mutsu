# Plain regex code blocks retain their RakuAST shape

Plain `{ ... }` regex code blocks now retain `RakuAST::Regex::Block` in
`.AST`, including the nested `RakuAST::Block`. Constructed RakuAST trees and
parser-created regexes execute the block through the existing inline regex-code
matcher.

Interpolated blocks (`<{ ... }>`), code interpolation, and other
runtime-valued regex bodies remain separate boundaries.
