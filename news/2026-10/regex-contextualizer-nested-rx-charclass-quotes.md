# Nested regex character classes keep contextualizer quotes literal

Match-time contextualizers now skip nested slash-delimited regex literals while
finding their closing parenthesis. Quote characters inside a nested regex
character class no longer get mistaken for code string delimiters.
