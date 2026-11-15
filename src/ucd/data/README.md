# Unicode 17 property sources

These files are the unmodified Unicode Character Database 17.0.0 releases:

| File | Source | SHA-256 |
| --- | --- | --- |
| `Blocks-17.0.0.txt` | `https://www.unicode.org/Public/17.0.0/ucd/Blocks.txt` | `c0edefaf1a19771e830a82735472716af6bf3c3975f6c2a23ffbe2580fbbcb15` |
| `DerivedGeneralCategory-17.0.0.txt` | `https://www.unicode.org/Public/17.0.0/ucd/extracted/DerivedGeneralCategory.txt` | `d62e5bab70ca74f099343f71224fa051cb1fdd61a1ab45c0488c44cfc0b6102e` |
| `Scripts-17.0.0.txt` | `https://www.unicode.org/Public/17.0.0/ucd/Scripts.txt` | `9f5e50d3abaee7d6ce09480f325c706f485ae3240912527e651954d2d6b035bf` |

Their copyright and license notices are retained in each file. `gc_gen` and
`unicode_script_gen` generate the compiled lookup tables from these sources.
MoarVM classifies 22 reserved codepoints at the ends of CJK extensions D, F,
and G as `Lo` / `Han`. The generators apply that block-level compatibility
rule after reading the standard property files.
