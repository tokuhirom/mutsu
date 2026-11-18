# The ecosystem page and the ledger summary publish one KPI

The ecosystem compatibility page showed "distributions fully passing 99.2%
(1324/1335)" and "test files passing 99.7%" while the ledger's `summary.md`, and
the history chart on the same page, said 98.7% (1327/1344) and 99.5% (5194/5220).
The page was not stale: the Pages workflow already redeploys on every
`Ecosystem sweep` completion, and the deploy after the 2026-11-18 sweep pulled the
ledger tip 28 seconds after it landed. The two numbers were computed from the same
records under two definitions. `gen-ecosystem-manifest.py` left the 15
distributions in `ecosystem/exclude.txt` out of the KPI, as the page's method text
promises. The rollup in `ecosystem-sweep.py` counted them, although bulk sweeps
skip them and their records are frozen measurements from weeks earlier.

Both publishers now call one function, `ecosystem_common.kpi()`, which leaves
excluded distributions out of every parity figure, like `no_baseline` ones. The
rollup's summary records which distributions it excluded and lists their count.
The page generator refuses to publish figures that differ from the ledger's
`summary.json` for the same exclude list. The Pages deploy now fails instead of
publishing zeros when the generator fails on a pulled ledger. Against the
2026-11-18 ledger both now read 99.2% (1324/1335) dist parity and 99.7%
(5106/5121) file parity. The history series steps up by that difference at the
next sweep.
