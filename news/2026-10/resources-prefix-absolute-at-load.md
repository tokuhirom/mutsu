# %?RESOURCES no longer follows a later chdir

The distribution prefix `%?RESOURCES` resolves against is now made absolute
when the module loads. With a relative `-I lib` / `use lib 'lib'` the prefix
was empty or relative, so `%?RESOURCES<x>` was read against whatever the cwd
was at the call: App::RaCoCo's reporter tests, which `indir` into a scratch
directory, looked for `resources/report-file.html` there and failed (item 7 of
#13126; `t/13-reporter-html.rakutest` now passes).
