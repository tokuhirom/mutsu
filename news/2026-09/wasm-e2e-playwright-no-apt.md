The WASM end-to-end job now installs Chromium without Playwright's
`--with-deps` apt step, avoiding Ubuntu mirror stalls on runners that already
provide the required shared libraries.

Fixes #7958.
