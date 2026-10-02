# CI runner images are pinned instead of floating on `-latest`

Every GitHub Actions workflow used `ubuntu-latest`, and the release build used
`macos-latest` for both macOS targets. Those aliases follow GitHub's schedule:
`macos-latest` had already moved to macOS 26 under us, and `ubuntu-latest` will
move off 24.04 the same way, changing the compiler, the glibc the Linux release
tarball links against and the preinstalled packages without any commit in this
repository.

All workflows now name the image they actually ran on — `ubuntu-24.04` and
`macos-26` (read from the runner logs of the v0.24.0 release run), next to the
already pinned `ubuntu-24.04-arm` — so nothing changes today, and a future image
bump is an explicit, reviewed edit. `make check-runner-pins` rejects a `-latest`
label and runs in CI's always-on `changes` job, so it also covers PRs that edit
only a non-`ci.yml` workflow (which are otherwise docs-only).
