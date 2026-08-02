# Clean stale spurt fixture before roast

An interrupted `roast/S32-io/spurt.t` run can leave its fixed-name temporary
file behind, causing the next otherwise clean `make roast` to abort. The roast
target now removes that known fixture before starting the suite.
