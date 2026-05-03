# Agent Notes

## Repo Shape
- This is a SWI-Prolog pack workspace/orchestrator; most top-level pack dirs are git submodules listed in `.gitmodules`.
- Active packs are enumerated in `packages.pl`; `idfpml` is skipped on `aarch64-linux`.
- `plsteroids.pl` is the main load entrypoint. It loads `xtools/prolog/record_locations` first, disables autoload while loading `packages`, `plsdirs`, and `pltools`, then re-enables autoload.
- `plsconfig.pl` points `plroot` at `target/lib` when that directory exists, otherwise at the source tree. If behavior looks stale after generated builds, remove or rebuild `target` rather than assuming source files are loaded.

## Commands
- Check the pinned/runtime SWI-Prolog version with `make plstatus`; the required version is in `bin/swipl_version`.
- Build the required SWI-Prolog under `$(HOME)/swipl-devel` / `$(HOME)/bin` with `make swipl` when `make plstatus` reports a mismatch.
- Build generated libs and saved state with `make build`; `make all` runs `make swipl`, cleans, builds, then runs the full test suite.
- Load everything without running tests: `make loadall`.
- Full tests: `make tests`. It uses `bin/benchtests.txt` to order tests and writes stdout/stderr under `target/`.
- Run one unit test file: `make path/to/test.plt`.
- Run all unit tests under a pack/dir: `make path/to/dir.utest`.
- Run the runtime-check variant for a unit test: `make path/to/test.plr`; for a pack/dir use `make path/to/dir.rtest`.
- Static checks: `make check` for all, `make checkc` for concurrent, or `make checker_name.stest` for one checker such as `make undefined.stest`.
- Package load isolation: `make checkload` for all packs or `make packname.checkload` for one pack.

## Test/Build Gotchas
- Test wrappers use `target/bin/plsteroids` if it exists; otherwise they fall back to `swipl` plus loader goals.
- `make build` builds the Intel BID library under `idfpml/prolog/idfpml/LIBRARY` except on `aarch64`.
- Generated artifacts live in `target/` and include `*.so`, `*.o`, `*_impl.h`, `*_intf.c`, `*_intf.h`, and `*_so.pl`; they are ignored and should not be hand-edited.
- `bin/concurrent` may try distributed/NFS workers; set `CONCURRENT_DISABLED=1` when a local-only run is needed.

## Static Analysis
- Static checker implementations are `stchecks/prolog/check_*.pl`; checker target names drop the `check_` prefix and `.pl` suffix.
- `make check` and `make checker.check` call `checkall` / `showcheck` with `[dir(pltool(prolog))]`, so they analyze the Prolog library paths configured by the workspace loader rather than arbitrary current-directory files.
