## 0.2.0 (23/11/2026)

### Added

- `Eio_fs.Nofollow.run` is `read_file`'s bounded read for a file the caller opened another way (#13)
- `Eio_fs.Tree.allocated` is the bytes one entry allocates itself, without its descendants (#16)
- `Eio_fs.Sync.adopt` applies a directory's wrappers to a file the caller already holds, as an open through that directory would, without opening it again. A wrapper registers `Eio_fs.Sync.Adopt` to take part (#19)
- `Eio_fs_unix.await_record_lock` takes `?mode`: `` `Shared `` waits for a read lock, and `` `Exclusive ``, the default, for a write lock (#14)
- `Eio_fs_unix.clone_trees` clones several trees of one directory into another, all or none (#15)
- `Eio_fs_unix.Lease.await_free` waits for a lease's exclusive holder to release it and holds nothing while it waits, so it never holds off an exclusive take (#18)
- `Eio_fs_chaos.Fault.Cancel n` cancels a file's read or write `n` turns of the event loop in. The test fails if a byte reaches the buffer after the cancel, or if a write the cancel stopped stores any (#26)

### Fixed

- `Eio_fs_unix.extents` places bytes written and not yet synced: it raised `EOPNOTSUPP` for such a file on ext4 and returned no extent for those bytes on APFS. It writes the file back first, so a call costs a sync (#17)
- `eio-fs.unix` compiles on a system whose `lseek` has no `SEEK_DATA` or `SEEK_HOLE`, such as OpenBSD, where `seek_data` and `seek_hole` raise `ENOSYS` (#24)
- Closing the directory `Eio_fs_chaos.Private.Crash.wrap` returns leaves the base directory open: it closed a directory its caller still held (#20)

## 0.1.0 (21/11/2026)

Initial release.
