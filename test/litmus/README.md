# Crash-consistency litmus tests

The oracle is [Ferrite](https://github.com/uwplse/ferrite) at
`923736167fb00aec979f244787a8c33e48e1c551` (master, 2016-09-01; `rosette/`
last changed in `a57453b`), and its paper, Bornholt et al., "Specifying and
Checking File System Crash-Consistency Models", ASPLOS 2016, at
`www/ferrite-asplos16.pdf` in that repository.

- `ferrite.ml` ports the Rosette models (`rosette/lang.rkt`, `seqfs.rkt`,
  `advfs.rkt`, `ext4.rkt`, `verifier.rkt`); its header lists the deviations.
- `test_ferrite.ml` runs Ferrite's litmus tests under eio-fs's crash model and
  compares with the ported models.
- `test_ours.ml` covers what Ferrite lacks (mkdir, unlink, symlink, truncate,
  failed fsync). No upstream states these; each test writes its ext4 and
  envelope sets by hand, from the sources its header names.

## Against upstream

| test | upstream expects | here |
|------|------------------|------|
| `rosette/litmus/append-chrome.rkt` | 3 states on seqfs, 7 on ext4, 4 on ext4 `nodelalloc`; surprising only on ext4 | matches |
| `rosette/litmus/create-rename.rkt` | without fsync 4, 3, 5 states on seqfs, advfs, ext4, surprising on advfs and ext4; with fsync 4, 4, 4, never surprising | matches |
| `rosette/litmus/replace.rkt` | without fsync 4, 3 on seqfs, advfs, surprising on advfs; with fsync 4, 4, never | matches; upstream has no ext4 case |
| `rosette/litmus/two-appends.rkt` | without fsync 5, 4 on seqfs, advfs, surprising on advfs; with fsync 5, 5, never | matches; upstream has no ext4 case |
| synthesis cases of the above (`synth`) | fence counts | not applicable: not ported |
| prefix-append (PA) | Figure 3: surprising on ext4, not on xfs, btrfs | eio-fs shows it |
| atomic-replace-via-rename (ARVR) | Figure 3: surprising on ext4, xfs, not btrfs | eio-fs shows it |
| atomic-create-via-rename (ACVR) | Figure 3: surprising on ext4, xfs, btrfs | eio-fs shows it |
| ordered-same-file-overwrites, ordered-two-file-overwrites, implied-directory-fsync | section 3 states them; Figure 3 reports no run | compared with the models only |

The "rosette outcome counts and verdicts" test checks the first four rows: the
ported models give upstream's `(check equal? (length all-states) n)` and its
`verify-correctness` verdict. Figure 3 was measured on Linux 4.1 in default
configuration (section 3.5); where it saw a surprising state, eio-fs must
produce it.

Against the ext4 model, eio-fs produces every state it allows and, per test,
exactly the extra states its `extra` list names. All but one have the shape
ext4 `data=ordered` forbids and `data=writeback` allows: a file with the
length a write gave it and zeros where that write's bytes go (the paper's
Definition 7, rule 4; `chaos.mli` draws a length independently of the
blocks). The other is in implied-directory-fsync: eio-fs's fsync does not
commit the file's creation, where ext4 commits it with the inode.

## Regenerate

Nothing here is generated: the counts and verdicts are copied from the Rosette
tests, Figure 3 from the paper. To re-derive the counts and verdicts, at that
revision, with Racket 6.6 and Rosette 2.0 (Ferrite's `circle.yml` and commit
`8d0079a`), as upstream's CI does:

    raco test rosette/litmus/*.rkt

The numbers in `test_ferrite.ml` were read from the `.rkt` sources at that
revision, not from a run.
