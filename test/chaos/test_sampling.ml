(* With 254 records, the four outside the linear tier's structure leave 250
   writes, each tried with four faults: a single byte, a tear, ENOSPC and
   EIO, in that order. The sampled tier has 250 runs for those 1000
   candidates, so taking every fourth one would only ever move a single
   byte, and never tear a write in the middle of the log. *)

let check w () = Eio_fs.Chaos.check w

let suite =
  ( "sampling",
    [
      Alcotest.test_case "log: correct" `Quick
        (Expect.passes (check (Sampling.log ~n:254)));
      Alcotest.test_case
        "log: no rollback in the middle, by the sampled tier, whatever the \
         period of its candidates"
        `Quick
        (Expect.caught ~tier:"sampled" (check (Sampling.log ~bug:true ~n:254)));
    ] )
