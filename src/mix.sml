(* Front-end driver *)

signature MIX =
sig
  val mix : string * string list -> OS.Process.status
end

structure Mix :> MIX =
struct
  fun mix (arg0, argv) = (
    print "Hello, world!\n";
    OS.Process.success
  )

    val _ = SMLofNJ.exportFn("_build/mix", mix)
end
