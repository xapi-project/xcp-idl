(* the build system chooses either 'coverage/enabled.ml' or
 * 'coverage/disabled.ml' as an implementation *)

(** [init name] sets up coverage profiling for binary [name]. You could
 *  use [Sys.argv.(0)] for [name].
 *)

val init: string -> unit

(** [dispatcher_init name] is like [init name] except it also initializes
 * the toplevel coverage API dispatcher on a system.
 * This is meant to be called only by XAPI *)
val dispatcher_init : string -> unit