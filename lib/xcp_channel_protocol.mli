(*
 * Copyright (C) Citrix Systems Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)

type t =
  | TCP_proxy of string * int             (** IP, port *)
  | V4V_proxy of int * int                (** domid, port *)
  | Unix_sendmsg of int * string * string (** domid, path, token *)

val rpc_of: t -> Rpc.t
val of_rpc: Rpc.t -> t

