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

open Rrd_interface
open Xcp_client

let rec retry_econnrefused f =
  try
    f ()
  with
  | Unix.Unix_error(Unix.ECONNREFUSED, "connect", _) ->
    (* debug "Caught ECONNREFUSED; retrying in 5s"; *)
    Thread.delay 5.;
    retry_econnrefused f
  | e ->
    (* error "Caught %s: does the rrd service need restarting?" (Printexc.to_string e); *)

    raise e

let rpc call =
  retry_econnrefused
    (fun () ->
       (* TODO: the message switch doesn't handle raw HTTP very well *)
       if (* !use_switch *) false
       then json_switch_rpc !queue_name call
       else xml_http_rpc
           ~srcstr:(get_user_agent ())
           ~dststr:"rrd"
           Rrd_interface.uri
           call
    )
module Client = RPC_API(Idl.GenClientExnRpc(struct let rpc=rpc end))
