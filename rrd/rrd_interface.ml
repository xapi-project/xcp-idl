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

(*
 * The interface of the RRD daemon is defined by the extern function
 * declarations in this file. Implemented by RRD server (separate
 * thread), used by RRD client (part of xapi).
 *)

let service_name = "rrd"
let queue_name = ref (Xcp_service.common_prefix ^ service_name)

let default_sockets_dir = "/var/lib/xcp"
let daemon_name = "xcp-rrdd"
let default_path = ref (Filename.concat default_sockets_dir daemon_name)
let forwarded_path = ref (Filename.concat default_sockets_dir daemon_name ^ ".forwarded")

let set_sockets_dir x =
  default_path := Filename.concat x daemon_name;
  forwarded_path := !default_path ^ ".forwarded"

let uri () = "file:" ^ !default_path

type plugin_protocol = | V1 | V2

type interdomain_uid = {
	name: string;
	frontend_domid: int;
}

type interdomain_info = {
	frequency: Rrd.sampling_frequency;
	shared_page_refs: int list;
}

(* The interface is defined by extern function declarations. *)

external has_vm_rrd : vm_uuid:string -> bool = ""

external push_rrd_local : vm_uuid:string -> domid:int -> unit = ""
external push_rrd_remote : vm_uuid:string -> remote_address:string -> unit = ""
external remove_rrd : uuid:string -> unit = ""
external migrate_rrd : ?session_id:string -> remote_address:string ->
	vm_uuid:string -> host_uuid:string -> unit = ""
external send_host_rrd_to_master : master_address:string -> unit = ""
external backup_rrds : ?remote_address:string option -> unit -> unit = ""
external archive_rrd : vm_uuid:string -> ?remote_address:string option -> unit = ""

external add_host_ds : ds_name:string -> unit = ""
external forget_host_ds : ds_name:string -> unit = ""
external query_possible_host_dss : unit -> Data_source.t list = ""
external query_host_ds : ds_name:string -> float = ""

external add_vm_ds : vm_uuid:string -> domid:int -> ds_name:string -> unit = ""
external forget_vm_ds : vm_uuid:string -> ds_name:string -> unit = ""
external query_possible_vm_dss : vm_uuid:string -> Data_source.t list = ""
external query_vm_ds : vm_uuid:string -> ds_name:string -> float = ""

external update_use_min_max : value:bool -> unit = ""

external update_vm_memory_target : domid:int -> target:int64 -> unit = ""

external set_cache_sr : sr_uuid:string -> unit = ""
external unset_cache_sr : unit -> unit = ""

module Plugin = struct
	external get_header : unit -> string = ""
	external get_path : uid:string -> string = ""

	module Local = struct
		external register : uid:string -> info:Rrd.sampling_frequency ->
			protocol:plugin_protocol -> float = ""
		external deregister : uid:string -> unit = ""
		external next_reading : uid:string -> float = ""
	end

	module Interdomain = struct
		external register : uid:interdomain_uid ->
			info:interdomain_info ->
			protocol:plugin_protocol -> float = ""
		external deregister : uid:interdomain_uid -> unit = ""
		external next_reading : uid:interdomain_uid -> float = ""
	end

	external register : uid:string -> frequency:Rrd.sampling_frequency ->
		float = ""
	external deregister : uid:string -> unit = ""
	external next_reading : uid:string -> float = ""
end

module HA = struct
	external enable_and_update :
		statefile_latencies:Rrd.Statefile_latency.t list ->
		heartbeat_latency:float -> xapi_latency:float -> unit = ""
	external disable : unit -> unit = ""
end

module Deprecated = struct
	(* Could change timescale to sum type, e.g. Slow | Fast.*)
	external load_rrd : uuid:string -> master_address:string -> is_master:bool ->
		timescale:int -> unit = ""
end
