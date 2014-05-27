(*
 * Copyright (C) 2014 Citrix Inc
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
open OUnit

let test_migrate_receive_memory_call () =
	let open Xenops_interface.Args.VM.Migrate_receive_memory in
	let c = Xcp_channel.t_of_file_descr Unix.(openfile "/dev/null" [O_WRONLY] 0o600) in
	let image_format = !Xenops_interface_upgrades.legacy_suspend_img_fmt in
	let arg_list =
		[ rpc_of___x1__ "dbg";
		  rpc_of___x2__ "vm_id";
		  rpc_of___x3__ 0L;
		  rpc_of___x4__ image_format;
		  rpc_of___x5__ "remote_instance_id";
		  rpc_of___x6__ c ]
	in
	let new_call = Rpc.call "VM.migrate_receive_memory" arg_list in
	(* Remove image_format argument from the RPC (not in legacy call) *)
	let legacy_call = Rpc.call "VM.migrate_receive_memory"
		(List.filter (fun a -> a <> (rpc_of___x4__ image_format)) arg_list)
	in
	Xenops_interface_upgrades.upgrade legacy_call
	|> assert_equal ~msg:"Upgraded call does not match new interface" new_call;
	Xenops_interface_upgrades.upgrade new_call
	|> assert_equal ~msg:"Upgrade not idempotent" new_call

let _ =
	let suite = "channel" >:::
		[
			"test_migrate_receive_memory_call" >:: (test_migrate_receive_memory_call);
		]
	in
	run_test_tt ~verbose:false suite
