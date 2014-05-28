(*
 * Copyright (C) 2014 Citrix Systems Inc.
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
(**
 * @group Xenops
 *)

let legacy_suspend_img_fmt = ref 1

let upgrade (call: Rpc.call) : Rpc.call =
	match (call.Rpc.name, call.Rpc.params) with
	| ("VM.migrate_receive_memory", [dbg; id; memory_limit; remote_instance_id; c]) ->
		let image_format = Rpc.Int (Int64.of_int !legacy_suspend_img_fmt) in
		{call with Rpc.params = [dbg; id; memory_limit; image_format; remote_instance_id; c]}
	| _ -> call
