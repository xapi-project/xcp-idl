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

(* Generic RPC marshalling functions for XCP services *)

module Request = Cohttp.Request.Make(Cohttp_posix_io.Buffered_IO)
module Response = Cohttp.Response.Make(Cohttp_posix_io.Buffered_IO)

let colon = Re_str.regexp "[:]"

let get_user_agent () = Sys.argv.(0)

let switch_port = ref 8080
let use_switch = ref false

let switch_rpc queue_name string_of_call response_of_string =
	let c = Protocol_unix.Client.connect !switch_port queue_name in
	fun call ->
		response_of_string (Protocol_unix.Client.rpc c (string_of_call call))

(* Use HTTP to frame RPC messages *)
let http_rpc string_of_call response_of_string ?(srcstr="unset") ?(dststr="unset") url call =
	let uri = Uri.of_string (url ()) in
	let req = string_of_call call in

	let headers = Cohttp.Header.of_list [
		"User-agent", get_user_agent ();
		"content-length", string_of_int (String.length req);
	] in
	(* If we have a username:password@ then use basic authentication *)
	let userinfo = Uri.userinfo uri in
	let headers = match userinfo with
		| Some x ->
			begin match Re_str.split_delim colon x with
			| username :: password :: [] ->
				Cohttp.Header.add_authorization headers (Cohttp.Auth.Basic (username, password))
			| _ -> headers
			end
		| None -> headers in
	

	let http_req = Request.make ~meth:`POST ~version:`HTTP_1_1 ~headers uri in

	Open_uri.with_open_uri uri
		(fun fd ->
			let ic = Unix.in_channel_of_descr fd in
			let oc = Unix.out_channel_of_descr fd in
			Request.write (fun t oc -> Request.write_body t oc req) http_req oc;
			match Response.read ic with
				| None -> failwith (Printf.sprintf "Failed to read HTTP response from: %s" (url ()))
				| Some t ->
					begin match Response.status t with
						| `OK ->
							let body = match Response.read_body_chunk t ic with
							| Cohttp.Transfer.Chunk body
							| Cohttp.Transfer.Final_chunk body -> body
							| _ -> "" in
							response_of_string body
						| bad -> failwith (Printf.sprintf "Unexpected HTTP response code: %s" (Cohttp.Code.string_of_status bad))
					end
		)
let xml_http_rpc = http_rpc Xmlrpc.string_of_call Xmlrpc.response_of_string
let json_switch_rpc queue_name = switch_rpc queue_name Jsonrpc.string_of_call Jsonrpc.response_of_string

(* Use a binary 16-byte length to frame RPC messages *)
let binary_rpc string_of_call response_of_string ?(srcstr="unset") ?(dststr="unset") url (call: Rpc.call) : Rpc.response =
	let uri = Uri.of_string (url ()) in
	Open_uri.with_open_uri uri
		(fun fd ->
			let ic = Unix.in_channel_of_descr fd in
			let oc = Unix.out_channel_of_descr fd in
			let msg_buf = string_of_call call in
			let len = Printf.sprintf "%016d" (String.length msg_buf) in
			output_string oc len;
			output_string oc msg_buf;
			flush oc;
			let len_buf = String.make 16 '\000' in
			really_input ic len_buf 0 16;
			let len = int_of_string len_buf in
			let msg_buf = String.make len '\000' in
			really_input ic msg_buf 0 len;
			let (response: Rpc.response) = response_of_string msg_buf in
			response
		)

let json_binary_rpc = binary_rpc Jsonrpc.string_of_call Jsonrpc.response_of_string

