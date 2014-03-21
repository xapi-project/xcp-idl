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

module StringSet = Set.Make(String)
open Stringext

(* Server configuration. We have built-in (hopefully) sensible defaults,
   together with command-line arguments and a configuration file. They
   are applied in order: (latest takes precedence)
      defaults < arguments < config file
*)
let default_service_name = Filename.basename Sys.argv.(0)
let config_file = ref (Printf.sprintf "/etc/%s.conf" default_service_name)
let pidfile = ref (Printf.sprintf "/var/run/%s.pid" default_service_name)
let log_destination = ref "syslog:daemon"
let daemon = ref false
let have_daemonized () = Unix.getppid () = 1

let common_prefix = "org.xen.xcp."

let finally f g =
	try
 		let result = f () in
		g ();
		result
	with e ->
		g ();
		raise e

type opt = string * Arg.spec * (unit -> string) * string

module D = Debug.Make(struct let name = default_service_name end)
open D

module Config_file = struct
	open Arg

	let apply v = function
	| Unit f -> f ()
	| Bool f -> f (bool_of_string v)
	| Set b -> b := (bool_of_string v)
	| Clear b -> b := not (bool_of_string v)
	| String f -> f v
	| Set_string s -> s := v
	| Int f -> f (int_of_string v)
	| Set_int i -> i := (int_of_string v)
	| Float f -> f (float_of_string v)
	| Set_float f -> f := (float_of_string v)
	| _ -> failwith "Unsupported type in config file"

	let parse_line data spec =
		let spec = List.map (fun (a, b, _, _) -> a, b) spec in
		(* Strip comments *)
		match String.split '#' data with
		| [] -> ()
		| x :: _ ->
			begin match Re_str.bounded_split_delim (Re_str.regexp "[ \t]*=[ \t]*") x 2 with
			| key :: v :: [] ->
				(* For values we will accept "v" and 'v' *)
				let v =
					if String.length v < 2
					then v
					else
						let first = v.[0] and last = v.[String.length v - 1] in
						if first = last && (first = '"' || first = '\'')
						then String.sub v 1 (String.length v - 2)
						else v in
				if List.mem_assoc key spec then apply v (List.assoc key spec)
			| _ -> ()
			end

	let parse filename spec =
		(* Remove the unnecessary doc parameter *)
		let ic = open_in filename in
		finally
		(fun () ->
			try
				while true do
					let line = input_line ic in
					parse_line line spec
				done
			with End_of_file -> ()
		) (fun () -> close_in ic)

	let dump spec =
		List.iter (fun (name, _, printer, description) ->
			debug "%s = %s (%s)" name (printer ()) description
		) spec

end

let common_options = [
	"use-switch", Arg.Bool (fun b -> Xcp_client.use_switch := b), (fun () -> string_of_bool !Xcp_client.use_switch), "true if the message switch is to be enabled";
	"switch-port", Arg.Set_int Xcp_client.switch_port, (fun () -> string_of_int !Xcp_client.switch_port), "port on localhost where the message switch is listening";
	"pidfile", Arg.Set_string pidfile, (fun () -> !pidfile), "Filename to write process PID";
	"log", Arg.Set_string log_destination, (fun () -> !log_destination), "Where to write log messages";
	"daemon", Arg.Bool (fun x -> daemon := x), (fun () -> string_of_bool !daemon), "True if we are to daemonise";
	"disable-logging-for", Arg.String
		(fun x ->
			try
				let modules = Re_str.split (Re_str.regexp "[ ]+") x in
				List.iter Debug.disable modules
			with e ->
				error "Processing disabled-logging-for = %s: %s" x (Printexc.to_string e)
		), (fun () -> String.concat " " (List.map fst !Debug.logging_disabled_for)), "A space-separated list of debug modules to suppress logging from";
	"config", Arg.Set_string config_file, (fun () -> !config_file), "Location of configuration file";
]

let arg_spec = List.map (fun (a, b, _, c) -> "-" ^ a, b, c)

type res = {
	name: string;
	description: string;
	essential: bool;
	path: string ref;
	perms: Unix.access_permission list
}

let default_resources = [
]


let canonicalise x =
	if not(Filename.is_relative x)
	then x
	else begin
		(* Search the PATH and XCP_PATH for the executable *)
		let paths = String.split ':' (Sys.getenv "PATH") in
		let xen_paths = try String.split ':' (Sys.getenv "XCP_PATH") with _ -> [] in
		let first_hit = List.fold_left (fun found path -> match found with
			| Some hit -> found
			| None ->
				let possibility = Filename.concat path x in
				if Sys.file_exists possibility
				then Some possibility
				else None
		) None (paths @ xen_paths) in
		match first_hit with
		| None ->
			warn "Failed to find %s on $PATH ( = %s) or $XCP_PATH ( = %s)" x (Sys.getenv "PATH") (try Sys.getenv "XCP_PATH" with Not_found -> "unset");
			x
		| Some hit -> hit
	end

let to_opt = List.map (fun f -> f.name, Arg.String (fun x -> f.path := canonicalise x), (fun () -> !(f.path)), f.description)

let read_config_file x =
	if Sys.file_exists !config_file then begin
		(* Will raise exception if config is mis-formatted. It's up to the
		   caller to inspect and handle the failure.
		*)
		Config_file.parse !config_file x;
	end

let configure ?(options=[]) ?(resources=[]) () =
	let resources = default_resources @ resources in
	let config_spec = common_options @ options @ (to_opt resources) in
	let arg_spec = arg_spec config_spec in
	Arg.parse (Arg.align arg_spec)
		(fun _ -> failwith "Invalid argument")
		(Printf.sprintf "Usage: %s [-config filename]" Sys.argv.(0));
	read_config_file config_spec;
	Config_file.dump config_spec;
	(* Check the required binaries are all available *)
	List.iter
		(fun f ->
			try
				if f.essential
				then Unix.access !(f.path) f.perms
			with _ ->
				error "Cannot access %s: please set %s in %s" !(f.path) f.description !config_file;
				error "For example:";
				error "    # %s" f.description;
				error "    %s=/path/to/%s" f.name f.name;
				exit 1
		) resources;

	Sys.set_signal Sys.sigpipe Sys.Signal_ignore

type 'a handler =
	(string -> Rpc.call) ->
	(Rpc.response -> string) ->
	('a -> Rpc.call -> Rpc.response) ->
	Unix.file_descr ->
	'a->
	unit

(* Apply a binary message framing protocol where the first 16 bytes are an integer length
   stored as an ASCII string *)
let binary_handler call_of_string string_of_response process s context =
	let ic = Unix.in_channel_of_descr s in
	let oc = Unix.out_channel_of_descr s in
	(* Read a 16 byte length encoded as a string *)
	let len_buf = String.make 16 '\000' in
	really_input ic len_buf 0 (String.length len_buf);
	let len = int_of_string len_buf in
	let msg_buf = String.make len '\000' in
	really_input ic msg_buf 0 (String.length msg_buf);
	let (request: Rpc.call) = call_of_string msg_buf in
	let (result: Rpc.response) = process context request in
	let msg_buf = string_of_response result in
	let len_buf = Printf.sprintf "%016d" (String.length msg_buf) in
	output_string oc len_buf;
	output_string oc msg_buf;
	flush oc

let http_handler call_of_string string_of_response process s =
	let ic = Unix.in_channel_of_descr s in
	let oc = Unix.out_channel_of_descr s in
	let module Request = Cohttp.Request.Make(Cohttp_posix_io.Buffered_IO) in
	let module Response = Cohttp.Response.Make(Cohttp_posix_io.Buffered_IO) in
	match Request.read ic with
	| None ->
		debug "Failed to read HTTP request"
	| Some req ->
		begin match Cohttp.Request.meth req, Uri.path (Cohttp.Request.uri req) with
		| `POST, _ ->
			let headers = Cohttp.Request.headers req in
			begin match Cohttp.Header.get headers "content-length" with
			| None ->
				debug "Failed to read content-length"
			| Some content_length ->
				let content_length = int_of_string content_length in
				let request_txt = String.make content_length '\000' in
				really_input ic request_txt 0 content_length;
				let rpc_call = call_of_string request_txt in
				debug "%s" (Rpc.string_of_call rpc_call);
				let rpc_response = process rpc_call in
				debug "   %s" (Rpc.string_of_response rpc_response);
				let response_txt = string_of_response rpc_response in
				let content_length = String.length response_txt in
				let headers = Cohttp.Header.of_list [
					"user-agent", default_service_name;
					"content-length", string_of_int content_length;
				] in
				let response = Cohttp.Response.make ~version:`HTTP_1_1 ~status:`OK ~headers ~encoding:(Cohttp.Transfer.Fixed content_length) () in
				Response.write (fun t oc -> Response.write_body t oc response_txt) response oc
			end
		| _, _ ->
			let content_length = 0 in
			let headers = Cohttp.Header.of_list [
				"user-agent", default_service_name;
				"content-length", string_of_int content_length;
			] in
			let response = Cohttp.Response.make ~version:`HTTP_1_1 ~status:`Not_found ~headers ~encoding:(Cohttp.Transfer.Fixed content_length) () in
			Response.write (fun t oc -> ()) response oc
		end

let ign_thread (t:Thread.t) = ignore t
let ign_int (t:int)         = ignore t
let ign_string (t:string)   = ignore t

let default_raw_fn rpc_fn s =
	http_handler Xmlrpc.call_of_string Xmlrpc.string_of_response rpc_fn s

let accept_forever sock f =
	ign_thread (Thread.create (fun () ->
		while true do
			let this_connection, _ = Unix.accept sock in
			ign_thread (Thread.create (fun c -> finally (fun () -> f c)  (fun () -> Unix.close c)) this_connection)
		done
	) ())

let mkdir_rec dir perm =
	let rec p_mkdir dir =
		let p_name = Filename.dirname dir in
		if p_name <> "/" && p_name <> "." 
		then p_mkdir p_name;
		(try Unix.mkdir dir perm  with Unix.Unix_error(Unix.EEXIST, _, _) -> ()) in
	p_mkdir dir

type server =
  | Socket of Unix.file_descr * (Unix.file_descr -> unit)
  | Queue of string * (Rpc.call -> Rpc.response)

(* Start accepting connections on sockets before we daemonize *)
let make_socket_server path fn =
	try
		(try Unix.unlink path with Unix.Unix_error(Unix.ENOENT, _, _) -> ());
		mkdir_rec (Filename.dirname path) 0o0755;
		let sock = Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
		Unix.bind sock (Unix.ADDR_UNIX path);
		Unix.listen sock 5;
		info "Listening on %s" path;
		Socket (sock, fn)
	with e ->
		error "Failed to listen on Unix domain socket %s. Raw error was: %s" path (Printexc.to_string e);
		begin match e with
		| Unix.Unix_error(Unix.EACCES, _, _) ->
			error "Access was denied.";
			error "Possible fixes include:";
			error "1. Run this program as root (recommended)";
			error "2. Make the permissions in the filesystem more permissive (my effective uid is %d)" (Unix.geteuid ());
			error "3. Adjust the sockets-path directive in %s" !config_file;
			exit 1
		| _ -> ()
		end;
		raise e

let make_queue_server name fn =
	Queue(name, fn) (* TODO: connect to the message switch *)

let make ~path ~queue_name ?raw_fn ~rpc_fn () =
	if !Xcp_client.use_switch
	then make_queue_server queue_name rpc_fn
	else make_socket_server path (match raw_fn with
		| Some x -> x
		| None -> default_raw_fn rpc_fn
		)

let serve_forever = function
	| Socket(listening_sock, fn) ->
		while true do
			let this_connection, _ = Unix.accept listening_sock in
			let (_: Thread.t) = Thread.create
				(fun () ->
					finally
						(fun () -> fn this_connection)
						(fun () -> Unix.close this_connection)
				) () in
			()
		done
	| Queue(queue_name, fn) ->
		let process x = Jsonrpc.string_of_response (fn (Jsonrpc.call_of_string x)) in
		Protocol_unix.Server.listen process !Xcp_client.switch_port queue_name

let pidfile_write filename =
	let fd = Unix.openfile filename
		[ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC; ]
		0o640 in
	finally
	(fun () ->
		let pid = Unix.getpid () in
		let buf = string_of_int pid ^ "\n" in
		let len = String.length buf in
		if Unix.write fd buf 0 len <> len
		then failwith "pidfile_write failed")
	(fun () -> Unix.close fd)


(* Cf Stevens et al, Advanced Programming in the UNIX Environment,
	 Section 13.3 *)
let daemonize () =
	if not (have_daemonized ())
	then
		ign_int (Unix.umask 0);
		match Unix.fork () with
	| 0 ->
		if Unix.setsid () == -1 then failwith "Unix.setsid failed";
		Sys.set_signal Sys.sighup Sys.Signal_ignore;
		(match Unix.fork () with
		| 0 ->
			Unix.chdir "/";
			mkdir_rec (Filename.dirname !pidfile) 0o755;
			pidfile_write !pidfile;
			Unix.close Unix.stdin;
			Unix.close Unix.stdout;
			Unix.close Unix.stderr;
			let nullfd = Unix.openfile "/dev/null" [ Unix.O_RDWR ] 0 in
			assert (nullfd = Unix.stdin);
			let (_:Unix.file_descr) = Unix.dup nullfd in ();
			let (_:Unix.file_descr) = Unix.dup nullfd in ();
	  | _ -> exit 0)
	| _ -> exit 0

let maybe_daemonize () = if !daemon then daemonize ()

let wait_forever () =
	while true do
		try
			Thread.delay 60.
		with e ->
			debug "Thread.delay caught: %s" (Printexc.to_string e)
	done

