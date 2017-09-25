
open Xcp_client

let json_url () = "file:" ^ Cluster_interface.json_path
let xml_url () = "file:" ^ Cluster_interface.xml_path

let json_http_rpc = http_rpc Jsonrpc.string_of_call Jsonrpc.response_of_string

let rpc url call =
  if !use_switch
  then json_switch_rpc Cluster_interface.queue_name call
  else json_http_rpc ~srcstr:"clusterd" ~dststr:"clusterd" url call

module LocalClient = Cluster_interface.LocalAPI(Idl.GenClient ())
module RemoteClient = Cluster_interface.RemoteAPI(Idl.GenClient ())
