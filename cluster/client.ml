let json_url () = "file:" ^ Interface.json_path

let json_http_rpc = Xcp_client.http_rpc Jsonrpc.string_of_call Jsonrpc.response_of_string

let rpc url call =
  if !Xcp_client.use_switch
  then Xcp_client.json_switch_rpc Interface.queue_name call
  else json_http_rpc ~srcstr:"clusterd" ~dststr:"clusterd" url call

module LocalClient = Interface.LocalAPI(Idl.GenClient ())
