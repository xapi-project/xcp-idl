(* Cluster interface *)

open Idl

let service_name = "cluster"
let queue_name = Xcp_service.common_prefix ^ service_name
let json_path = "/var/xapi/cluster.json"
let xml_path = "/var/xapi/cluster"

type cluster_name = string
[@@doc ["Name of the cluster"]]
[@@deriving rpcty]

type address = IPv4 of string
[@doc ["An IP address"]]
[@@deriving rpcty]
let printaddr () = function | IPv4 s -> Printf.sprintf "IPv4(%s)" s
let str_of_address address = match address with IPv4 a -> a

type addresslist = address list [@@deriving rpcty]

type nodeid = int32 [@@deriving rpcty]

let string_of_nodeid = Int32.to_string

type node = {
  addr: address;
  id: nodeid;
}
[@@doc
  ["This type describes an individual node in the cluster. It must have";
   "a unique identity (an int32), and may have multiple IP addresses on";
   "which it can be contacted."]]
[@@deriving rpcty]


type cluster_config = {
  cluster_name : string;
  all_members : node list; (* each host can have multiple addresses *)
  enabled_members : node list;
  authkey: string;
}
[@@doc
  ["This type contains all of the information required to configure";
   "the cluster. This includes all details required for the corosync";
   "configuration as well as anything else required for pacemaker and";
   "SBD. All nodes have a local copy of this and we take pains to";
   "ensure it is kept in sync."]]
[@@deriving rpcty]

type diagnostics = {
  cluster_config : cluster_config option;
  node_id : nodeid option;
  token : string option;
  num_times_booted : int;
  is_quorate : bool;
}
[@@doc
 [ "This type contains diagnostic information about the current state";
   "of the cluster daemon. All state required for test purposes should";
   "be in this type."]]
[@@deriving rpcty]

type token = string
[@@doc ["This secret token is used to authenticate remote API calls on a cluster"]]
[@@deriving rpcty]

let token_p = Param.mk ~name:"token" token

type error =
  | InternalError of string
  | Unix_error of string
[@@deriving rpcty]

module E = Error.Make(struct
    type t = error [@@deriving rpcty]
  end)
let err = E.error

type named_unit = unit [@@deriving rpcty]
type my_string = string [@@deriving rpcty]

let unit_p         = Param.mk ~name:"unit" ~description:["unit"] named_unit
let string_p       = Param.mk ~name:"string" ~description:["string"] my_string
let address_p      = Param.mk ~name:"address" ~description:[
    "IP address of a cluster member";
  ] address
let local_ip_p     = Param.mk ~name:"local_ip" ~description:[
    "The local address of the cluster member";
  ] address

type remove = bool [@@deriving rpcty]

module LocalAPI(R:RPC) = struct
  open R

  let description = Interface.{
      name = "Local";
      namespace = None;
      description = [
        "Local Cluster APIs. These are intended to be used to control the xapi-clusterd service";
        "There is no authentication on these, but they are only available on the local machine.";
      ];
      version = (1,0,0);
    }

  let implementation = implement description

  let create = declare
      "create"
      ["Creates the cluster. The call takes the local IP addresses of";
       "the initial host to add to the cluster. This will be the";
       "address on which the rings will be created."]
      (local_ip_p @-> returning token_p err)

  let destroy = declare
      "destroy"
      ["Destroys a created cluster"]
      (unit_p @-> returning unit_p err)

  let leave = declare
      "leave"
      ["Causes this host to permanently leave the cluster, but leaves the rest of the cluster";
       "enabled. This is not a temporary removal - if the admin wants the hosts to rejoin the cluster again,";
       "he will have to call `join` rather than `enable`."]
      (unit_p @-> returning unit_p err)

  let disable = declare
      "disable"
      ["Stop the cluster on this host; leave the rest of the cluster";
       "enabled. The cluster can be reenabled either by restarting the";
       "host, or by calling the `enable` API call."]
      (unit_p @-> returning unit_p err)

  let enable =
    declare
      "enable"
      ["Rejoins the cluster following a call to `disable`. The parameter";
      "passed is the new local address to use in case it changed while the";
      "host was disabled."]
      (local_ip_p @-> returning unit_p err)

  let join =
    let new_p = Param.mk ~name:"new_member" address in
    let existing_p = Param.mk ~name:"existing_members" addresslist in
    declare
      "join"
      ["Adds a node to an initialised cluster. Takes the IP address of";
       "the new member and a list of the addresses of all the existing";
       "members."]
      (token_p @-> new_p @-> existing_p @-> returning unit_p err)


  let declare_dead =
    let dead_members_p = Param.mk ~name:"dead_members" addresslist in
    declare
      "declare-dead"
      ["Declare that some hosts in the cluster are permanently dead. Removes";
       "the hosts from the cluster. If the hosts do attempt to rejoin the";
       "cluster in future, this may lead to fencing of other hosts and/or";
       "data loss or data corruption."]
      (dead_members_p @-> returning unit_p err)

  let diagnostics =
    let diagnostics_p = Param.mk ~name:"diagnostics" diagnostics in
    declare
      "diagnostics"
      ["Returns diagnostic information about the cluster"]
      (unit_p @-> returning diagnostics_p err)

  (*  let node_remove = declare
      "node_remove"
      ["Cooperatively removes a node from the cluster"]
      (cluster_id_p @-> address_p @-> returning unit_p err)*)

end

module RemoteAPI(R:RPC) = struct
  open R

  let description = Interface.{
      name = "Remote Cluster";
      namespace = None;
      description = [
        "These API functions are called between hosts during various operations on a cluster";
        "They must all be protected with a token on each and every API call to prevent";
        "malicious other entities from disrupting the cluster";
      ];
      version = (1,0,0);
    }

  let implementation = implement description

  let nodeid_p = Param.mk ~name:"nodeid" nodeid
  let cluster_config_p = Param.mk ~name:"cluster_config" cluster_config

  let join =
    declare
      "join"
      ["Internal API to join a new member to an existing cluster."]
      (token_p @-> address_p @-> returning cluster_config_p err)

  let rejoin =
    declare
      "rejoin"
      ["Internal API to rejoin a member already known to a cluster. The member";
       "may want to return using a different IP address but must always return";
       "with the same node ID."]
      (token_p @-> nodeid_p @-> address_p @-> returning cluster_config_p err)

  let eject =
    let remove_p = Param.mk ~name:"remove" remove in
    declare
      "eject"
      ["Internal API to remove a member from an existing cluster.";
       "If remove=true then this node will be removed from our memory."]
      (remove_p @-> token_p @-> nodeid_p @-> returning unit_p err)

  let config =
    declare
      "config"
      ["Internal API to persist the cluster config on the host."]
      (token_p @-> cluster_config_p @-> returning unit_p err)

end
