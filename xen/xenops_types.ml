open Sexplib.Std
open Xcp_pci

type power_state = Running | Halted | Suspended | Paused
[@@deriving sexp, rpcty]

type disk =
  | Local of string  (** path to a local block device *)
  | VDI of string  (** typically "SR/VDI" *)
[@@deriving sexp, rpcty]

module Nvram_uefi_variables = struct
  type onboot = Persist | Reset [@@deriving rpcty, sexp]

  type t =
    {on_boot: onboot [@default Persist]; backend: string [@default "xapidb"]}
  [@@deriving rpcty, sexp]

  let default_t =
    match Rpcmarshal.unmarshal t.Rpc.Types.ty Rpc.(Dict []) with
    | Ok x -> x
    | Error (`Msg m) ->
        failwith
          (Printf.sprintf "Error creating Nvram_uefi_variables.default_t: %s" m)
end

module Vm = struct
  type igd_passthrough = GVT_d [@@deriving rpcty, sexp]

  type video_card =
    | Cirrus
    | Standard_VGA
    | Vgpu
    | IGD_passthrough of igd_passthrough
  [@@default Cirrus] [@@deriving rpcty, sexp]

  type firmware_type = Bios | Uefi of Nvram_uefi_variables.t
  [@@deriving rpcty, sexp]

  let[@deriving rpcty] default_firmware = Bios

  type hvm_info =
    { hap: bool [@default true]
    ; shadow_multiplier: float [@default 1.0]
    ; timeoffset: string [@default ""]
    ; video_mib: int [@default 4]
    ; video: video_card [@default Cirrus]
    ; acpi: bool [@default true]
    ; serial: string option [@default None]
    ; keymap: string option [@default None]
    ; vnc_ip: string option [@default None]
    ; pci_emulations: string list [@default []]
    ; pci_passthrough: bool [@default false]
    ; boot_order: string [@default ""]
    ; qemu_disk_cmdline: bool [@default false]
    ; qemu_stubdom: bool [@default false]
    ; firmware: firmware_type [@default default_firmware] }
  [@@deriving rpcty, sexp]

  type pv_direct_boot =
    { kernel: string [@default ""]
    ; cmdline: string [@default ""]
    ; ramdisk: string option [@default None] }
  [@@deriving rpcty, sexp]

  type pv_indirect_boot =
    { bootloader: string [@default ""]
    ; extra_args: string [@default ""]
    ; legacy_args: string [@default ""]
    ; bootloader_args: string [@default ""]
    ; devices: disk list [@default []] }
  [@@deriving rpcty, sexp]

  type pv_boot = Direct of pv_direct_boot | Indirect of pv_indirect_boot
  [@@deriving rpcty, sexp]

  type pv_info =
    { boot: pv_boot
    ; framebuffer: bool [@default true]
    ; framebuffer_ip: string option [@default None]
    ; vncterm: bool [@default true]
    ; vncterm_ip: string option [@default None] }
  [@@deriving rpcty, sexp]

  type builder_info = HVM of hvm_info | PV of pv_info | PVinPVH of pv_info
  [@@deriving rpcty, sexp]

  type id = string [@@deriving rpcty, sexp]

  type action = Coredump | Shutdown | Start | Pause [@@deriving rpcty, sexp]

  type scheduler_params =
    { priority: (int * int) option
    ; (* weight, cap *)
      affinity: int list list
    (* vcpu -> pcpu list *) }
  [@@deriving rpcty, sexp]

  type t =
    { id: id
    ; name: string [@default "unnamed"]
    ; ssidref: int32
    ; xsdata: (string * string) list
    ; platformdata: (string * string) list
    ; bios_strings: (string * string) list
    ; ty: builder_info
    ; suppress_spurious_page_faults: bool
    ; machine_address_size: int option
    ; memory_static_max: int64
    ; memory_dynamic_max: int64
    ; memory_dynamic_min: int64
    ; vcpu_max: int
    ; (* boot-time maximum *)
      vcpus: int
    ; (* ideal number to use *)
      scheduler_params: scheduler_params
    ; on_crash: action list
    ; on_shutdown: action list
    ; on_reboot: action list
    ; pci_msitranslate: bool
    ; pci_power_mgmt: bool
    ; has_vendor_device: bool [@default false] }
  [@@deriving rpcty, sexp]

  type console_protocol = Rfb | Vt100 [@@deriving rpcty, sexp]

  type console = {protocol: console_protocol; port: int; path: string}
  [@@deriving rpcty, sexp]

  type domain_type =
    | Domain_HVM
    | Domain_PV
    | Domain_PVinPVH
    | Domain_undefined
  [@@deriving rpcty, sexp]

  type state =
    { power_state: power_state
    ; domids: int list
    ; consoles: console list
    ; memory_target: int64
    ; memory_actual: int64
    ; memory_limit: int64
    ; vcpu_target: int
    ; (* actual number of vcpus *)
      shadow_multiplier_target: float
    ; (* actual setting *)
      rtc_timeoffset: string
    ; uncooperative_balloon_driver: bool
    ; guest_agent: (string * string) list
    ; xsdata_state: (string * string) list
    ; pv_drivers_detected: bool
    ; last_start_time: float
    ; hvm: bool
    ; nomigrate: bool
    ; (* true: VM must not migrate *)
      nested_virt: bool
    ; (* true: VM uses nested virtualisation *)
      domain_type: domain_type }
  [@@deriving rpcty, sexp]
end

module Task = struct
  type id = string [@@deriving rpcty]

  type async_result = Rpc.t [@@deriving rpcty]

  type completion_t = {duration: float; result: async_result option}
  [@@deriving rpcty]

  type state = Pending of float | Completed of completion_t | Failed of Rpc.t
  [@@deriving rpcty]

  type t =
    { id: id
    ; dbg: string
    ; ctime: float
    ; state: state
    ; subtasks: (string * state) list
    ; debug_info: (string * string) list
    ; backtrace: string (* An s-expression encoded Backtrace.t *)
    ; cancellable: bool }
  [@@deriving rpcty]

  type t_list = t list [@@deriving rpcty]
end

module Host = struct
  type cpu_info =
    { cpu_count: int
    ; socket_count: int
    ; vendor: string
    ; speed: string
    ; modelname: string
    ; family: string
    ; model: string
    ; stepping: string
    ; flags: string
    ; features: int64 array
    ; features_pv: int64 array
    ; features_hvm: int64 array
    ; features_oldstyle: int64 array }
  [@@deriving rpcty]

  type chipset_info = {iommu: bool; hvm: bool} [@@deriving rpcty]

  type hypervisor = {version: string; capabilities: string} [@@deriving rpcty]

  type t =
    { cpu_info: cpu_info
    ; hypervisor: hypervisor
    ; chipset_info: chipset_info }
  [@@deriving rpcty]

  type guest_agent_feature =
    {name: string; licensed: bool; parameters: (string * string) list}
  [@@deriving rpcty]

  type guest_agent_feature_list = guest_agent_feature list [@@deriving rpcty]
end

module Pci = struct
  include Xcp_pci

  type id = string * string [@@deriving rpcty]

  type t =
    { id: id
    ; position: int
    ; address: address
    ; msitranslate: bool option
    ; power_mgmt: bool option }
  [@@deriving rpcty]

  type state = {plugged: bool} [@@deriving rpcty]
end

type disk_list = disk list

module Vbd = struct
  type mode = ReadOnly | ReadWrite [@@deriving rpcty]

  type ty = CDROM | Disk | Floppy [@@deriving rpcty]

  type id = string * string [@@deriving rpcty]

  (* FIXME: take a URL and call VDI.attach ourselves *)

  type qos_class = Highest | High | Normal | Low | Lowest | Other of int
  [@@deriving rpcty]

  type qos_scheduler = RealTime of qos_class | Idle | BestEffort of qos_class
  [@@deriving rpcty]

  type qos = Ionice of qos_scheduler [@@deriving rpcty]

  type t =
    { id: id [@default "", ""]
    ; position: Device_number.t option [@default None]
    ; mode: mode [@default ReadWrite]
    ; backend: disk option [@default None]
    ; ty: ty [@default Disk]
    ; unpluggable: bool [@default true]
    ; extra_backend_keys: (string * string) list [@default []]
    ; extra_private_keys: (string * string) list [@default []]
    ; qos: qos option [@default None]
    ; persistent: bool [@default true] }
  [@@deriving rpcty]

  type state =
    { active: bool
    ; plugged: bool
    ; qos_target: qos option
    ; backend_present: disk option }
  [@@deriving rpcty]
end

(* XXX: this code shouldn't care about the vswitch/bridge difference *)
module Network = struct
  type t =
    | Local of string  (** Name of a local switch *)
    | Remote of string * string  (** Vm.id * switch *)
    | Sriov of Xcp_pci.address  (** Xcp_pci.address *)
  [@@default Local "xenbr0"] [@@deriving rpcty]

  type ts = t list [@@deriving rpcty]
end

module Vif = struct
  type id = string * string [@@deriving rpcty]

  type ipv4_configuration =
    | Unspecified4
    | Static4 of string list * string option
  [@@deriving rpcty]

  (* a list of CIDRs and optionally a gateway *)

  let default_ipv4_configuration = Unspecified4

  type ipv6_configuration =
    | Unspecified6
    | Static6 of string list * string option
  [@@deriving rpcty]

  (* a list of CIDRs and optionally a gateway *)

  let default_ipv6_configuration = Unspecified6

  type locked_addresses = {ipv4: string list; ipv6: string list}
  [@@deriving rpcty]

  type locking_mode =
    | Unlocked
    (* all traffic permitted *)
    | Disabled
    (* no traffic permitted *)
    | Locked of locked_addresses
  [@@deriving rpcty]

  let default_locking_mode = Unlocked

  module PVS_proxy = struct
    type site = string [@@deriving rpcty]

    type server = {addresses: string list; first_port: int; last_port: int}
    [@@deriving rpcty]

    type interface = string [@@deriving rpcty]

    type t = site * server list * interface [@@deriving rpcty]
  end

  type t =
    { id: id [@default "", ""]
    ; position: int [@default 0]
    ; mac: string [@default "fe:ff:ff:ff:ff:ff"]
    ; carrier: bool [@default true]
    ; mtu: int [@default 1500]
    ; rate: (int64 * int64) option [@default None]
    ; backend: Network.t
    ; other_config: (string * string) list [@default []]
    ; locking_mode: locking_mode [@default default_locking_mode]
    ; extra_private_keys: (string * string) list [@default []]
    ; ipv4_configuration: ipv4_configuration
           [@default default_ipv4_configuration]
    ; ipv6_configuration: ipv6_configuration
           [@default default_ipv6_configuration]
    ; pvs_proxy: PVS_proxy.t option [@default None]
    ; vlan: int64 option [@default None] }
  [@@deriving rpcty]

  type state =
    { active: bool
    ; plugged: bool
    ; kthread_pid: int
    ; media_present: bool
    ; device: string option
    ; pvs_rules_active: bool }
  [@@deriving rpcty]
end

module Vgpu = struct
  type gvt_g =
    { physical_pci_address: address option
    ; (* unused; promoted to Vgpu.t *)
      low_gm_sz: int64
    ; high_gm_sz: int64
    ; fence_sz: int64
    ; monitor_config_file: string option }
  [@@deriving sexp, rpcty]

  type nvidia =
    { physical_pci_address: address option
    ; (* unused; promoted to Vgpu.t *)
      config_file: string }
  [@@deriving sexp, rpcty]

  type mxgpu =
    { physical_function: address option
    ; (* unused; promoted to Vgpu.t *)
      vgpus_per_pgpu: int64
    ; framebufferbytes: int64 }
  [@@deriving sexp, rpcty]

  type implementation =
    | GVT_g of gvt_g
    | Nvidia of nvidia
    | MxGPU of mxgpu
    | Empty
  [@@default Empty] [@@deriving rpcty]

  type id = string * string [@@deriving rpcty]

  let pci_default = Pci.{domain= 0; bus= 0; dev= 0; fn= 0}

  type t =
    { id: id [@default "", ""]
    ; position: int [@default 0]
    ; physical_pci_address: Pci.address [@default pci_default]
    ; implementation: implementation [@default Empty] }
  [@@deriving rpcty]

  let upgrade_pci_info x =
    match x with
    | {implementation= GVT_g {physical_pci_address= Some address; _}; _}
    | {implementation= Nvidia {physical_pci_address= Some address; _}; _}
    | {implementation= MxGPU {physical_function= Some address; _}; _} ->
        {x with physical_pci_address= address}
    | _ -> x

  type state = {active: bool; plugged: bool; emulator_pid: int option} [@@deriving rpcty]
end

module Vusb = struct
  type id = string * string [@@deriving rpcty]

  type t =
    {id: id; hostbus: string; hostport: string; version: string; path: string}
  [@@deriving rpcty]

  type state = {plugged: bool} [@@deriving rpcty]
end

module Metadata = struct
  type t =
    { vm: Vm.t
    ; vbds: Vbd.t list [@default []]
    ; vifs: Vif.t list [@default []]
    ; pcis: Pci.t list [@default []]
    ; vgpus: Vgpu.t list [@default []]
    ; vusbs: Vusb.t list [@default []]
    ; domains: string option [@default None] }
  [@@deriving rpcty]
end

module Dynamic = struct
  type id =
    | Vm of Vm.id
    | Vbd of Vbd.id
    | Vif of Vif.id
    | Pci of Pci.id
    | Vgpu of Vgpu.id
    | Vusb of Vusb.id
    | Task of Task.id
  [@@deriving rpcty]

  type barrier = int * id list [@@deriving rpcty]

  type t =
    | Vm_t of Vm.id * (Vm.t * Vm.state) option
    | Vbd_t of Vbd.id * (Vbd.t * Vbd.state) option
    | Vif_t of Vif.id * (Vif.t * Vif.state) option
    | Pci_t of Pci.id * (Pci.t * Pci.state) option
    | Vgpu_t of Vgpu.id * (Vgpu.t * Vgpu.state) option
    | Vusb_t of Vusb.id * (Vusb.t * Vusb.state) option
    | Task_t of Task.id * Task.t option
  [@@deriving rpcty]

  let rpc_of_id = Rpcmarshal.marshal id.Rpc.Types.ty
end

module DB = struct
  type t =
    { task: Task.t Rpc.Refmap.t
    ; host: Host.t Rpc.Refmap.t
    ; vm: Vm.state Rpc.Refmap.t
    ; pci: Pci.state Rpc.Refmap.t
    ; vbd: Vbd.state Rpc.Refmap.t
    ; vusb: Vusb.state Rpc.Refmap.t
    ; vif: Vif.state Rpc.Refmap.t
    ; vgpu: Vgpu.state Rpc.Refmap.t }
  [@@deriving rpcty]

  let rels = []

  let find_objs : type a.
      a Rpc.Types.cls -> (a Rpc.Refmap.t, t) Rpc.Types.field =
   fun cls ->
    match cls with
    | Task.T -> t_task
    | Host.T -> t_host
    | Vm.STATE -> t_vm
    | Pci.STATE -> t_pci
    | Vbd.STATE -> t_vbd
    | Vusb.STATE -> t_vusb
    | Vif.STATE -> t_vif
    | Vgpu.STATE -> t_vgpu
    | _ -> failwith "Unknown class"

  let typ_of_cls : type a. a Rpc.Types.cls -> a Rpc.Types.typ =
   fun cls ->
    match cls with
    | Task.T -> Task.typ_of
    | Host.T -> Host.typ_of
    | Vm.STATE -> Vm.typ_of_state
    | Pci.STATE -> Pci.typ_of_state
    | Vbd.STATE -> Vbd.typ_of_state
    | Vusb.STATE -> Vusb.typ_of_state
    | Vif.STATE -> Vif.typ_of_state
    | Vgpu.STATE -> Vgpu.typ_of_state
    | _ -> failwith "Unknown cls"

  let empty_db =
    { task= Refmap.empty
    ; host= Refmap.empty
    ; vm= Refmap.empty
    ; pci= Refmap.empty
    ; vbd= Refmap.empty
    ; vusb= Refmap.empty
    ; vif= Refmap.empty
    ; vgpu= Refmap.empty }
end
