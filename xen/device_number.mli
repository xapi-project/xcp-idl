(** Disks are attached to particular bus types: *)
type bus_type =
  | Xen  (** A xen paravirtualised bus *)
  | Scsi  (** A SCSI bus *)
  | Floppy  (** A floppy bus *)
  | Ide  (** An IDE bus *)

(** A specification for a device number. There are more valid specifications
    than valid device numbers because of hardware and/or protocol limits. *)
type spec = bus_type * int * int

(** A valid device number *)
type t

val typ_of : t Rpc.Types.typ

val make : spec -> t
(** [make spec] validates a given device number specification [spec] and returns
    a device number *)

val spec : t -> spec
(** [spec t] takes a [t] and returns the corresponding [spec] *)

val of_string : bool -> string -> t
(** [of_string hvm name] returns the interface which best matches the [name] by
    applying the policy: first check if it is a disk_number, else fall back to a
    linux_device for backwards compatability *)

val to_debug_string : t -> string
(** [to_debug_string i] returns a pretty-printed interface *)

val to_linux_device : t -> string
(** [to_linux_device i] returns a possible linux string representation of
    interface [i] *)

val of_linux_device : string -> t
(** [of_linux_device x] returns the interface corresponding to string [x] *)

val upgrade_linux_device : string -> string
(** [upgrade_linux_device x] upgrades hd* style device names to xvd* and leaves
    all other device names unchanged. *)

type xenstore_key = int

val to_xenstore_key : t -> xenstore_key
(** [to_xenstore_key i] returns the xenstore key from interface [i] *)

val of_xenstore_key : xenstore_key -> t
(** [of_xenstore_key key] returns an interface from a xenstore key *)

type disk_number = int

val to_disk_number : t -> disk_number
(** [to_disk_number i] returns the corresponding non-negative disk number *)

val of_disk_number : bool -> disk_number -> t
(** [of_disk_number hvm n] returns the interface corresponding to disk number
    [n] which depends on whether the guest is [hvm] or not. *)
