open Device_number

let device_number =
  Alcotest.testable (Fmt.of_to_string Device_number.to_debug_string) ( = )

(* spec * linux string * xenstore key *)
let examples =
  [
    ((Xen, 0, 0), "xvda", 51712)
  ; ((Xen, 0, 1), "xvda1", 51713)
  ; ((Ide, 0, 0), "hda", 768)
  ; ((Ide, 0, 1), "hda1", 769)
  ; ((Scsi, 0, 0), "sda", 2048)
  ; ((Scsi, 0, 1), "sda1", 2049)
  ; ((Scsi, 1, 3), "sdb3", 2067)
  ; ((Ide, 2, 2), "hdc2", 5634)
  ; ((Xen, 26, 0), "xvdaa", 268442112)
  ]

let deprecated =
  [
    ((Ide, 4, 0), "hde", 8448)
  ; ((Ide, 5, 0), "hdf", 8512)
  ; ((Ide, 6, 0), "hdg", 8704)
  ; ((Ide, 7, 0), "hdh", 8768)
  ; ((Ide, 8, 0), "hdi", 14336)
  ; ((Ide, 15, 0), "hdp", 22848)
  ]

let examples_to_test =
  let using_deprecated_ide =
    try
      ignore (make (Ide, 4, 0)) ;
      true
    with _ -> false
  in
  examples @ if using_deprecated_ide then deprecated else []

let equivalent =
  [
    ("d0", "xvda")
  ; ("d0", "0")
  ; ("d5", "5")
  ; ("xvdf", "5")
  ; ("d0p0", "xvda")
  ; ("d536p37", "xvdtq37")
  ]

let test_examples =
  let tests =
    List.map
      (fun (spec, linux, xenstore) ->
        ( "test_examples " ^ linux
        , `Quick
        , fun () ->
            let of_spec = make spec in
            let of_linux = of_linux_device linux in
            let of_xenstore = of_xenstore_key xenstore in
            Alcotest.check device_number "examples must be equal" of_spec
              of_linux ;
            Alcotest.check device_number "examples must be equal" of_spec
              of_xenstore ))
      examples_to_test
  in
  tests

(* NB we always understand the deprecated linux/xenstore devices even if we
   don't generate them ourselves *)
let test_deprecated =
  let tests =
    List.map
      (fun (_, linux, xenstore) ->
        ( "test_deprecated " ^ linux
        , `Quick
        , fun () ->
            let of_linux = of_linux_device linux in
            let of_xenstore = of_xenstore_key xenstore in
            Alcotest.check device_number "must be equal" of_linux of_xenstore ))
      deprecated
  in
  tests

let test_equivalent =
  let tests =
    List.map
      (fun (x, y) ->
        let test_name = Printf.sprintf "test_equivalent %s=%s" x y in
        ( test_name
        , `Quick
        , fun () ->
            let x' = of_string false x in
            let y' = of_string false y in
            Alcotest.check device_number "must be equal" x' y' ))
      equivalent
  in
  tests

let test_2_way_convert =
  (* We now always convert Ide specs into xvd* linux devices, so they become Xen
     specs when converted back. *)
  let equal_linux old_t new_t =
    match (spec old_t, spec new_t) with
    | (Ide, disk1, partition1), (Xen, disk2, partition2)
      when disk1 = disk2 && partition1 = partition2 ->
        true
    | old_spec, new_spec ->
        old_spec = new_spec
  in
  let device_number_equal_linux =
    Alcotest.testable
      (Fmt.of_to_string Device_number.to_debug_string)
      equal_linux
  in
  [
    ( "test_2_way_convert"
    , `Slow
    , fun () ->
        for disk_number = 0 to (1 lsl 20) - 1 do
          List.iter
            (fun hvm ->
              let original = of_disk_number hvm disk_number in
              let of_linux = of_linux_device (to_linux_device original) in
              let of_xenstore = of_xenstore_key (to_xenstore_key original) in
              Alcotest.check device_number_equal_linux
                "of_linux must be equal to original" original of_linux ;
              Alcotest.check device_number
                "of_xenstore must be equal to original" original of_xenstore)
            [true; false]
        done )
  ]

let tests =
  test_examples @ test_deprecated @ test_equivalent @ test_2_way_convert
