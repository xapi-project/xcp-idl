open OUnit

let assert_equal_list a b = assert_equal (List.sort compare a) (List.sort compare b)

let test_debug_enable_disable () =
	Debug.disable "xapi" ~level:Syslog.Info;
	assert_equal (Debug.is_disabled "xapi" Syslog.Info) true;

	Debug.enable "xapi" ~level:Syslog.Info;
	assert_equal (Debug.is_disabled "xapi" Syslog.Info) false

let test_debug_set_level () =
	Debug.set_level ~brand:"xapi" Syslog.Err;
	assert_equal (Debug.is_disabled "xapi" Syslog.Err) false;
	assert_equal (Debug.is_disabled "xapi" Syslog.Warning) true;
	assert_equal (Debug.is_disabled "xapi" Syslog.Info) true;
	assert_equal (Debug.is_disabled "xapi" Syslog.Debug) true;

	Debug.set_level ~brand:"xapi" Syslog.Info;
	assert_equal (Debug.is_disabled "xapi" Syslog.Err) false;
	assert_equal (Debug.is_disabled "xapi" Syslog.Warning) false;
	assert_equal (Debug.is_disabled "xapi" Syslog.Info) false;
	assert_equal (Debug.is_disabled "xapi" Syslog.Debug) true;

	Debug.set_level ~brand:"xapi" Syslog.Debug;
	Debug.set_level ~brand:"xenopsd" Syslog.Err;

	assert_equal (Debug.is_disabled "xapi" Syslog.Err) false;
	assert_equal (Debug.is_disabled "xapi" Syslog.Warning) false;
	assert_equal (Debug.is_disabled "xapi" Syslog.Info) false;
	assert_equal (Debug.is_disabled "xapi" Syslog.Debug) false;

	assert_equal (Debug.is_disabled "xenopsd" Syslog.Err) false;
	assert_equal (Debug.is_disabled "xenopsd" Syslog.Warning) true;
	assert_equal (Debug.is_disabled "xenopsd" Syslog.Info) true;
	assert_equal (Debug.is_disabled "xenopsd" Syslog.Debug) true

let test_debug_set_level_multiple_loggers () = 
	let a = (module Debug.Make(struct let name = "aaaa" end) : Debug.DEBUG) in
	let b = (module Debug.Make(struct let name = "bbbb" end) : Debug.DEBUG) in
        
	(* Set level on aaaa - bbbb should not be affected *)
	Debug.set_level ~brand:"aaaa" Syslog.Err;
	assert_equal (Debug.is_disabled "aaaa" Syslog.Err) false;
	assert_equal (Debug.is_disabled "aaaa" Syslog.Warning) true;
	assert_equal (Debug.is_disabled "aaaa" Syslog.Info) true;
	assert_equal (Debug.is_disabled "aaaa" Syslog.Debug) true;

	assert_equal (Debug.is_disabled "bbbb" Syslog.Err) false;
	assert_equal (Debug.is_disabled "bbbb" Syslog.Warning) false;
	assert_equal (Debug.is_disabled "bbbb" Syslog.Info) false;
	assert_equal (Debug.is_disabled "bbbb" Syslog.Debug) false;

	(* Set level on all declared loggers *)
	Debug.set_level Syslog.Warning;
	assert_equal (Debug.is_disabled "aaaa" Syslog.Err) false;
	assert_equal (Debug.is_disabled "aaaa" Syslog.Warning) false;
	assert_equal (Debug.is_disabled "aaaa" Syslog.Info) true;
	assert_equal (Debug.is_disabled "aaaa" Syslog.Debug) true;

	assert_equal (Debug.is_disabled "bbbb" Syslog.Err) false;
	assert_equal (Debug.is_disabled "bbbb" Syslog.Warning) false;
	assert_equal (Debug.is_disabled "bbbb" Syslog.Info) true;
	assert_equal (Debug.is_disabled "bbbb" Syslog.Debug) true 

let tests =
  "debug" >:::
    [
      "Test Debug.enable / disable" >:: test_debug_enable_disable; 
      "Test Debug.set_level" >:: test_debug_set_level;
      "Test Debug.set_level (multiple loggers)">:: test_debug_set_level_multiple_loggers;
    ]
