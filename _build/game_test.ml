open OUnit2
open Game

let j = Yojson.Basic.from_file "tworoom.json"

let state = j |> init_state
let state2 = do' "take black hat" state
let state3 = do' "go north" state

let item1 = {id = "item1"; description = "test1"; points = 1}
let item2 = {id = "item2"; description = "test2"; points = -4}
let item3 = {id = "item3"; description = "test3"; points = 4}

let room1 = {id = "room1"; description = "room1"; points = 10; 
              exits = [("room2","underground")]; treasure = ["item1"]}

let room2 = {id = "room2"; description = "room2"; points = 100; 
              exits = [("room1","above");("room3","north")]; 
              treasure = ["item2";"item3"]}

let room3 = {id = "room3"; description = "room3"; points = 50; 
              exits = []; treasure = []}

let assoc = [("room1","above");("room3","north")]

let tests =
[ 
  (*======== Tests for General Helper Functions ========*)
  "test for sum 1" >:: (fun _ -> assert_equal 0 (sum []));
  "test for sum 2" >:: (fun _ -> assert_equal 15 ([1;2;3;4;5]|> sum));
  "test for getitem 1" >:: (fun _ -> assert_equal None (getitem "item1" [item2]));
  "test for getitem 2" >:: 
  (fun _ -> assert_equal (Some item2) (getitem "item2" [item1;item2;item3]));
  "test for getroom 1" >:: (fun _ -> assert_equal None (getroom "room1" []));
  "test for getroom 2" >:: 
  (fun _ -> assert_equal (Some room1) (getroom "room1" [room2;room1;room3]));
  "test for extract 1" >:: 
  (fun _ -> assert_raises (Failure "Extraction from None") (fun () -> extract None));
  "test for extract 2" >:: 
  (fun _ -> assert_equal item2 (extract (Some item2)));
  "test for assoc_Get 1" >:: (fun _ -> assert_equal None (assoc_get "room2" assoc));
  "test for assoc_get 2" >:: 
  (fun _ -> assert_equal (Some "above") (assoc_get "room1" assoc));

  (*======== Tests for init_state Functions ========*)
  "max" >:: (fun _ -> assert_equal 11111 (j |> init_state |> max_score));
  "initial turns" >:: (fun _ -> assert_equal 0 (j |> init_state |> turns));
  "initial score" >:: (fun _ -> assert_equal 10001 (j |> init_state |> score));
  "current room" >:: (fun _ -> assert_equal "room1" (j |> init_state |> current_room_id));
  "inventory" >:: (fun _ -> assert_equal ["white hat"] (j |> init_state |> inv));
  "location" >:: 
  (fun _ -> assert_equal [("black hat","room1");("red hat","room1")] (j |> init_state |> locations));
  "visited" >:: (fun _ -> assert_equal ["room1"] (j |> init_state |> visited));
  "room list" >:: 
  (fun _ -> assert_equal [{id = "room1";
    description= "This is Room 1.  There is an exit to the north.\nYou should drop the white hat here.";
    points = 1;
    exits = [("north","room2")];
    treasure = ["white hat";"red hat"]};
    {id="room2";
    description = "This is Room 2.  There is an exit to the south.\nYou should drop the black hat here.";
    points = 10;
    exits = [("south","room1")];
    treasure =["black hat"]}] ((j |> init_state).rooms ));
  "item list" >:: (fun _ -> assert_equal [
    {
    id = "black hat";
    description = "a black fedora";
    points = 100
  }; {
    id = "white hat";
    description = "a white panama";
    points = 1000
  }; {
    id = "red hat";
    description = "a red fez";
    points = 10000
  }
    ] ((j |> init_state).items)); 
  "treasure location assoc list" >:: 
  (fun _ -> assert_equal [("white hat","room1");("red hat","room1");("black hat","room2")] 
    ((j |> init_state).treasure_locations));
  
  (*======== Tests for do' Functions ========*)
  "do' quit" >:: (fun _ -> assert_equal state (do' "quit" state));
  "do' inv" >:: (fun _ -> assert_equal state (do' "inv" state));
  "do' inventory" >:: (fun _ -> assert_equal state (do' "inventory" state));
  "do' look" >:: (fun _ -> assert_equal state (do' "look" state));
  "do' score" >:: (fun _ -> assert_equal state (do' "score" state));
  "do' turns" >:: (fun _ -> assert_equal state (do' "turns" state));

  (* take *)
  "do' take non exist item" >:: 
  (fun _ -> assert_raises (Illegal) (fun () -> do' "take asdf" state));
  "do' take item already in inv" >:: 
  (fun _ -> assert_raises (Illegal) (fun () -> do' "take white hat" state));
   "do' take item from another room" >:: 
  (fun _ -> assert_raises (Illegal) (fun () -> do' "take black hat" state3));
  "do' take correct item inv" >:: 
  (fun _ -> assert_equal ["red hat";"white hat"] (do' "taKe red hat" state |> inv));
  "do' take correct item turn" >:: 
  (fun _ -> assert_equal 1 (do' "take red hat" state |> turns));
  "do' take correct item location" >:: 
  (fun _ -> assert_equal [("black hat","room1")] (do' "take rEd hat" state |> locations));
  "do' take correct item away from treasure room score" >:: 
  (fun _ -> assert_equal 1 (do' "take red hat" state |> score));
  "do' take correct item away from non treasure room score unchanged" >:: 
  (fun _ -> assert_equal 10001 (do' "take black hat" state |> score));

  (* drop *)
  "do' drop non exist item" >:: 
  (fun _ -> assert_raises (Illegal) (fun () -> do' "drop asdf" state));
  "do' drop item not in inv" >:: 
  (fun _ -> assert_raises (Illegal) (fun () -> do' "drop red hat" state));
  "do' drop correct item inv" >:: 
  (fun _ -> assert_equal [] (do' "drop white hat" state |> inv));
  "do' drop correct item turn" >:: 
  (fun _ -> assert_equal 1 (do' "drop white hat" state |> turns));
  "do' drop correct item location" >:: 
  (fun _ -> assert_equal [("white hat", "room1");("black hat","room1");("red hat","room1")] 
    (do' "drop white hat" state |> locations));
  "do' drop correct item into treasure room score" >:: 
  (fun _ -> assert_equal 11001 (do' "drop white hat" state |> score));
  "do' drop correct item into non treasure room score unchanged" >:: 
  (fun _ -> assert_equal 10001 (do' "drop black hat" state2 |> score));


  (* go *)
  "do' go to non exist direction" >:: 
  (fun _ -> assert_raises (Illegal) (fun () -> do' "go asdf" state));
  "do' go to correct direction current_room" >:: 
  (fun _ -> assert_equal "room2" (do' "go north" state |> current_room_id));
  "do' go to correct direction turn" >:: 
  (fun _ -> assert_equal 1 (do' "go north" state |> turns));
  "do' go to correct room 1st time visited" >:: 
  (fun _ -> assert_equal ["room2";"room1"] 
    (do' "go north" state |> visited));
  "do' go to new room score" >:: 
  (fun _ -> assert_equal 10011 (do' "go north" state |> score));
  "do' revisit room score unchanged" >:: 
  (fun _ -> assert_equal 10011 (do' "go south" state3 |> score));
 
 (* short hand go *)
 "do' short hand go to non exist direction, treat as invalid command, return unchanged state" >:: 
  (fun _ -> assert_equal state (do' "east" state));
  "do' short hand go to correct direction current_room" >:: 
  (fun _ -> assert_equal "room2" (do' "north" state |> current_room_id));
  "do' short hand go to correct direction turn" >:: 
  (fun _ -> assert_equal 1 (do' "north" state |> turns));
  "do' short hand go to correct room 1st time visited" >:: 
  (fun _ -> assert_equal ["room2";"room1"] 
    (do' "north" state |> visited));
  "do' short hand go to new room score" >:: 
  (fun _ -> assert_equal 10011 (do' "north" state |> score));
  "do' short hand revisit room score unchanged" >:: 
  (fun _ -> assert_equal 10011 (do' "south" state3 |> score));
]


(* no test for repl, since it returns a unit -tested using interactive shell.*)

let suite =
  "Adventure test suite"
  >::: tests

let _ = run_test_tt_main suite
