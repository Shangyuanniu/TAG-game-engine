(*
 * CS 3110 Fall 2016 A2
 * Author: Shangyuan Steven Niu
 * NetID: sn522
 *
 * Acknowledge here any contributions made to your solution that
 * did not originate from you or from the course staff: 
 * All ideas are original. Copyright Shangyuan Niu @ Cornell 2016.
 *  - item names/ids should be unique. 
   - its not required that every object has a treasure room. 
   - commands can have white spaces before and after the string body
   - an item can only have one treasure room.
 *)


(* import Yojson.Basic.Util for convenience*)
open Yojson.Basic.Util

(*======== Record Type Definition ========*)

(* You may redefine [state] to be whatever type
 * you wish, but do not change its name. *)
type room = {
              id: string; 
              description : string; 
              points: int;
              exits: (string * string) list; 
              treasure: string list
            }

type item = {
              id : string; 
              description: string; 
              points: int
            }

type state = {
              rooms: room list; 
              current_room: string; 
              items: item list; 
              inv: string list; 
              locations: (string*string) list;
              visited: string list; 
              max_score: int; 
              score: int; 
              turns: int; 
              treasure_locations: (string*string) list
            }

(*======== Exception Definition ========*)

(* [Illegal] is raised by [do'] to indicate that a command is illegal;
 * see the documentation of [do'] below. *)
exception Illegal


(*======== General Helper Functions ========*)

(* General helper function - calculate the sum of all the elements of 
* a int list. *)
let rec sum = function
  |[] -> 0
  |h::t-> h + sum t

(* General helper function - get item/room with a specific id from the 
* item/room list. *)
let rec getitem id (itemlist:item list) = match itemlist with
  |[] -> None
  |h::t-> if h.id = id then Some h else getitem id t

let rec getroom id (roomlist:room list) = match roomlist with
  |[] -> None
  |h::t-> if h.id = id then Some h else getroom id t

(* General helper function - extract element from element option*)
let extract o = match o with
  |Some t -> t
  |None -> raise (Failure "Extraction from None")

(* General helper function - get the value with key from a assoc list*)
let rec assoc_get key = function
  | [] -> None
  | (k',v)::t -> if key=k' then Some v else assoc_get key t



(*======== Helper Functions for init_state and its Sub Functions ========*)

(* Helper function for to_room_record, turns an exit json object into
 * a string*string tuple. *)
let to_exit_tuple exitjson = 
  (
    exitjson |> member "direction" |> to_string,
    exitjson |> member "room_id" |> to_string
  )

(* Helper function for init_state, turns a room json object into
 * room type record. *)
let to_room_record room_json = 
  {
    id = room_json |> member "id" |> to_string;
    description = room_json |> member "description" |> to_string;
    points = room_json |> member "points" |> to_int;
    exits = room_json |> member "exits" |> to_list |> List.map to_exit_tuple;
    treasure = room_json |> member "treasure" |> to_list|> List.map to_string
  }

(* Helper function for init_state, turns an item json object into
 * item type record. *)
let to_item_record item_json = 
  {
    id = item_json |> member "id" |> to_string;
    description = item_json |> member "description" |> to_string;
    points = item_json |> member "points" |> to_int;
  }
 
(* Helper function for init_state, turns a location json object into
 * a string*string tuple. *)
 let to_location_tuple locationjson = (
    locationjson |> member "item" |> to_string,
    locationjson |> member "room" |> to_string
 )

 (* Helper function for build_treasure_loc, turns a room type record into
 * a string*string tuple which map all the treasure room items to 
 * this treasure room. *)
 let rec build_singleroom_tresure treasurelist id = match treasurelist with
  | [] -> []
  | h::t -> (h, id) :: build_singleroom_tresure t id

(* Helper function for init_state, turns a room json object list into
 * a string*string tuple. *)
let rec build_treasure_loc = function
  | [] -> []
  | h::t -> build_singleroom_tresure h.treasure h.id @ build_treasure_loc t

(* Helper for get_items_score, compute the score of a given item if it is in
 * its treasure room. *)

 let rec get_treasure_score itemloc treasurelist itemlist = match itemloc with
  | (item,loc) -> if assoc_get item treasurelist <> None then 
                    if loc = (assoc_get item treasurelist |> extract) then 
                      ((getitem item itemlist) |> extract).points 
                    else 0 
                  else 0

(* Helper function for init_state, compute the initial item score by taking
 * in twp assoc lists. x is the treasure list and y is the current location 
 * list*)
let rec get_items_score x y itemlist= match y with
| [] -> 0
| h::t -> get_treasure_score h x itemlist+ get_items_score x t itemlist

(* General helper function, used to print all list elements*)
let rec print_list = function 
|[] -> ()
| h::[] -> print_string h; print_endline "."
| e::l -> print_string e ; print_string ", " ; print_list l


(*======== Main init_state Function ========*)

(* [init_state j] is the initial state of the game as
 * determined by JSON object [j] *)
let init_state j =
  {
    rooms = j |> member "rooms" |> to_list |> List.map to_room_record;

    current_room = j |> member "start_room" |> to_string;

    items = j |> member "items" |> to_list |> List.map to_item_record; 

    inv = j |> member "start_inv" |> to_list |> List.map to_string;

    locations = j |> member "start_locations" |> to_list 
    |> List.map to_location_tuple;

    visited = (j |> member "start_room" |> to_string) :: [];

    max_score = (j |> member "rooms" |> to_list 
    |> List.map (fun room_json -> (room_json |> member "points" |> to_int))
    |> sum) + (j
    |> member "items" |> to_list 
    |> List.map (fun item_json -> item_json |> member "points" |> to_int)
    |> sum);

    (* An assoc list map item id to the corresponding treasure room id*)
    treasure_locations = j |> member "rooms" |> to_list 
    |> List.map to_room_record|> build_treasure_loc;

    score = 
    (let x = (j |> member "rooms" |> to_list 
    |> List.map to_room_record |> build_treasure_loc) in
    let y = (j |> member "start_locations" |> to_list 
    |> List.map to_location_tuple) in get_items_score x y (
    j |> member "items" |> to_list |> List.map to_item_record
    ))
    + 
    (j|> member "rooms" |> to_list |> List.map to_room_record 
     |> getroom (j |> member "start_room" |> to_string) |> extract).points;

    turns = 0
  }


(*======== Getter Functions ========*)

(* [max_score s] is the maximum score for the adventure whose current
 * state is represented by [s]. *)
let max_score s = s.max_score

(* [score s] is the player's current score. *)
let score s = s.score

(* [turns s] is the number of turns the player has taken so far. *)
let turns s = s.turns

(* [current_room_id s] is the id of the room in which the adventurer
 * currently is. *)
let current_room_id s = s.current_room

(* [inv s] is the list of item id's in the adventurer's current inventory.
 * No item may appear more than once in the list.  Order is irrelevant. *)
let inv s = s.inv

(* [visited s] is the list of id's of rooms the adventurer has visited.
 * No room may appear more than once in the list.  Order is irrelevant. *)
let visited s = s.visited

(* [locations s] is an association list mapping item id's to the
 * id of the room in which they are currently located.  Items
 * in the adventurer's inventory are not located in any room.
 * No item may appear more than once in the list.  The relative order
 * of list elements is irrelevant, but the order of pair components
 * is essential:  it must be [(item id, room id)]. *)
let locations s = s.locations


(*======== Main do' Function ========*)

(* [do' c st] is [st'] if it is possible to do command [c] in
 * state [st] and the resulting new state would be [st'].  The
 * function name [do'] is used because [do] is a reserved keyword.
 *   - The "go" (and its shortcuts), "take" and "drop" commands
 *     either result in a new state, or are not possible because
 *     their object is not valid in state [st] hence they raise [Illegal].
 *       + the object of "go" is valid if it is a direction by which
 *         the current room may be exited
 *       + the object of "take" is valid if it is an item in the
 *         current room
 *       + the object of "drop" is valid if it is an item in the
 *         current inventory
 *       + if no object is provided (i.e., the command is simply
 *         the bare word "go", "take", or "drop") the behavior
 *         is unspecified
 *   - The "quit", "look", "inventory", "inv", "score", and "turns"
 *     commands are always possible and leave the state unchanged.
 *   - The behavior of [do'] is unspecified if the command is
 *     not one of the commands given in the assignment writeup.
 * The underspecification above is in order to enable karma
 * implementations that provide new commands. *)
let do' c st = 
  (* Change state according to user commands*)
  let com = c |> String.trim |> String.lowercase_ascii in 
  let action = if String.length com>=4 then String.sub com 0 4 
    else com in
  if action = "take" then 
    let itemid = (String.length com - 5) |> String.sub com 5 in 
    if (assoc_get itemid st.locations)
    <> None && (assoc_get itemid st.locations |> extract
    = st.current_room) then 
      {
        st with 
        inv = itemid ::st.inv;
        locations = List.filter (fun (x,_) -> x <> itemid) st.locations;
        turns = st.turns + 1;
        score = if (assoc_get itemid st.treasure_locations <> None)
        && (assoc_get itemid st.treasure_locations |> extract
          = st.current_room) then st.score - (getitem itemid st.items
          |>extract).points 
        else st.score;
      }
    else raise Illegal

  else if action = "drop" then
    let itemid = (String.length com - 5) |> String.sub com 5 in
    if List.exists (fun x-> x = itemid) st.inv then 
      {
        st with 
        inv = List.filter (fun x -> x <> itemid) st.inv;
        locations = (itemid, st.current_room)::st.locations;
        turns = st.turns+1;
        score = if (assoc_get itemid st.treasure_locations <> None) && 
        (assoc_get itemid st.treasure_locations |> extract
          = st.current_room) then st.score + (getitem itemid st.items 
          |> extract).points
        else st.score;
      }
    else raise Illegal   


  else if let action = if String.length com >= 2 then
  String.sub com 0 2 else com in action = "go" then
    let direction = String.sub com 3 ((String.length com)-3) in 
    let possible_dir_list = 
    (getroom st.current_room st.rooms|> extract).exits in
    if assoc_get direction possible_dir_list <> None then 
      let newroomid = assoc_get direction possible_dir_list |> extract in
      {
        st with
        current_room = newroomid;
        score = if List.exists (fun x-> x = newroomid) st.visited
        then st.score else st.score + 
        (getroom newroomid st.rooms |> extract).points;
        turns = st.turns +1;
        visited = if List.exists (fun x-> x = newroomid) st.visited 
        then st.visited else newroomid::st.visited
      }
    else raise Illegal

  else if com = "quit" || com = "look" ||com = "inventory" 
  || com = "inv" || com = "score"
    || com = "turns" then st

  else let possible_dir_list = 
  (getroom st.current_room st.rooms|>extract).exits in 
    if assoc_get com possible_dir_list <> None then 
      let newroomid = assoc_get com possible_dir_list |> extract in
      {
        st with
        current_room = newroomid;
        score = if List.exists (fun x-> x = newroomid) st.visited
        then st.score else st.score + 
        (getroom newroomid st.rooms |> extract).points;
        turns = st.turns+1;
        visited = if List.exists (fun x-> x = newroomid) st.visited 
        then st.visited else newroomid::st.visited
      }

  else st


(*======== REPL Function ========*)

(* The interactive intervace. [line] is the raw command, 
 * [st] is the game state.*)
let rec repl line st = 
  let com = line |> String.trim |> String.lowercase_ascii in

  if com = "quit" then (print_endline "Bye!"; st)

  else if com = "look" then 
  (print_endline (getroom st.current_room st.rooms |> extract).description;
    repl (read_line ()) st)

  else if com = "inventory" || com = "inv" then 
    (print_endline "Here are the items in your inventory: ";
    print_list st.inv;
    repl (read_line ()) st)

  else if com = "score" then (st.score|> string_of_int |> print_endline;
    repl (read_line ()) st)

  else if com = "turns" then (st.turns|> string_of_int|>print_endline;
    repl (read_line ()) st)

  else if let action = if String.length com >=4 then String.sub com 0 4 
    else com in (action = "drop" ||
    action = "take") then let evaluate = try do' com st with
      | Illegal -> print_endline "Illgeal item name, please try again!";
      st
    in if evaluate <> st then (print_endline "Success!"; 
      repl (read_line ()) evaluate)
    else repl (read_line ()) st

  else if let action = if String.length com>=2 then String.sub com 0 2 else com
   in action = "go" then let evaluate = try do' com st with
      | Illegal -> print_endline "Illegal direction name, please try again!"; st
      in if evaluate <> st then (print_endline "Success!"; evaluate 
        |> repl (read_line ())) else 
      evaluate |> repl (read_line ())

  else let evaluate = do' com st in if evaluate <> st then 
  (print_endline "Success!";
    repl (read_line ()) evaluate) else 
    (print_endline "Sorry I don't understand your command. Please try again!"; 
    repl (read_line ()) st)

(*======== Main Trigger Function ========*)

(* [main f] is the main entry point from outside this module
 * to load a game from file [f] and start playing it
 *)
let main file_name = 
  (* Read the Json file and initialize all the relevant field to its initial value. *)
  let state = Yojson.Basic.from_file file_name
  |> init_state in let run = 
  (print_endline (getroom state.current_room state.rooms |> extract).description;
    repl (read_line ()) state) in ()

