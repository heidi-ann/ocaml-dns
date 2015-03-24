(*
 * Copyright (c) 2015 Heidi Howard <hh360@cam.ac.uk>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

module Store = struct
  include Hashtbl

  let find t k = 
    try Some (Hashtbl.find t k) with | Not_found -> None

end

open Name

type time = int32

type t = { 
  max_size: int;
  mutable curr_size: int; 
  data: (Packet.question, time * Packet.rr list) Store.t;
  }

let min_ttl = Int32.of_int 0
let max_ttl = Int32.of_int 2147483647 (* max 1 week, same as dnscache *)

let create n = {
  max_size = n;
  curr_size = 0;
  data = Store.create (n/5);
  }

let map_filter ~f xs = 
  let rec apply = function
  | [] -> []
  | x::xs -> 
    match f x with 
    | None -> apply xs 
    | Some y -> y :: (apply xs) in
  apply xs

let lookup t time query =
  let open Packet in
  match query.questions with
    | [] -> (* QDCOUNT=0 *) None
    | (qu::_) -> (* assume QDCOUNT=1, ignore extra questions *)
  match (Store.find t.data qu) with
    | None -> None
    | Some (init,rrs) ->
  let check_ttl rr =
    let diff = Int32.sub time init in
    let open Packet in
    if (diff <= rr.ttl) then Some {rr with ttl = Int32.sub rr.ttl diff}
    else None in
  match (map_filter check_ttl rrs) with
    | [] -> None
    | r -> let open Query in
        Some 
        {
        rcode=NoError;
        aa= false;
        answer= r;
        authority= [];
        additional= [];
        }

let filter_rr rr =
  let open Packet in
  match rr.rdata with
    | SOA _ -> false (* do not cache SOA records *)
    | _ -> rr.ttl > min_ttl && rr.ttl < max_ttl

let rec add_pkt t time packet =
  let open Packet in
  match packet.questions with
    | [] -> (* QDCOUNT=0 *) ()
    | (qu::_) -> (* assume QDCOUNT=1, ignore extra questions *)
  add t time qu packet.answers 


and add t time qu rrs =
  let rrs = List.filter filter_rr rrs in
  let if_no_space () = t.curr_size + List.length rrs > t.max_size in
  if if_no_space() then compress t time;
  if if_no_space() then compress_agressive t time (t.max_size - List.length rrs);
  let length = 
    match Store.find t.data qu with
      | Some (_,xs) -> List.length xs 
      | None -> 0 in 
  Store.replace t.data qu (time,rrs);
  t.curr_size <- (t.curr_size - length + (List.length rrs))


and compress t time =
  let open Packet in
  let take_action qu (init,xs) = 
    match List.filter (fun rr -> (Int32.sub time init) <= rr.ttl) xs with
    | [] -> remove t qu
    | rrs -> replace t time qu rrs in
  Store.iter take_action t.data

and remove t qu = 
  let length = 
    match Store.find t.data qu with
      | Some (_,xs) -> List.length xs 
      | None -> 0 in 
  Store.remove t.data qu;
  t.curr_size <- t.curr_size - length

and replace t time qu rrs = 
  remove t qu;
  add t time qu rrs

and compress_agressive t _ new_size = 
  (* currently this naively removes RR's, 
  should be replaced with LRU or similar cache eviction policy *)
  Store.iter (fun k _ -> if new_size<t.curr_size then remove t k) t.data


let size t = t.curr_size

let to_string t = 
  let open Packet in
  let v_to_string = List.fold_left 
    (fun s rr -> Printf.sprintf "%s %s\n" s (Packet.rr_to_string rr)) "" in
  Store.fold (fun k (t,v) s -> 
    Printf.sprintf "%s%s %i\n%s" s (domain_name_to_string k.q_name) (Int32.to_int t) (v_to_string v))
  t.data (Printf.sprintf "Size [RRs] %i/%i\n" t.curr_size t.max_size)
