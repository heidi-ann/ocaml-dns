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

open Packet

exception InvalidUpdate of string

let check_zonename zonename rr = 
	if not (List.all (fn r -> Name.subzone r.q_name zonename) rr)
	then raise InvalidUpdate "All RRs must be within the zonename"

let zone_class = function
 | [] -> raise InvalidUpdate "Update must contain at least one RR"
 | x::xs -> 
 	let checker rr = 
 		if x.q_class != rr.q_class then raise InvalidUpdate "All RR q_class must be the same" in
 	List.iter checker xs; x.q_class


let create ~id ~zonename ~rr = 
  check_zonename zonename rr;
  let detail = {
    qr=Query; opcode=Update;
    aa=true; tc=false; rd=true; ra=false; rcode=NoError;
  } in
  let zone = { q_name=zonename; q_type=Q_SOA; q_class=zone_class rr; q_unicast=Q_Normal } in
  let prerequisite = [] in
  let update = rr in
  let additionals = [] in
  { 
  	id; detail; questions=[zone];
    answers=prerequisite; authorities=update; additionals;
  }