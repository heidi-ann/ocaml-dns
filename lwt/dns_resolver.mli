(*
 * Copyright (c) 2012 Richard Mortier <mort@cantab.net>
 * Copyright (c) 2014 Anil Madhavapeddy <anil@recoil.org>
 * Copyright (c) 2014 David Sheets <sheets@alum.mit.edu>
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

type commfn = {
  txfn    : Dns.Buf.t -> unit Lwt.t;
  rxfn    : (Dns.Buf.t -> Dns.Packet.t option) -> Dns.Packet.t Lwt.t;
  timerfn : unit -> unit Lwt.t;
  cleanfn : unit -> unit Lwt.t;
}

val resolve : 
  (module Dns.Protocol.CLIENT) ->
  ?alloc:(unit -> Dns.Buf.t) ->
  ?dnssec:bool ->
  commfn -> Dns.Packet.q_class -> 
  Dns.Packet.q_type -> 
  Dns.Name.domain_name -> 
  Dns.Packet.t Lwt.t

(** A less abstract version of resolve which allows the user to form there own DNS packet to resolve *)
val resolve_packet : 
  (module Dns.Protocol.CLIENT) ->
  ?alloc:(unit -> Dns.Buf.t) ->
  commfn -> Dns.Packet.t -> 
  Dns.Packet.t Lwt.t

val gethostbyname :
  ?alloc:(unit -> Dns.Buf.t) ->
  ?q_class:Dns.Packet.q_class ->
  ?q_type:Dns.Packet.q_type -> commfn ->
  string -> Ipaddr.t list Lwt.t

val gethostbyaddr :
  ?alloc:(unit -> Dns.Buf.t) ->
  ?q_class:Dns.Packet.q_class ->
  ?q_type:Dns.Packet.q_type -> commfn ->
  Ipaddr.V4.t -> string list Lwt.t
