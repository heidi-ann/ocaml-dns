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

open Name

type time = int32
type t

(** [create n] a new cache to hold maximum of n resource records *)
val create: int -> t

val lookup: t -> time -> Packet.t -> Query.answer option

val add_pkt: t -> time -> Packet.t -> unit
val remove: t -> Packet.question -> unit

(** compress removes expired RR's, automatically called when there isn't enough space to add
	resource records using the add function *)
val compress: t -> time -> unit

(** number of resource records (not number of domain names) *)
val size: t -> int

val to_string: t -> string