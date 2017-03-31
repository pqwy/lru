module M_as_F (K: Hashtbl.HashedType) (V: Lru.Weighted):
  Lru.F.S with type k = K.t and type v = V.t =
struct

  let nope name _ =
    invalid_arg @@ Format.sprintf "M_as_F.%s: not implemented" name

  include Lru.M.Make (K) (V)

  let unadd   = nope "unadd"
  let pop_lru = nope "pop_lru"

  let empty n = create n

  let find ?promote k t =
    match find ?promote k t with Some v -> Some (v, t) | _ -> None

  let retaining f t = f t; t

  let trim = retaining trim
  let resize cap = retaining @@ resize cap
  let add ?trim k v = retaining @@ add ?trim k v
  let remove k = retaining @@ remove k
  let drop_lru = retaining drop_lru

  let fold f s t = fold f s t
  let iter f t = iter f t
  let to_list t = to_list t
end
