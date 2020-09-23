include Lru.M.Make (struct include String let hash = Hashtbl.hash end) (struct type t = int let weight _ = 1 end)
