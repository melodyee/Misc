let i x = x
let k x y = x
let s x y z = x z (y z)
let ki x = k i x

let () =
  let t = k in
  let f = ki in
  let or__ a b = a t (b t f) in
  let and__ a b = a (b t f) f in
  let or_ = t in
  let and_ = f in
  let not g = g f t in
  Show.print (t 1 2);
  Show.print (f 1 2);
  Show.print (not t 1 2);
  Show.print (not f 1 2);
  Show.print (and__ t t 1 2);
  Show.print (and__ f t 1 2);
  Show.print (and__ t f 1 2);
  Show.print (and__ f f 1 2);
  Show.print (or__ t t 1 2);
  Show.print (or__ f t 1 2);
  Show.print (or__ t f 1 2);
  Show.print (or__ f f 1 2)
