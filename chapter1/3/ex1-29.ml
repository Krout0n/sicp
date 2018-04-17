let simpthon (f, a, b, n) =
  let h = float_of_int (b - a) /. float_of_int (n) in
  let rec iter (k, product) =
    let y = float_of_int(f(float_of_int(a) +. float_of_int (k) *. h)) in
    if a > b then 0.0
    else if k = (b + 1) then product
    else if k = 0 then iter (k+1, product+.y)
    else if k mod 2 = 1 then iter (k+1, product+.2.0*.y)
    else iter (k+1, product+.4.0*.y)
  in iter (0, 0.0) ;;

// print_float (simpthon (fun x -> x *. x *. x), 0, 1, 100);;
