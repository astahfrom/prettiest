open Core

module Gen = Quickcheck.Generator


module Check (P : Prettiest.S) = struct
  open P
  open P.Infix

  let assert_equal a b =
    if Option.equal String.equal (render a) (render b)
    then ()
    else failwith ("Unequal at width: " ^ string_of_int P.width)

  let assert_height a b =
    let height s = s |> String.split_lines |> List.length in
    let a' = render a and b' = render b in
    if Option.equal Int.equal (Option.map ~f:height a') (Option.map ~f:height b')
    then ()
    else failwith ("Unequal at width: " ^ string_of_int P.width)

  let string_gen = Gen.list Gen.char_alphanum |> Gen.map ~f:String.of_char_list

  let text_gen = Gen.map string_gen ~f:text

  let doc_gen : P.t Quickcheck.Generator.t =
    let open Gen.Let_syntax in
    Gen.fixed_point (fun self ->
        match%bind Gen.size with
        | 0 -> text_gen
        | n -> 
          let cat =
            Gen.with_size ~size:(n - 1) self >>= fun l ->
            Gen.with_size ~size:(n - 1) self >>| fun r ->
            l <> r
          and flush =
            Gen.with_size ~size:(n - 1) self >>| fun a ->
            flush a
          in
          Gen.union [ text_gen; cat; flush ]
      )

  let%test_unit "empty left unit of <>" =
    Quickcheck.test doc_gen ~f:(fun a -> assert_equal (empty <> a) a)

  let%test_unit "empty right unit of <>" =
    Quickcheck.test doc_gen ~f:(fun a -> assert_equal (empty <> a) a)

  let%test_unit "<> associative" =
    Quickcheck.test (Gen.tuple3 doc_gen doc_gen doc_gen)
      ~f:(fun (a, b, c) ->
          assert_equal
            (a <> (b <> c))
            ((a <> b) <> c))

  let%test_unit "text <> homomorphism" =
    Quickcheck.test (Gen.tuple2 string_gen string_gen)
      ~f:(fun (s, t) ->
          assert_equal
            (text s <> text t)
            (text (s ^ t)))

  let%test_unit "flush <>" =
    Quickcheck.test (Gen.tuple2 doc_gen doc_gen)
      ~f:(fun (a, b) ->
          assert_equal
            (flush a <> flush b)
            (flush (flush a <> b)))

  let%test_unit "<|> associative" =
    Quickcheck.test (Gen.tuple3 doc_gen doc_gen doc_gen)
      ~f:(fun (a, b, c) ->
          assert_equal
            (a <|> (b <|> c))
            ((a <|> b) <|> c))

  let%test_unit "<> left distribute <|> wrt height" =
    Quickcheck.test (Gen.tuple3 doc_gen doc_gen doc_gen)
      ~f:(fun (a, b, c) ->
          assert_height
            ((a <|> b) <> c)
            ((a <> c) <|> (b <> c)))

  let%test_unit "<> right distribute <|> wrt height" =
    Quickcheck.test (Gen.tuple3 doc_gen doc_gen doc_gen)
      ~f:(fun (a, b, c) ->
          assert_height
            (c <> (a <|> b))
            ((c <> a) <|> (c <> b)))

  let%test_unit "flush distribute <|> wrt height" =
    Quickcheck.test (Gen.tuple2 doc_gen doc_gen)
      ~f:(fun (a, b) ->
          assert_height
            (flush (a <|> b))
            (flush a <|> flush b))

  let%test_unit "<|> commutative wrt height" =
    Quickcheck.test (Gen.tuple2 doc_gen doc_gen)
      ~f:(fun (a, b) ->
          assert_height
            (a <|> b)
            (b <|> a))

end


module P20 = Prettiest.Make (struct let width = 20 end)
module C20 = Check (P20)

module P50 = Prettiest.Make (struct let width = 50 end)
module C50 = Check (P50)

module P80 = Prettiest.Make (struct let width = 80 end)
module C80 = Check (P80)
