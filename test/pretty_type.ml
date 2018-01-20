open Core

module P = Prettiest
open P.Infix
open P.Characters

module Type = struct
  type name = string

  type tag = name

  type t =
    | Var of name
    | Alias of name * t list
    | Arrow of t list
    | Forall of name list * t
    | Sum of (tag * t) list
    | Product of (tag * t) list

  let prec = function
    | Var _
    | Alias (_, []) -> (0, -1)
    | Sum _
    | Product _ -> (0, 100)
    | Alias _ -> (1, 0)
    | Arrow _ -> (2, 1)
    | Forall _ -> (3, 2)
end

let arrow = P.text "->"

let tag c = P.text c <> colon

let parens x = lparen <> x <> rparen

let rec transform paren go above =
  go (fun self -> paren above self (transform paren go self)) above

let pretty_record xs =
  P.choice [
    lbrace <> P.hsep xs <> rbrace;
    lbrace <+> P.vcat xs <+> rbrace;
    (lbrace <+> P.vcat xs <> comma) $$ rbrace;
  ]

let pretty_elim xs =
  let flat = P.intersperse ~sep:(space <> bar) xs in
  let non_flat = match xs with
    | [] -> []
    | cx :: xs -> (lbrack <+> cx) :: List.map xs ~f:(fun cx -> bar <+> cx)
  in P.choice [
    lbrack <+> P.hsep flat <+> rbrack;
    P.vcat non_flat </> rbrack;
  ]

let pretty_type' f : Type.t -> P.t = function
  | Var x -> P.text x
  | Alias (name, []) -> P.text name
  | Alias (name, ts) ->
    let ts' = List.map ~f:f ts in
    P.text name <//> P.sep ts'
  | Arrow ts ->
    let flat = P.intersperse_map ~f:f ~sep:(space <> arrow) ts in
    P.sep flat
  | Forall (xs, t) ->
    let xs' = List.map ~f:P.text xs in
    let t' = f t in
    (P.text "forall" <//> P.sep xs' <> dot) <//> t'
  | Sum xs ->
    let pair (c, x) = tag c <//> f x in
    let xs' = List.map ~f:pair xs in
    pretty_elim xs'
  | Product xs ->
    let pair (c, x) = tag c <//> f x in
    let xs' = P.intersperse_map ~f:pair ~sep:comma xs in
    pretty_record xs'

let paren_prec prec above self doc =
  let (_, a) = prec above
  and (b, _) = prec self in
  if a < b then parens doc else doc

let pretty_type = transform (paren_prec Type.prec) pretty_type'

let fit = Option.value ~default:"did not fit"

let print f x =
  let doc = f x in
  P.render 80 doc |> fit |> print_endline;
  Out_channel.newline stdout;
  P.render 50 doc |> fit |> print_endline;
  Out_channel.newline stdout;
  P.render 20 doc |> fit |> print_endline

let print_type = print pretty_type

let%expect_test "type 1" =
  Forall (
    ["A"; "B"],
    Arrow [
      Var "A";
      Arrow [ Var "A"; Var "B" ];
      Var "B";
    ]
  ) |> print_type;
  [%expect {|
    forall A B. A -> (A -> B) -> B

    forall A B. A -> (A -> B) -> B

    forall A B.
      A -> (A -> B) -> B
      |}]

let%expect_test "type 2" =
  Arrow [
    Alias ("bool", []);
    Forall (["A"], Arrow [Alias ("bool", []); Alias ("option", [Var "A"])]);
    Alias ("option", [Alias ("list", [Alias ("int", [])])]);
  ] |> print_type;
  [%expect {|
    bool -> (forall A. bool -> option A) -> option (list int)

    bool -> (forall A. bool ->
                       option A) -> option (list int)

    bool ->
    (forall A.
       bool ->
       option A) ->
    option (list int)
    |}]

let%expect_test "type 3" =
  Forall (
    ["A"],
    Arrow [
      Var "A";
      Sum [
        ("none", Product []);
        ("some", Var "A")
      ]
    ]
  ) |> print_type;
  [%expect {|
    forall A. A -> [ none: {} | some: A ]

    forall A. A -> [ none: {} | some: A ]

    forall A.
      A -> [ none: {}
           | some: A ]
  |}]

let%expect_test "type 4" =
  let list a : Type.t =
    Sum [
      ("nil", Product []);
      ("cons", Product [
          ("head", a);
          ("tail", Alias ("list", [a]))
        ])
    ] in
  Forall (
    ["A"; "B"],
    Arrow [
      Arrow [ Var "A"; Var "B" ];
      list (Var "A");
      list (Var "B");
    ]
  ) |> print_type;
  [%expect {|
    forall A B. (A -> B) ->
                [ nil: {} | cons: {head: A, tail: list A} ] ->
                [ nil: {} | cons: {head: B, tail: list B} ]

    forall A B.
      (A -> B) ->
      [ nil: {} | cons: {head: A, tail: list A} ] ->
      [ nil: {} | cons: {head: B, tail: list B} ]

    forall A B.
      (A -> B) ->
      [ nil: {}
      | cons:
          { head: A,
            tail:
              list A,
          } ] ->
      [ nil: {}
      | cons:
          { head: B,
            tail:
              list B } ]
  |}]
