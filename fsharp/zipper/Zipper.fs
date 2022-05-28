module Zipper

type Tree<'a> = Tree of value: 'a * left: Tree<'a> option * right: Tree<'a> option

type Direction<'a> =
    | Left of value: 'a * right: Tree<'a> option
    | Right of value: 'a * left: Tree<'a> option

type Zipper<'a> = Zipper of tree: Tree<'a> * path: Direction<'a> list

let tree value left right = Tree(value, left, right)

///get a zipper out of a rose tree, the focus is on the root node
let fromTree tree = Zipper(tree, [])

///move the focus to the parent, returns a new zipper
let up = function
    | Zipper (_, []) -> None
    | Zipper (left, Left (value, right) :: directions) ->
        Zipper(tree value (Some left) right, directions)
        |> Some
    | Zipper (right, Right (value, left) :: directions) ->
        Zipper(tree value left (Some right), directions)
        |> Some
        
///get the rose tree out of the zipper
let rec toTree (Zipper(tree, _) as zipper) =
    zipper
    |> up
    |> Option.map toTree
    |> Option.defaultValue tree

///get the value of the focus node
let value (Zipper (Tree (v, _, _), _)) = v

let down dir directions head =
    head
    |> Option.map (fun tree -> Zipper(tree, dir :: directions))

///move the focus to the left child of the focus node, returns a new zipper
let left (Zipper (Tree (value, left, right), directions) as zipper) =
    down (Left(value, right)) directions left

///move the focus to the right child of the focus node, returns a new zipper
let right (Zipper (Tree (value, left, right), directions) as zipper) =
    down (Right(value, left)) directions right

///set the value of the focus node, returns a new zipper
let setValue newValue (Zipper (Tree (_, left, right), directions)) =
    Zipper(tree newValue left right, directions)

///insert a new subtree before the focus node, it becomes the left of the focus node, returns a new zipper
let setLeft newLeft (Zipper (Tree (value, _, right), directions)) =
    Zipper(tree value newLeft right, directions)

///insert a new subtree after the focus node, it becomes the right of the focus node, returns a new zipper
let setRight newRight (Zipper (Tree (value, left, _), directions)) =
    Zipper(tree value left newRight, directions)