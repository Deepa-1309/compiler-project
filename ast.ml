(** {1 Abstract syntax of the expression language. }

   This module captures the {i abstract syntax tree} of the expression
   language. The compiler takes its input as a string, possibly
   through a file, from the user. However, internally it is much more
   convenient to represent the program as a tree called the {i abstract
   syntax tree}.

   For example, the two different expressions [(2 + 3) * 4] from [2 +
   (3 * 4)] can be represented as the distinct trees as shown below

 *)

(**
{v

+--------------------------------------------------------+
|          (2 + 3) * 4       |      2 + (3 * 4)          |
|----------------------------|---------------------------|
|                            |                           |
|              *             |        +                  |
|             / \            |       / \                 |
|            +   4           |      2   *                |
|           / \              |         / \               |
|          2   3             |        3   4              |
|                            |                           |
+--------------------------------------------------------+

v}

*)

(**

   The tree representation makes the order of application of the
   operators explicit. So when using the tree representation, we can
   ignore parenthesis, operator precedence and other similar concepts
   that are required to disambiguate expressions represented as
   strings (is it add 2 and 3 and then multiply 4 or is it add 2 to
   the multiplication of 3 and 4).

 *)

(** {2 Formal definition}

   The abstract syntax for the expression language is defined {i
   inductively} as the set of {i trees} satisfying the following rules
   of construction.

   + Every integer is an expression. This is the base case that
   creates the leaves of the tree. All leaves are labeled by integers.
   + If [e₁] and [e₂] are expressions then so is [e₁ ∘ e₂] where [∘]
   is one of the operators [+],[*] or [-]. This is the inductive step
   which builds a tree with internal node marked with the operator [∘]
   and subtrees [e₁] and [e₂].

   The use of the word {i inductively} means that every expression
   (tree) is built out of the above rules where the rule 1 acts as the
   base case. If we interpret the above two rules as rules to build
   expression trees the resulting labeled tree is called the abstract
   syntax tree (or in compiler literature parse tree) of expressions.


 *)

(** {2 Representing the abstract syntax tree}

The AST of the expression language can easily be represented using {i
   variant types} in Ocaml. For each rule in the inductive definition
   above, we have a constructor for the appropriate type.

@see <https://dev.realworldocaml.org/variants.html> Variants in Ocaml

 *)

type expr = Const of int
          | Op of expr * binOp * expr

and binOp = Plus
          | Minus
          | Mul
          | Div

(** {2 Helper functions for building expression }

*)

(** A more convenient way to build [e₁ + e₂] *)
let plus  e1 e2 = Op (e1, Plus, e2)

(** A more convenient way to build [e₁ - e₂] *)
let minus e1 e2 = Op (e1, Minus, e2)

(** A more convenient way to build [e₁ * e₂] *)
let mul e1 e2 = Op (e1, Mul, e2)

let div e1 e2 = Op (e1, Div, e2)

(**

This function gives the meaning of each operator as an [int -> int -> int]
function. This is used by the reverse. Not required by the [ec] compiler so may
be skipped in the first reading. It is only used by the reverse polish rp machine.

 *)

let binOpDenote = function
  | Plus  -> Int.add
  | Minus -> Int.sub
  | Mul   -> Int.mul
  | Div   -> Int.div
