import gleam/string
import gleam/list
import gleam/option.{type Option, Some, None}
import gleam/option as opt

pub type Expr {
  List(List(Expr))
  Atom(String)
}

pub fn is_alphanumeric(s: String) -> Bool {
  string.to_graphemes(s)
  |> list.all(fn(char) {
    case char {
      "a" | "b" | "c" | "d" | "e" | "f" | "g" | "h" | "i" | "j" | "k" | "l" | "m" |
      "n" | "o" | "p" | "q" | "r" | "s" | "t" | "u" | "v" | "w" | "x" | "y" | "z" |
      "A" | "B" | "C" | "D" | "E" | "F" | "G" | "H" | "I" | "J" | "K" | "L" | "M" |
      "N" | "O" | "P" | "Q" | "R" | "S" | "T" | "U" | "V" | "W" | "X" | "Y" | "Z" |
      "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" -> True
      _ -> False
    }
  })
}

type Parser(a) = fn(List(String)) -> Option(#(List(String), a))

fn alt(p: Parser(a), q: Parser(a)) -> Parser(a) {
  fn(input) {
    opt.lazy_or(p(input), fn() { q(input) })
  }
}

fn wrap(x: a) -> Parser(a) {
  fn(input) { Some(#(input, x)) }
}

fn fail() -> Parser(a) {
  fn(_) { None }
}

fn then(p: Parser(a), k: fn(a) -> Parser(b)) -> Parser(b) {
  fn(input) {
    opt.then(p(input), fn(x) {
      case x {
        #(input, v) -> k(v)(input)
      }
    })
  }
}

fn seq(p: Parser(a), q: Parser(b)) -> Parser(b) {
  then(p, fn(_) { q })
}

fn satisfy(f: fn(String) -> Bool) -> Parser(String) {
  fn(input) {
    case input {
      [x, ..xs] -> case f(x) {
        True -> Some(#(xs, x))
        False -> None
      }
      [] -> None
    }
  }
}

fn char(c: String) -> Parser(Nil) {
  use _ <- then(satisfy(fn(x) { x == c }))
  wrap(Nil)
}

fn many(p: Parser(a)) -> Parser(List(a)) {
  alt(
    then(p, fn(x) {
      then(many(p), fn(xs) {
        wrap(list.append([x], xs))
      })
    }),
    wrap([]))
}

fn parse_atom() -> Parser(Expr) {
  use cs <- then(many(satisfy(is_alphanumeric)))
  case cs {
    [] -> fail()
    _ -> wrap(Atom(string.concat(cs)))
  }
}

fn parse_list() -> Parser(Expr) {
  use _ <- then(char("("))
  use e <- then(parse_expr())
  use es <- then(many(seq(char(" "), parse_expr())))
  use _ <- then(char(")"))
  wrap(List(list.append([e], es)))
}

fn parse_expr() -> Parser(Expr) {
  alt(parse_atom(), parse_list())
}

pub fn from_string(s: String) -> Option(Expr) {
  case parse_expr()(string.to_graphemes(s)) {
    Some(#(_, e)) -> Some(e)
    None -> None
  }
}

pub fn to_string(e: Expr) -> String {
  case e {
    List(es) -> "(" <> string.join(list.map(es, to_string), with: " ") <> ")"
    Atom(s) -> s
  }
}