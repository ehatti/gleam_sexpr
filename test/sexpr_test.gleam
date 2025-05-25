import sexpr.{Atom, List, to_string, from_string}
import gleam/option.{Some}

pub fn main() -> Nil {
  let assert True = to_string(List([Atom("foo"), Atom("bar")])) == "(foo bar)"
  echo from_string("(foo bar)")
  let assert True = from_string("(foo bar)") == Some(List([Atom("foo"), Atom("bar")]))
  Nil
}