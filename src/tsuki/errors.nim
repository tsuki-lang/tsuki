const
  errorFormat* = "$#($#, $#): $#"
  exceptionFormat* = "$#($#, $#)"

  # lex errors
  leUnexpectedChar* = "unexpected character '$#'"
  leUnexpectedEof* = "unexpected end of file"

  # parse errors
  peUnexpectedToken* = "unexpected token: '$#'"
  peXExpected* = "$# expected"
  peTokenMissing* = "missing '$#'"

  # compile errors
  ceSymUndeclared* = "undeclared symbol '$#'"
  ceSymIsNotAVariable* = "'$#' is not a variable"
  ceIdentExpected* = "identifier expected"
  ceSymCannotBeCalled* = "'$#' cannot be called"
  ceWrongParamCount* = "'$#' takes $# parameters, but $# were supplied"
  ceAsgnInvalidLHS* = "invalid left-hand side of assignment"
  ceIfExprMustHaveElse* = "if expression must have an else branch"
