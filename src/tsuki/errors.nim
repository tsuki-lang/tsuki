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
  peIndentLevel* = "indent level $# expected, but got $#"

  # compile errors
  ceSymUndeclared* = "undeclared symbol '$#'"
  ceSymIsNotAVariable* = "'$#' is not a variable"
  ceSymIsNotAnObject* = "'$#' is not an object type"
  ceSymAlreadyDeclared* = "'$#' is already declared"
  ceIdentExpected* = "identifier expected"
  ceSymCannotBeCalled* = "'$#' cannot be called"
  ceWrongParamCount* = "'$#' takes $# parameters, but $# were supplied"
  ceAsgnInvalidLHS* = "invalid left-hand side of assignment"
  ceIfExprMustHaveElse* = "if expression must have an else branch"
  ceExprExpected* = "expression expected"
  ceInvalidBreak* = "break can only be used in loops"
  ceInvalidContinue* = "continue can only be used in loops"
  ceInvalidReturn* = "return can only be used in procedures"
  ceFieldAlreadyExists* = "field '$#' is already declared"
  ceFieldUndeclared* = "undeclared field '$#'"
  ceMemberUndeclared* = "undeclared field '$1', use '.$1()' to call the method"
  ceFieldsUninitialized* = "all fields must be initialized; missing: $#"
  ceImplInvalid* = "an impl block may only contain proc definitions"
  ceInvalidMember* =
    "member access '.$#' may only be used inside of an impl block"
