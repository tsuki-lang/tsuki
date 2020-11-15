const
  errorFormat* = "$#($#, $#): $#"

  # lex errors
  leUnexpectedChar* = "unexpected character '$#'"
  leUnexpectedEof* = "unexpected end of file"

  # parse errors
  peUnexpectedToken* = "unexpected token: '$#'"
  peXExpected* = "$# expected"
  peTokenMissing* = "missing '$#'"
