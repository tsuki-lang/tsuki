//! Semantic analyzer for types.

use crate::ast::{Ast, NodeData, NodeHandle, NodeKind};
use crate::common::{ErrorKind, Errors};
use crate::scope::{ScopeStack, Scopes, SymbolKind, Symbols, Variable, VariableKind};
use crate::sem::{SemCommon, SemPass};
use crate::types::{BuiltinTypes, FloatSize, IntegerSize, TypeId, TypeLog, TypeLogEntry, Types};

pub(crate) struct SemTypes<'c, 't, 'tl, 'bt, 's, 'sy> {
   common: &'c SemCommon,
   errors: Errors,

   types: &'t mut Types,
   log: &'tl mut TypeLog,
   builtin: &'bt BuiltinTypes,
   scopes: &'s mut Scopes,
   symbols: &'sy mut Symbols,

   scope_stack: ScopeStack,
}

/// Values borrowed to `SemTypes`, used during its construction.
pub(crate) struct SemTypesBorrows<'c, 't, 'tl, 'bt, 's, 'sy> {
   pub(crate) common: &'c SemCommon,
   pub(crate) types: &'t mut Types,
   pub(crate) log: &'tl mut TypeLog,
   pub(crate) builtin: &'bt BuiltinTypes,
   pub(crate) scopes: &'s mut Scopes,
   pub(crate) symbols: &'sy mut Symbols,
}

/// Specifies whether a node should be annotated in expression or statement context.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum NodeContext {
   Expression,
   Statement,
}

impl<'c, 't, 'tl, 'bt, 's, 'sy> SemTypes<'c, 't, 'tl, 'bt, 's, 'sy> {
   /// Creates a new instance of the `SemTypes` analysis phase.
   pub fn new(borrows: SemTypesBorrows<'c, 't, 'tl, 'bt, 's, 'sy>) -> Self {
      let SemTypesBorrows {
         common,
         types,
         log,
         builtin,
         scopes,
         symbols,
      } = borrows;
      let mut scope_stack = ScopeStack::new();
      // The scope stack is always initialized with a top-level module scope, such that there is
      // always a valid scope on top.
      let _module_scope = scope_stack.push(scopes.create_scope());
      SemTypes {
         common,
         errors: Errors::new(),

         types,
         log,
         builtin,
         scopes,
         symbols,

         scope_stack,
      }
   }

   /// Annotates the given AST with the given type, and returns the type.
   fn annotate(&mut self, ast: &mut Ast, node: NodeHandle, typ: TypeId) -> TypeLogEntry {
      ast.set_type_id(node, typ);
      self.log.push(typ, node)
   }

   /// Emits an error of the given kind, also returning the error type.
   fn error(&mut self, ast: &Ast, node: NodeHandle, kind: ErrorKind) -> TypeLogEntry {
      self.emit_error(kind, ast.span(node).clone());
      self.log.push(self.builtin.t_error, node)
   }

   /// Annotates a literal with a concrete type.
   fn annotate_literal(&mut self, ast: &mut Ast, node: NodeHandle) -> TypeLogEntry {
      let typ = match ast.kind(node) {
         NodeKind::True => self.builtin.t_bool,
         NodeKind::False => self.builtin.t_bool,
         NodeKind::Uint8 => self.builtin.t_uint8,
         NodeKind::Uint16 => self.builtin.t_uint16,
         NodeKind::Uint32 => self.builtin.t_uint32,
         NodeKind::Uint64 => self.builtin.t_uint64,
         NodeKind::Int8 => self.builtin.t_int8,
         NodeKind::Int16 => self.builtin.t_int16,
         NodeKind::Int32 => self.builtin.t_int32,
         NodeKind::Int64 => self.builtin.t_int64,
         NodeKind::Float32 => self.builtin.t_float32,
         NodeKind::Float64 => self.builtin.t_float64,
         NodeKind::Character => self.builtin.t_char,
         _ => unreachable!(),
      };
      self.annotate(ast, node, typ)
   }

   // Currently, this does some rather simplistic analysis just to Make it Work™, but in the
   // future when operators will be lowered to trait instance function calls, this will be
   // replaced by much simpler logic and compiler intrinsics inside the stdlib.

   /// Annotates a unary operator with types.
   fn annotate_unary_operator(&mut self, ast: &mut Ast, node: NodeHandle) -> TypeLogEntry {
      let log_entry = self.annotate_node(ast, ast.first_handle(node), NodeContext::Expression);
      let right = self.log.typ(log_entry);
      let right_kind = self.types.kind(right);
      let typ = match ast.kind(node) {
         NodeKind::Not if right == self.builtin.t_bool => right,
         NodeKind::BitNot if right_kind.is_integer() => right,
         NodeKind::Neg if right_kind.is_numeric() => right,
         _ => {
            let right_name = self.types.name(right);
            let kind = ErrorKind::InvalidUnaryOperator(right_name.into());
            return self.error(ast, node, kind);
         }
      };
      self.annotate(ast, node, typ)
   }

   /// Widens the given integer node to the provided size.
   ///
   /// For literal nodes, this converts the literal directly. For other nodes, this wraps the node
   /// in a `WidenUint` or `WidenInt` with the type set to represent the new size.
   fn widen_integer(
      &mut self,
      ast: &mut Ast,
      node: NodeHandle,
      new_size: IntegerSize,
   ) -> TypeLogEntry {
      if ast.kind(node).is_integer() {
         // Shortcut path for literals.
         let as_uint = ast.extra(node).unwrap_uint();
         ast.convert(
            node,
            match new_size {
               IntegerSize::U8 => NodeKind::Uint8,
               IntegerSize::U16 => NodeKind::Uint16,
               IntegerSize::U32 => NodeKind::Uint32,
               IntegerSize::U64 => NodeKind::Uint64,
               IntegerSize::S8 => NodeKind::Int8,
               IntegerSize::S16 => NodeKind::Int16,
               IntegerSize::S32 => NodeKind::Int32,
               IntegerSize::S64 => NodeKind::Int64,
            },
         );
         ast.set_extra(
            node,
            match new_size {
               IntegerSize::U8 => NodeData::Uint8(as_uint as u8),
               IntegerSize::U16 => NodeData::Uint16(as_uint as u16),
               IntegerSize::U32 => NodeData::Uint32(as_uint as u32),
               IntegerSize::U64 => NodeData::Uint64(as_uint as u64),
               IntegerSize::S8 => NodeData::Int8(as_uint as i8),
               IntegerSize::S16 => NodeData::Int16(as_uint as i16),
               IntegerSize::S32 => NodeData::Int32(as_uint as i32),
               IntegerSize::S64 => NodeData::Int64(as_uint as i64),
            },
         );
      } else {
         // Backend path for other nodes.
         if ast.kind(node).is_unsigned_integer() {
            ast.wrap(node, NodeKind::WidenUint);
         } else {
            ast.wrap(node, NodeKind::WidenInt);
         }
      }
      self.annotate(
         ast,
         node,
         match new_size {
            IntegerSize::U8 => self.builtin.t_uint8,
            IntegerSize::U16 => self.builtin.t_uint16,
            IntegerSize::U32 => self.builtin.t_uint32,
            IntegerSize::U64 => self.builtin.t_uint64,
            IntegerSize::S8 => self.builtin.t_int8,
            IntegerSize::S16 => self.builtin.t_int16,
            IntegerSize::S32 => self.builtin.t_int32,
            IntegerSize::S64 => self.builtin.t_int64,
         },
      )
   }

   /// Widens a float node to the given size.
   ///
   /// Behavior with literals is similar to `widen_integer`.
   fn widen_float(&mut self, ast: &mut Ast, node: NodeHandle, new_size: FloatSize) -> TypeLogEntry {
      if ast.kind(node).is_float() {
         let as_float = ast.extra(node).unwrap_float();
         ast.convert(
            node,
            match new_size {
               FloatSize::S32 => NodeKind::Float32,
               FloatSize::S64 => NodeKind::Float64,
            },
         );
         ast.set_extra(
            node,
            match new_size {
               FloatSize::S32 => NodeData::Float32(as_float as f32),
               FloatSize::S64 => NodeData::Float64(as_float),
            },
         );
      } else {
         ast.wrap(node, NodeKind::WidenFloat);
      }
      self.annotate(
         ast,
         node,
         match new_size {
            FloatSize::S32 => self.builtin.t_float32,
            FloatSize::S64 => self.builtin.t_float64,
         },
      )
   }

   /// Attempts to convert the type `from` to type `tp`. If an implicit conversion is not possible,
   /// returns `None`. Otherwise returns the converted type ID.
   fn perform_implicit_conversion(
      &mut self,
      ast: &mut Ast,
      node: NodeHandle,
      from: TypeId,
      to: TypeId,
   ) -> Option<TypeLogEntry> {
      // If the two types are equal, there's need for conversion.
      if from == to {
         return Some(self.log.push(to, node));
      }
      // Otherwise, compare their kinds for various traits.
      let from_kind = self.types.kind(from);
      let to_kind = self.types.kind(to);

      // Widening integer conversions
      if from_kind.is_integer() && to_kind.is_integer() {
         // Integers are only implicitly convertible to wider types of the same signedness,
         // eg. Int8 -> Int16, Int32 -> Int64, but not Int64 -> Int32, or Uint32 -> Int32.
         let from_size = from_kind.unwrap_integer();
         let to_size = to_kind.unwrap_integer();
         if to_size >= from_size {
            return Some(self.widen_integer(ast, node, to_size));
         }
      }

      // Widening float conversions
      if from_kind.is_float() && to_kind.is_float() {
         // Floats are only implicitly convertible if the destination type is wider than the
         // source type (Float32 -> Float64).
         let from_size = from_kind.unwrap_float();
         let to_size = to_kind.unwrap_float();
         if to_size >= from_size {
            return Some(self.widen_float(ast, node, to_size));
         }
      }

      None
   }

   /// Annotates a location expression, ie. variables `a`, members `.x`.
   fn annotate_location(&mut self, ast: &mut Ast, node: NodeHandle) -> TypeLogEntry {
      match ast.kind(node) {
         NodeKind::Identifier => {
            let name = self.common.get_source_range_from_node(ast, node);
            if let Some(variable) = self.scope_stack.lookup(self.scopes, name) {
               let typ = self.symbols.type_id(variable);
               ast.convert_to_symbol(node, variable);
               let log = self.annotate(ast, node, typ);
               ast.wrap(node, NodeKind::Variable);
               log
            } else {
               self.error(ast, node, ErrorKind::UndeclaredSymbol(name.into()))
            }
         }
         // TODO: Make this into a better error. This would require slicing the source string,
         // which we can't do because spans don't store direct byte indices to it at the moment.
         _ => self.error(ast, node, ErrorKind::InvalidLhsOfAssignment),
      }
   }

   /// Emits a type mismatch error.
   fn type_mismatch(
      &mut self,
      ast: &Ast,
      node: NodeHandle,
      expected: TypeId,
      got: TypeId,
   ) -> TypeLogEntry {
      let expected_name = self.types.name(expected);
      let provided_name = self.types.name(got);
      let kind = ErrorKind::TypeMismatch(expected_name.to_owned(), provided_name.to_owned());
      self.error(ast, node, kind)
   }

   /// Annotates a binary operator with types.
   fn annotate_binary_operator(&mut self, ast: &mut Ast, node: NodeHandle) -> TypeLogEntry {
      let (left, right) = (ast.first_handle(node), ast.second_handle(node));
      let left_entry = self.annotate_node(ast, left, NodeContext::Expression);
      let right_entry = self.annotate_node(ast, right, NodeContext::Expression);
      let left_type = self.log.typ(left_entry);
      let right_type = self.log.typ(right_entry);
      let conversion = self.perform_implicit_conversion(ast, right, right_type, left_type);
      let left_type_kind = self.types.kind(left_type);
      let typ = match ast.kind(node) {
         // Arithmetic operators always evaluate to the same type as the LHS.
         NodeKind::Plus | NodeKind::Minus | NodeKind::Mul | NodeKind::Div
            if conversion.is_some() =>
         {
            left_type
         }

         // Comparison operators always evaluate to `Bool`.
         NodeKind::Equal | NodeKind::NotEqual
            if conversion.is_some() && left_type_kind.is_bool() =>
         {
            self.builtin.t_bool
         }
         | NodeKind::Equal
         | NodeKind::NotEqual
         | NodeKind::Less
         | NodeKind::LessEqual
         | NodeKind::Greater
         | NodeKind::GreaterEqual
            if conversion.is_some() && left_type_kind.is_numeric() =>
         {
            self.builtin.t_bool
         }

         // Other operators, and failed conversions, raise a type mismatch error.
         _ => {
            return self.type_mismatch(ast, node, left_type, right_type);
         }
      };
      self.annotate(ast, node, typ)
   }

   /// Annotates a function call.
   fn annotate_call(&mut self, ast: &mut Ast, node: NodeHandle) -> TypeLogEntry {
      // Because function calls aren't really supported yet, the function call syntax is reused
      // for backend intrinsics. This will someday be replaced by a `compiler_intrinsic` pragma.
      let callee = ast.first_handle(node);
      if ast.kind(callee) != NodeKind::Identifier {
         return self.error(ast, node, ErrorKind::NonIntrinCall);
      }
      let name = self.common.get_source_range_from_node(ast, callee);
      let expected_argument_count;
      match name {
         "__intrin_print_int32" => {
            expected_argument_count = 1;
            ast.convert_preserve(node, NodeKind::PrintInt32);
         }
         "__intrin_print_float32" => {
            expected_argument_count = 1;
            ast.convert_preserve(node, NodeKind::PrintFloat32);
         }
         _ => return self.error(ast, node, ErrorKind::NonIntrinCall),
      }
      let arguments = ast.extra(node).unwrap_node_list();
      if arguments.len() != expected_argument_count {
         let kind = ErrorKind::NArgumentsExpected(expected_argument_count, arguments.len());
         return self.error(ast, node, kind);
      }
      // I don't like that I have to use normal indices. Give me back my inline iterators :(
      for i in 0..arguments.len() {
         // TODO: Argument type checking.
         let argument = ast.extra(node).unwrap_node_list()[i];
         let _ = self.annotate_node(ast, argument, NodeContext::Expression);
      }
      self.annotate(ast, node, self.builtin.t_unit)
   }

   /// Annotates an assignment.
   fn annotate_assignment(
      &mut self,
      ast: &mut Ast,
      node: NodeHandle,
      context: NodeContext,
   ) -> TypeLogEntry {
      // TODO: Pointers and assigning values to them.
      let (left, right) = (ast.first_handle(node), ast.second_handle(node));
      let left_entry = self.annotate_location(ast, left);
      let left_type = self.log.typ(left_entry);
      let right_entry = self.annotate_node(ast, right, NodeContext::Expression);
      let right_type = self.log.typ(right_entry);
      // Check types.
      if right_type != left_type {
         return self.type_mismatch(ast, node, left_type, right_type);
      }
      // Check mutability.
      // TODO: This could maybe be moved into a different check, shoving this logic into assignments
      // doesn't seem very clean.
      let target_is_mutable = match ast.kind(left) {
         NodeKind::Variable => {
            let symbol = ast.first_handle(left);
            let variable = self.symbols.kind(ast.symbol_id(symbol)).unwrap_variable();
            variable.kind == VariableKind::Var
         }
         _ => unreachable!(),
      };
      if !target_is_mutable {
         return self.error(ast, left, ErrorKind::CannotAssignImmutableLocation);
      }
      match context {
         NodeContext::Expression => self.annotate(ast, node, left_type),
         NodeContext::Statement => self.annotate(ast, node, self.builtin.t_statement),
      }
   }

   /// Annotates statements in a list of statements.
   fn annotate_statement_list(
      &mut self,
      ast: &mut Ast,
      node: NodeHandle,
      context: NodeContext,
   ) -> TypeLogEntry {
      let mut last_log = None;
      ast.walk_node_list_mut(node, |ast, index, child| {
         // Trailing expressions in expression context statement lists get special treatment.
         // They are resulting expressions of the these statement lists, and thus get analyzed as
         // proper expressions rather than statements. They are also not subject to triggering the
         // UnusedValue error, as the resulting value _is_ actually used - it's the result of the
         // statement list.
         let is_last_expression =
            ast.is_last_child(node, index) && context == NodeContext::Expression;
         let log_entry = self.annotate_node(
            ast,
            child,
            if is_last_expression {
               NodeContext::Expression
            } else {
               NodeContext::Statement
            },
         );
         let typ = self.log.typ(log_entry);
         let type_kind = self.types.kind(typ);
         // For expressions, we have some special cases.
         if !type_kind.is_statement() {
            if is_last_expression {
               // Trailing expressions get assigned to the `last_log`, so that we know what the
               // result of the statement list is.
               last_log = Some(log_entry);
            } else {
               // Other expressions are unused, which is invalid.
               self.emit_error(ErrorKind::UnusedValue, ast.span(child).clone());
            }
         }
      });
      // Statement lists in expression context must always have a trailing expression.
      if context == NodeContext::Expression && last_log.is_none() {
         return self.error(ast, node, ErrorKind::MissingResult);
      }
      // Nodes in expression context inherit their type from the last expression statement in
      // the list.
      if let Some(log) = last_log {
         self.annotate(ast, node, self.log.typ(log))
      } else {
         self.annotate(ast, node, self.builtin.t_statement)
      }
   }

   /// Annotates a "pass" (`_`) statement.
   fn annotate_pass(&mut self, ast: &mut Ast, node: NodeHandle) -> TypeLogEntry {
      self.annotate(ast, node, self.builtin.t_statement)
   }

   /// Annotates a prefix `do` block.
   fn annotate_do(
      &mut self,
      ast: &mut Ast,
      node: NodeHandle,
      context: NodeContext,
   ) -> TypeLogEntry {
      let scope = self.scope_stack.push(self.scopes.create_scope());
      ast.set_scope(node, Some(scope));
      let log_entry = self.annotate_statement_list(ast, node, context);
      self.scope_stack.pop();
      ast.convert_preserve(
         node,
         match context {
            NodeContext::Expression => NodeKind::DoExpression,
            NodeContext::Statement => NodeKind::DoStatement,
         },
      );
      log_entry
   }

   fn annotate_if(
      &mut self,
      ast: &mut Ast,
      node: NodeHandle,
      context: NodeContext,
   ) -> TypeLogEntry {
      let mut typ = None;
      ast.walk_node_list_mut(node, |ast, _index, branch| {
         // The scope is introduced before the condition is analyzed to have proper scoping behavior
         // in `if val`.
         let scope = self.scope_stack.push(self.scopes.create_scope());
         ast.set_scope(branch, Some(scope));
         // Only check the condition if it's an `if` branch. `else` branches do not have
         // the condition.
         if ast.kind(branch) == NodeKind::IfBranch {
            let condition = ast.first_handle(branch);
            let condition_entry = self.annotate_node(ast, condition, context);
            let condition_type = self.log.typ(condition_entry);
            if !self.types.kind(condition_type).is_bool() {
               self.emit_error(
                  ErrorKind::IfConditionMustBeBool,
                  ast.span(condition).clone(),
               );
            }
         }
         let body_entry = self.annotate_statement_list(ast, branch, context);
         let body_type = self.log.typ(body_entry);
         if context == NodeContext::Expression {
            match typ {
               None => typ = Some(body_type),
               Some(typ) if body_type != typ => {
                  // The type log entry is discarded here, because more mismatch errors may
                  // arise later in the `if` statement.
                  let _ = self.type_mismatch(ast, node, typ, body_type);
               }
               _ => (),
            }
         }
         self.scope_stack.pop();
      });
      ast.convert_preserve(
         node,
         match context {
            NodeContext::Expression => NodeKind::IfExpression,
            NodeContext::Statement => NodeKind::IfStatement,
         },
      );
      self.annotate(ast, node, typ.unwrap_or(self.builtin.t_statement))
   }

   /// Annotates a variable declaration.
   fn annotate_variable_declaration(&mut self, ast: &mut Ast, node: NodeHandle) -> TypeLogEntry {
      let kind = if ast.kind(node) == NodeKind::Val {
         VariableKind::Val
      } else {
         VariableKind::Var
      };
      let variable = Variable { kind };
      let name_node = ast.first_handle(node);
      let value_node = ast.second_handle(node);
      let value_log = self.annotate_node(ast, value_node, NodeContext::Expression);
      let value_type = self.log.typ(value_log);
      match ast.kind(name_node) {
         NodeKind::Discard => {
            // A discarding assignment is converted to an AssignDiscard node containing
            // the original value.
            ast.convert(node, NodeKind::AssignDiscard);
            ast.set_first_handle(node, value_node);
         }
         NodeKind::Identifier => {
            let name = self.common.get_source_range_from_node(ast, name_node);
            let symbol =
               self.symbols.create(name, node, value_type, SymbolKind::Variable(variable));
            ast.convert_to_symbol(name_node, symbol);
            let scope = self.scope_stack.top();
            self.scopes.insert(scope, name, symbol);
            // The variable type annotation is less relevant to error reporting than the fact that
            // it's a statement. This sounds counterintuitive at first, but note that we're
            // requested to annotate the Val/Var node, not the variable name node, so the calling
            // function likely expects a statement instead of an expression.
            let _ = self.annotate(ast, name_node, value_type);
         }
         _ => unreachable!(),
      }
      self.annotate(ast, node, self.builtin.t_statement)
   }

   /// Annotates the given AST node.
   fn annotate_node(
      &mut self,
      ast: &mut Ast,
      node: NodeHandle,
      context: NodeContext,
   ) -> TypeLogEntry {
      match ast.kind(node) {
         // Literals
         | NodeKind::True
         | NodeKind::False
         | NodeKind::Uint8
         | NodeKind::Uint16
         | NodeKind::Uint32
         | NodeKind::Uint64
         | NodeKind::Int8
         | NodeKind::Int16
         | NodeKind::Int32
         | NodeKind::Int64
         | NodeKind::Float32
         | NodeKind::Float64
         | NodeKind::Character => self.annotate_literal(ast, node),

         // Locations
         NodeKind::Identifier => self.annotate_location(ast, node),

         // Unary operators
         // ---
         // The following operators were omitted from the generic rule:
         // NodeKind::Member - magic for field access in self
         // NodeKind::Ref - magic for creating pointers
         // NodeKind::Deref - magic for dereferencing
         NodeKind::Not | NodeKind::Neg | NodeKind::BitNot => {
            self.annotate_unary_operator(ast, node)
         }

         // Binary operators
         // ---
         // The following kinds were omitted from the generic rule:
         // NodeKind::Dot - magic for field access
         | NodeKind::Plus
         | NodeKind::Minus
         | NodeKind::Mul
         | NodeKind::Div
         | NodeKind::Equal
         | NodeKind::NotEqual
         | NodeKind::Less
         | NodeKind::LessEqual
         | NodeKind::Greater
         | NodeKind::GreaterEqual => self.annotate_binary_operator(ast, node),
         NodeKind::Call => self.annotate_call(ast, node),
         NodeKind::Assign => self.annotate_assignment(ast, node, context),
         // Other operators are to be implemented later.

         // Control flow
         NodeKind::StatementList => self.annotate_statement_list(ast, node, context),
         NodeKind::Pass => self.annotate_pass(ast, node),
         NodeKind::Do => self.annotate_do(ast, node, context),
         NodeKind::If => self.annotate_if(ast, node, context),

         // Declarations
         NodeKind::Val | NodeKind::Var => self.annotate_variable_declaration(ast, node),

         // Other nodes are invalid (or not implemented yet).
         other => self.error(ast, node, ErrorKind::SemTypesInvalidAstNode(other)),
      }
   }
}

impl SemPass for SemTypes<'_, '_, '_, '_, '_, '_> {
   type Result = TypeLogEntry;

   /// Performs type analysis for the given AST node. This annotates the node with a concrete type.
   fn analyze(&mut self, mut ast: Ast, root_node: NodeHandle) -> Ast {
      let _ = self.annotate_node(&mut ast, root_node, NodeContext::Statement);
      ast
   }

   fn filename(&self) -> &str {
      &self.common.filename
   }

   fn errors(&self) -> &Errors {
      &self.errors
   }

   fn errors_mut(&mut self) -> &mut Errors {
      &mut self.errors
   }

   fn into_errors(self) -> Errors {
      self.errors
   }
}
