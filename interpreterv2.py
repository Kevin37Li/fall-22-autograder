from enum import Enum
from intbase import InterpreterBase, ErrorType
from tokenize import Tokenizer
# from env_v1 import EnvironmentManager
# from func_v1 import FunctionManager

# Enumerated type for our different language data types
class Type(Enum):
  INT = 1
  BOOL = 2
  STRING = 3
  REFINT = 4
  REFBOOL = 5
  REFSTRING = 6
  VOID = 7

# Represents a value, which has a type and its value
class Value:
  def __init__(self, type = None, value = None):
    self.t = type
    self.v = value

  def value(self):
    return self.v

  def set(self, other):
    self.t = other.t
    self.v = other.v
    return self

  def type(self):
    return self.t

# The EnvironmentManager class keeps a mapping between each global variable (aka symbol)
# in a brewin program and the value of that variable - the value that's passed in can be
# anything you like. In our implementation we pass in a Value object which holds a type
# and a value (e.g., Int, 10).
class EnvironmentManager:
  def __init__(self):
    self.environment = {}

  # Gets the data associated a variable name
  def get(self, symbol):
    if symbol in self.environment:
      return self.environment[symbol]

    return None

  def has(self, symbol):
    return symbol in self.environment

  # Sets the data associated with a variable name
  def set(self, symbol, value):
    self.environment[symbol] = value


# FuncInfo is a class that represents information about a function
# Right now, the only thing this tracks is the line number of the first executable instruction
# of the function (i.e., the line after the function prototype: func foo)
class FuncInfo:
  def __init__(self, start_ip):
    self.start_ip = start_ip    # line number, zero-based
    self.func_scope = EnvironmentManager()
    self.param_order = []
    self.return_type = None
    self.return_addr = None

# FunctionManager keeps track of every function in the program, mapping the function name
# to a FuncInfo object (which has the starting line number/instruction pointer) of that function.
class FunctionManager:
  def __init__(self, tokenized_program):
    self.func_cache = {}
    self._cache_function_line_numbers(tokenized_program)

  def get_function_info(self, func_name):
    if func_name not in self.func_cache:
      return None
    return self.func_cache[func_name]

  def _cache_function_line_numbers(self, tokenized_program):
    for line_num, line in enumerate(tokenized_program):
      if line and line[0] == InterpreterBase.FUNC_DEF:
        func_name = line[1]
        func_info = FuncInfo(line_num + 1)   # function starts executing on line after funcdef

        # adding parameters to func_scope in FuncInfo object
        for token in line[2:]:
          if ":" in token:
            pname, parameter_type = token.split(":")
            match parameter_type:
              case InterpreterBase.INT_DEF: 
                ptype = Type.INT
              case InterpreterBase.BOOL_DEF:
                ptype = Type.BOOL
              case InterpreterBase.STRING_DEF:
                ptype = Type.STRING
              case InterpreterBase.REFINT_DEF:
                ptype = Type.REFINT
              case InterpreterBase.REFBOOL_DEF:
                ptype = Type.REFBOOL
              case InterpreterBase.REFSTRING_DEF:
                ptype = Type.REFSTRING

            func_info.func_scope.set(pname, Value(ptype))
            func_info.param_order.append(pname)
          # set the return type for the function
          else:
            match token:
              case InterpreterBase.INT_DEF: 
                rtype = Type.INT
              case InterpreterBase.BOOL_DEF:
                rtype = Type.BOOL
              case InterpreterBase.STRING_DEF:
                rtype = Type.STRING
              case InterpreterBase.VOID_DEF:
                rtype = Type.VOID

        func_info.return_type = rtype
        self.func_cache[func_name] = func_info



# Main interpreter class
class Interpreter(InterpreterBase):

  RESULTI_DEF = "resulti"
  RESULTB_DEF = "resultb"
  RESULTS_DEF = "results"

  def __init__(self, console_output=True, input=None, trace_output=False):
    super().__init__(console_output, input)
    self._setup_operations()  # setup all valid binary operations and the types they work on
    self.trace_output = trace_output

  # run a program, provided in an array of strings, one string per line of source code
  def run(self, program):
    self.program = program
    self._compute_indentation(program)  # determine indentation of every line
    self.tokenized_program = Tokenizer.tokenize_program(program)
    self.func_manager = FunctionManager(self.tokenized_program)
    main_info = self._get_func_info(InterpreterBase.MAIN_FUNC)
    self.ip = main_info.start_ip
    self.return_stack = []
    self.terminate = False
    self.env_manager = []
    self.env_manager.append([main_info.func_scope])
    self.return_stack.append(main_info)

    # main interpreter run loop
    while not self.terminate:
      self._process_line()

  def _process_line(self):
    if self.trace_output:
      print(f"{self.ip:04}: {self.program[self.ip].rstrip()}")
    tokens = self.tokenized_program[self.ip]
    if not tokens:
      self._blank_line()
      return

    args = tokens[1:]

    match tokens[0]:
      case InterpreterBase.ASSIGN_DEF:
        self._assign(args)
      case InterpreterBase.FUNCCALL_DEF:
        self._funccall(args)
      case InterpreterBase.ENDFUNC_DEF:
        self._endfunc()
      case InterpreterBase.IF_DEF:
        self._if(args)
      case InterpreterBase.ELSE_DEF:
        self._else()
      case InterpreterBase.ENDIF_DEF:
        self._endif()
      case InterpreterBase.RETURN_DEF:
        self._return(args)
      case InterpreterBase.WHILE_DEF:
        self._while(args)
      case InterpreterBase.ENDWHILE_DEF:
        self._endwhile(args)
      case InterpreterBase.VAR_DEF:
        self._var(args)
      case default:
        raise Exception(f'Unknown command: {tokens[0]}')

  def _blank_line(self):
    self._advance_to_next_statement()

  def _var(self, args):
    if len(args) < 2:
      super().error(ErrorType.SYNTAX_ERROR, "Invalid var statement")

    type = args[0]
    match type:
      case "int":
        value_type = Value(Type.INT, 0)
      case "bool":
        value_type = Value(Type.BOOL, False)
      case "string":
        value_type = Value(Type.STRING, "")
      case default:
        super().error(ErrorType.TYPE_ERROR, f"Invalid type {type} in var statement", self.ip)

    for vname in args[1:]:
      if self._is_redefined(vname):
        super().error(ErrorType.NAME_ERROR, f"variable ({vname}) is redefined in the same block", self.ip)
      self._set_value(vname, value_type, self._get_current_func_scope_chain()[-1])

    self._advance_to_next_statement()

  def _is_redefined(self, vname):
    if self._get_current_func_scope_chain()[-1].has(vname):
      return True
    return False

  def _is_constant(self, token):
    for scope in reversed(self._get_current_func_scope_chain()):
      if scope.has(token):
        return False
    return True

  def _where_defined(self, vname):
    if self.trace_output:
      print(len(self._get_current_func_scope_chain()))
    for scope in reversed(self._get_current_func_scope_chain()):
      if self.trace_output:
        print(scope.environment)
      if scope.has(vname):
        return scope
    super().error(ErrorType.NAME_ERROR, f"the variable ({vname}) refered is not defined yet", self.ip)

    
  def _assign(self, tokens):
   if len(tokens) < 2:
     super().error(ErrorType.SYNTAX_ERROR,"Invalid assignment statement") #no
   vname = tokens[0]
   scope = self._where_defined(vname)
   value_type = self._eval_expression(tokens[1:])
   self._set_value(vname, value_type, scope)
   self._advance_to_next_statement()

  def _funccall(self, args):
    if not args:
      super().error(ErrorType.SYNTAX_ERROR,"Missing function name to call", self.ip) #!
    if args[0] == InterpreterBase.PRINT_DEF:
      self._print(args[1:])
      self._advance_to_next_statement()
    elif args[0] == InterpreterBase.INPUT_DEF:
      self._input(args[1:])
      self._advance_to_next_statement()
    elif args[0] == InterpreterBase.STRTOINT_DEF:
      self._strtoint(args[1:])
      self._advance_to_next_statement()
    else:
      func_info = self._get_func_info(args[0])

      if len(func_info.param_order) != len(args[1:]):
        super().error(ErrorType.NAME_ERROR, f"The number of arguments to function call ({args[0]}) doesn't match the number of formal parameters", self.ip)
      func_info.return_addr = self.ip+1
      self.ip = func_info.start_ip

      arguments = args[1:]
      for param_pos, arg in enumerate(arguments):
        pname = func_info.param_order[param_pos]
        param_value_type = func_info.func_scope.get(pname)
        if param_value_type == None:
          super().error(ErrorType.NAME_ERROR, f"Unknown parameter name ({pname})", self.ip)
        ptype = param_value_type.type()
        if self.trace_output:
          print(f"The parameter {pname} of type ({ptype.name}) in function ({args[0]})")
        # pass by reference
        if "REF" in ptype.name and not self._is_constant(arg):
          func_info.func_scope.set(pname, self._get_value(arg))
        # pass by value
        else:
          func_info.func_scope.set(pname, Value().set(self._get_value(arg)))

      self.env_manager.append([func_info.func_scope]) 
      self.return_stack.append(func_info)    

  def _endfunc(self):
    callee_func_info = self.return_stack.pop()
    self.env_manager.pop()
    if not self.return_stack:  # done with main!
      self.terminate = True
    else:
      self.ip = callee_func_info.return_addr

  def _if(self, args):
    if not args:
      super().error(ErrorType.SYNTAX_ERROR,"Invalid if syntax", self.ip) #no
    value_type = self._eval_expression(args)
    if value_type.type() != Type.BOOL:
      super().error(ErrorType.TYPE_ERROR,"Non-boolean if expression", self.ip) #!
    if value_type.value():
      self._get_current_func_scope_chain().append(EnvironmentManager())
      self._advance_to_next_statement()
      return
    else:
      for line_num in range(self.ip+1, len(self.tokenized_program)):
        tokens = self.tokenized_program[line_num]
        if not tokens:
          continue
        if (tokens[0] == InterpreterBase.ENDIF_DEF or tokens[0] == InterpreterBase.ELSE_DEF) and self.indents[self.ip] == self.indents[line_num]:
          if tokens[0] == InterpreterBase.ELSE_DEF:
            self._get_current_func_scope_chain().append(EnvironmentManager())

          self.ip = line_num + 1
          return
    super().error(ErrorType.SYNTAX_ERROR,"Missing endif", self.ip) #no

  def _endif(self):
    self._advance_to_next_statement()
    self._get_current_func_scope_chain().pop()

  def _else(self):
    for line_num in range(self.ip+1, len(self.tokenized_program)):
      tokens = self.tokenized_program[line_num]
      if not tokens:
        continue
      if tokens[0] == InterpreterBase.ENDIF_DEF and self.indents[self.ip] == self.indents[line_num]:
          self._get_current_func_scope_chain().pop()
          self.ip = line_num + 1
          return
    super().error(ErrorType.SYNTAX_ERROR,"Missing endif", self.ip) #no

  def _return(self,args):
    if not args:
      self._endfunc()
      return

    value_type = self._eval_expression(args)
    callee_func_info = self.return_stack.pop()

    if value_type.type() != callee_func_info.return_type:
      super().error(ErrorType.TYPE_ERROR, f"The return value at line ({self.ip}) is not compatible with the return type of the function", self.ip)

    caller_func_info = self._get_current_func_info()
    self._set_return_value(value_type, caller_func_info.func_scope)

    self.ip = callee_func_info.return_addr
    self.env_manager.pop()


  def _set_return_value(self, value_type, scope):
    match value_type.type():
      case Type.INT:
        self._set_value(Interpreter.RESULTI_DEF, value_type, scope)
      case Type.BOOL:
        self._set_value(Interpreter.RESULTB_DEF, value_type, scope)
      case Type.STRING:
        self._set_value(Interpreter.RESULTS_DEF, value_type, scope)
      case default:
        super().error(ErrorType.TYPE_ERROR, f"Unknown type {value_type.type()} for the return value at ({self.ip})")


  def _while(self, args):
    if not args:
      super().error(ErrorType.SYNTAX_ERROR,"Missing while expression", self.ip) #no
    value_type = self._eval_expression(args)
    if value_type.type() != Type.BOOL:
      super().error(ErrorType.TYPE_ERROR,"Non-boolean while expression", self.ip) #!
    if value_type.value() == False:
      self._exit_while()
      return

    # If true, we advance to the next statement
    self._get_current_func_scope_chain().append(EnvironmentManager())
    self._advance_to_next_statement()

  def _exit_while(self):
    while_indent = self.indents[self.ip]
    cur_line = self.ip + 1
    while cur_line < len(self.tokenized_program):
      if self.tokenized_program[cur_line][0] == InterpreterBase.ENDWHILE_DEF and self.indents[cur_line] == while_indent:
        self.ip = cur_line + 1
        return
      if self.tokenized_program[cur_line] and self.indents[cur_line] < self.indents[self.ip]:
        break # syntax error!
      cur_line += 1
    # didn't find endwhile
    super().error(ErrorType.SYNTAX_ERROR,"Missing endwhile", self.ip) #no

  def _endwhile(self, args):
    self._get_current_func_scope_chain().pop()
    while_indent = self.indents[self.ip]
    cur_line = self.ip - 1
    while cur_line >= 0:
      if self.tokenized_program[cur_line][0] == InterpreterBase.WHILE_DEF and self.indents[cur_line] == while_indent:
        self.ip = cur_line
        return
      if self.tokenized_program[cur_line] and self.indents[cur_line] < self.indents[self.ip]:
        break # syntax error!
      cur_line -= 1
    # didn't find while
    super().error(ErrorType.SYNTAX_ERROR,"Missing while", self.ip) #no

  def _print(self, args):
    if not args:
      super().error(ErrorType.SYNTAX_ERROR,"Invalid print call syntax", self.ip) #no
    out = []
    for arg in args:
      val_type = self._get_value(arg)
      out.append(str(val_type.value()))
    super().output(''.join(out))

  def _input(self, args):
    if args:
      self._print(args)
    result = super().get_input()
    self._set_return_value(Value(Type.STRING, result), self._get_current_func_info.func_scope)

  def _strtoint(self, args):
    if len(args) != 1:
      super().error(ErrorType.SYNTAX_ERROR,"Invalid strtoint call syntax", self.ip) #no
    value_type = self._get_value(args[0])
    if value_type.type() != Type.STRING:
      super().error(ErrorType.TYPE_ERROR,"Non-string passed to strtoint", self.ip) #!
    self._set_return_value(Value(Type.INT, int(value_type.value())), self._get_current_func_info.func_scope)

  def _advance_to_next_statement(self):
    # for now just increment IP, but later deal with loops, returns, end of functions, etc.
    self.ip += 1

  # create a lookup table of code to run for different operators on different types
  def _setup_operations(self):
    self.binary_op_list = ['+','-','*','/','%','==','!=', '<', '<=', '>', '>=', '&', '|']
    self.binary_ops = {}
    self.binary_ops[Type.INT] = {
     '+': lambda a,b: Value(Type.INT, a.value()+b.value()),
     '-': lambda a,b: Value(Type.INT, a.value()-b.value()),
     '*': lambda a,b: Value(Type.INT, a.value()*b.value()),
     '/': lambda a,b: Value(Type.INT, a.value()//b.value()),  # // for integer ops
     '%': lambda a,b: Value(Type.INT, a.value()%b.value()),
     '==': lambda a,b: Value(Type.BOOL, a.value()==b.value()),
     '!=': lambda a,b: Value(Type.BOOL, a.value()!=b.value()),
     '>': lambda a,b: Value(Type.BOOL, a.value()>b.value()),
     '<': lambda a,b: Value(Type.BOOL, a.value()<b.value()),
     '>=': lambda a,b: Value(Type.BOOL, a.value()>=b.value()),
     '<=': lambda a,b: Value(Type.BOOL, a.value()<=b.value()),
    }
    self.binary_ops[Type.STRING] = {
     '+': lambda a,b: Value(Type.STRING, a.value()+b.value()),
     '==': lambda a,b: Value(Type.BOOL, a.value()==b.value()),
     '!=': lambda a,b: Value(Type.BOOL, a.value()!=b.value()),
     '>': lambda a,b: Value(Type.BOOL, a.value()>b.value()),
     '<': lambda a,b: Value(Type.BOOL, a.value()<b.value()),
     '>=': lambda a,b: Value(Type.BOOL, a.value()>=b.value()),
     '<=': lambda a,b: Value(Type.BOOL, a.value()<=b.value()),
    }
    self.binary_ops[Type.BOOL] = {
     '&': lambda a,b: Value(Type.BOOL, a.value() and b.value()),
     '==': lambda a,b: Value(Type.BOOL, a.value()==b.value()),
     '!=': lambda a,b: Value(Type.BOOL, a.value()!=b.value()),
     '|': lambda a,b: Value(Type.BOOL, a.value() or b.value())
    }

  def _compute_indentation(self, program):
    self.indents = [len(line) - len(line.lstrip(' ')) for line in program]

  def _get_func_info(self, funcname):
    func_info = self.func_manager.get_function_info(funcname)
    if func_info == None:
      super().error(ErrorType.NAME_ERROR,f"Unable to locate {funcname} function", self.ip) #!
    return func_info

  def _get_current_func_scope_chain(self):
    return self.env_manager[-1]

  def _get_current_func_info(self):
    return self.return_stack[-1]

  # given a token name (e.g., x, 17, True, "foo"), give us a Value object associated with it
  def _get_value(self, token):
    if not token:
      super().error(ErrorType.NAME_ERROR,f"Empty token", self.ip) #no
    if token[0] == '"':
      return Value(Type.STRING, token.strip('"'))
    if token.isdigit() or token[0] == '-':
      return Value(Type.INT, int(token))
    if token == InterpreterBase.TRUE_DEF or token == InterpreterBase.FALSE_DEF:
      return Value(Type.BOOL, token == InterpreterBase.TRUE_DEF)

    scope = self._where_defined(token)
    value = scope.get(token)
    return value

  # given a variable name and a Value object, associate the name with the value
  def _set_value(self, varname, value_type, scope):
    scope.set(varname,value_type)

  # evaluate expressions in prefix notation: + 5 * 6 x
  def _eval_expression(self, tokens):
    stack = []

    for token in reversed(tokens):
      if token in self.binary_op_list:
        v1 = stack.pop()
        v2 = stack.pop()
        if v1.type() != v2.type():
          super().error(ErrorType.TYPE_ERROR,f"Mismatching types {v1.type()} and {v2.type()}", self.ip) #!
        operations = self.binary_ops[v1.type()]
        if token not in operations:
          super().error(ErrorType.TYPE_ERROR,f"Operator {token} is not compatible with {v1.type()}", self.ip) #!
        stack.append(operations[token](v1,v2))
      elif token == '!':
        v1 = stack.pop()
        if v1.type() != Type.BOOL:
          super().error(ErrorType.TYPE_ERROR,f"Expecting boolean for ! {v1.type()}", self.ip) #!
        stack.append(Value(Type.BOOL, not v1.value()))
      else:
        value_type = self._get_value(token)
        stack.append(value_type)

    if len(stack) != 1:
      super().error(ErrorType.SYNTAX_ERROR,f"Invalid expression", self.ip) #no

    return stack[0]





program = [
"func main void",
"  var int x",
"  var string y",
"  var bool z",
"  assign x 42",
"  assign y \"foo\"",
"  assign z True ",
"  funccall foo x y z",
"  funccall print x \" \" y \" \" z",
"  funccall bletch",
"  funccall bar resulti",
"  funccall print resulti",
"endfunc",
"",
"func foo a:refint b:refstring c:refbool void",
" assign a -42",
" assign b \"bar\"",
" assign c False",
"endfunc",
"",
"func bletch int",
" return 100",
"endfunc",
"",
"func bar a:refint void",
" assign a -100 ",
"endfunc"
]


interpreter = Interpreter(trace_output=True)
interpreter.run(program)




