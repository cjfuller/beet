## Lexer

class BeetSyntaxError < StandardError
end

Tokens = ['keyword', 'id', 'eof', 'comma', 'num', 'other', 'string', 'sig-space', 'dot', 'newline']

Keywords = ['def', 'include', 'package', 'macro', 'binop']

Special_chars = [/\s+/, /(\()/, /(\))/, /({)/, /(})/, /(;)/, /(\.)/, /(,)/, /(#)/]

class Token

  def initialize(type, v)
    @type = type
    @v = v
  end

  def is_type?(type)
    @type == type
  end

  def is_any?(*types)
    types.include? @type
  end

  def to_s
    "<#{self.type} #{self.v}>"
  end

  attr_accessor :type, :v
end

def count_and_strip_leading_spaces(line)
  [line.strip, line.length - line.lstrip.length]
end

def ends_in_unescaped_quote?(str)
  /(?<!\\)(\\\\)*"\Z/.match str
end

def is_number(str)
  /\A\-?[0-9]+\.?[0-9]*\Z/.match str
end

def valid_identifier(str)
  /\A[\|\$\@\!\?\+\*\=\/\-\&\^\<\>\~\:_A-Za-z0-9]+\Z/.match str
end

def is_blank?(str)
  /\A\s*\Z/.match str
end

def get_token_type(str)
  if Keywords.include? str then
    :keyword
  elsif is_number str then
    :number
  elsif str == "," then
    :comma
  elsif str == "." then
    :dot
  elsif str.start_with? '"' then
    :string
  elsif valid_identifier(str) then
    :id
  else
    :other
  end
end

def make_token_helper(parts, head: nil, head_type: nil)
  if parts.empty?
    return nil
  end
  part = parts.shift
  if head_type == :string then
    if ends_in_unescaped_quote? part then
      [Token.new(:string, head + part), make_token_helper(parts)].flatten
    else
      make_token_helper(parts, head: head + part, head_type: head_type)
    end
  elsif part.start_with? '"' and not ends_in_unescaped_quote? part then
    make_token_helper(parts, head: part, head_type: :string)
  else
    [Token.new(get_token_type(part), part), make_token_helper(parts)].flatten
  end
end

def get_tokens(stream)
  return [Token.new(:eof, nil)] if stream.eof?
  line, n = count_and_strip_leading_spaces(stream.readline)
  if is_blank? line then
    return [Token.new(:newline, nil)]
  end
  parts = line.split /(".*?(?<!\\)(?:\\\\)*")/
  Special_chars.each do |c|
    parts = parts.map do |p|
      if p.start_with? '"'
        p
      else
        p.split(c)
      end
    end
    parts.flatten!
  end
  tokens = [Token.new(:sig_space, n), make_token_helper(parts)].flatten.delete_if { |e| e.nil? or (e.is_type? :other and e.v == "")}
  if i = (tokens.find_index { |e| (e.is_type? :other) and e.v == "#" }) then
    tokens = tokens[0...i]
  end
  tokens + [Token.new(:newline, nil)]
end

def tokenize(stream)
  all_tokens = []
  until (t = get_tokens(stream))[-1].is_type? :eof do
    all_tokens = all_tokens.concat t
  end
  all_tokens
end

################ Parser

module Parser

  IndentSize = 4

  InitialParserState = {line_number: 1,
    unmatched_paren_count: 0,
    indent_level: 0}

  ParserState = {line_number: 1,
    unmatched_paren_count: 0,
    indent_level: 0}

  BinaryOperators = %w[+ - / * ** == and or : = :=]

  class << self

    def curr_line
      ParserState[:line_number]
    end

    def increment_line
      ParserState[:line_number] += 1
    end

    def curr_indent
      ParserState[:indent_level]
    end

    def set_indent(level)
      ParserState[:indent_level] = level
    end

    def open_paren
      ParserState[:unmatched_paren_count] += 1
    end

    def close_paren
      ParserState[:unmatched_paren_count] -= 1
    end

    def unmatched_parens?
      ParserState[:unmatched_paren_count] > 0
    end

    def try_parse_special(tokens, index)
      nil
    end

    def whitespace_syntax_filter(tokens)
      last_indent = 0
      indent_size = 4

      output = []

      tokens.each do |t|
        if t.is_type? :sig_space then
          indent_diff = t.v - last_indent

          if indent_diff > 0 then
            (indent_diff / indent_size).times do |_|
              output << Token.new(:other, "{")
            end
          elsif indent_diff < 0 then
            (indent_diff / indent_size).abs.times do |_|
              output << Token.new(:other, "}")
            end
          end

          last_indent = t.v
        else
          output << t
        end
      end

      output
    end

    PreprocessorFilters = [Proc.new { |arg| Parser.whitespace_syntax_filter(arg) }]

    def do_preprocessing(tokens)
      PreprocessorFilters.each do |pf|
        tokens = pf.call(tokens)
      end
      tokens
    end

    def parse_next_expression(tokens, index)
      if node_with_count = try_parse_special(tokens, index) then
        node_with_count
      elsif index < tokens.length
        method_name = "parse_#{tokens[index].type.to_s}"
        
        
        if self.respond_to? method_name then
          result = send(method_name, tokens, index)
          
          result
        else
          raise BeetSyntaxError, "Unexpected #{tokens[index].inspect} at line #{curr_line}."
        end
      end
    end

    def parse_newline(tokens, index)
      increment_line
      if unmatched_parens?
        
        expr, n = parse_next_expression(tokens, index+1)
        [expr, n+1]
      else
        [nil, 1]
      end
    end

    def next_starts_function?(nxt)
      # id or number or string or block or parens next -> function call
      (nxt and (nxt.is_any?(:id, :number, :string) or 
        nxt.v == "{" or nxt.v == "("))
    end

    def maybe_parse_method_call(tokens, index, ast_node)
            
      if tokens[index].is_type? :dot
        msg, n = parse_next_expression(tokens, index + 1)
        [{
          type: :message, receiver: ast_node,
          message: msg
          }, n + 1]
      elsif tokens[index].is_type? :id and BinaryOperators.include? tokens[index].v
        
        args, n_args = parse_args(tokens, index + 1)
        bl, n_bl = parse_block(tokens, index + 1 + n_args)
        x = [{
          type: :message, receiver: ast_node,
          message: {type: :function_call, name: {type: :variable, name: tokens[index].v}, args: args, block: bl}
          }, 1 + n_args + n_bl]
          
          x
      else
        
        [ast_node, 0]
      end
    end

    def parse_sig_space(tokens, index)
      retval = [nil, 0]
      new_indent = tokens[index].v
      
      
      if new_indent < curr_indent then
        n_down = (curr_indent - new_indent)/IndentSize
        
        tokens[index..index] = (n_down.times.map { |i| Token.new(:other, "}") })
      elsif new_indent > curr_indent then
        n_up = (new_indent - curr_indent)/IndentSize
        tokens[index..index] = (n_up.times.map { |i| Token.new(:other, "{") })
        
      else
        retval = [nil, 1]
      end
      set_indent(new_indent)
      retval
    end

    def parse_id(tokens, index)
      t = tokens[index]
      ast_node = {type: :variable, name: t.v}
      
      count = 1
      ast_node, n_method = maybe_parse_method_call(tokens, index + 1, ast_node)
      if n_method > 0 then
        return [ast_node, count + n_method]
      end

      nxt = index < tokens.length - 1 ? tokens[index+count] : nil

      if next_starts_function? nxt then        
        args, n_eaten = parse_args(tokens, index + 1)
        bl, n_bl_eaten = parse_block(tokens, index + 1 + n_eaten)
        ast_node = {
          type: :function_call, name: ast_node,
          args: args, block: bl
        }
        count += n_eaten + n_bl_eaten
        nxt = index < tokens.length - 1 ? tokens[index+count] : nil
      end


      if nxt.is_type? :comma then
        # comma-grouped list
        ast_node, n_comma = parse_list(tokens, index + count, ast_node)
        [ast_node, n_comma + count]
      else
        [ast_node, count]
      end
    end

    def parse_string(tokens, index)
      ast_node = {type: :string, value: tokens[index].v}
      nxt = tokens[index + 1]
      if nxt.is_type? :comma then
        ast_node, n_comma = parse_list(tokens, index + 1, ast_node)
        [ast_node, n_comma + 1]
      else
        ast_node, n_method = maybe_parse_method_call(tokens, index + 1, ast_node)
        [ast_node, n_method + 1]
      end
    end

    def parse_number(tokens, index)
      val = tokens[index].v
      if val.include? "." then
        val = val.to_f
      else
        val = val.to_i
      end
      ast_node = {type: :number, value: val}
      nxt = tokens[index + 1]
      if nxt.is_type? :comma then
        ast_node, n_comma = parse_list(tokens, index + 1, ast_node)
        [ast_node, n_comma + 1]
      else
        ast_node, n_method = maybe_parse_method_call(tokens, index + 1, ast_node)
        [ast_node, n_method + 1]
      end
    end

    def parse_macro_def(tokens, index)
      # a macro def has a more rigid form: a name (id), open parens, a list of names in the expected syntax.  Note that these should be comma separated if it's expected that they'll be called comma separated, space separated if it's exected they'll be space separated, etc., close parens.  If you want something not to be a variable, use a single quote before the name.  If you want one of the arguments to be a block, then prepend an ampersand.  Finally, a block includes the macro body.  All expressions in the macro body are automatically quoted except for the unquoted parameters
      # Example, to define a standard if/else macro:
      # macro if(condition &ifbody 'else &elsebody)
      #     <macro body here>
    end

    def expand_macro(tokens, index)
      #macros are called with the exact syntax specified by the macro definition.  So for instance, in the if/else example, example is:
      # if(x == 3)
      #     print "It's 3!"
      # else
      #     print "Sadly, not 3.
    end

    def parse_args(tokens, index)
      if tokens[index].v == "(" then
        parse_group(tokens, index, bound: true)
      elsif tokens[index].is_any? :id, :number, :string then
        
        expr = parse_next_expression(tokens, index)
        
        expr
      else
        [nil, 0]
      end
    end

    def parse_other(tokens, index)
      if tokens[index].v == "(" then
        parse_group(tokens, index)
      elsif tokens[index].v == "{" then
        parse_block(tokens, index)
      elsif tokens[index].v == ";" then
        [nil, 1]
      elsif tokens[index].v == "" then
        expr, n = parse_next_expression(tokens, index+1)
        [expr, n + 1]
      else
        raise BeetSyntaxError, "Unexpected symbol #{tokens[index].v} at line #{curr_line}."
      end
    end

    def parse_group(tokens, index, bound: false)
      open_paren
      
      expr = nil
      n = 0
      if tokens[index + 1].v != ")" then
        expr, n = parse_next_expression(tokens, index + 1)
      end
      
      
      unless tokens[index + n + 1].v == ")" then
        raise BeetSyntaxError, "Expected close parenthesis after expression #{expr}.  Got #{tokens[index + n + 1].inspect}. At line: #{curr_line}." 
      end
      close_paren
      node = {type: :group, contents: expr}
      
      unless bound then
        
        node, n_method = maybe_parse_method_call(tokens, index + 2 + n, node)
        n += n_method
      end
      [node, n+2]
    end

    def parse_block(tokens, index)
      
      
      # block must start with {.  There may be a newline before that.

      cumulative_count = index
      while (cumulative_count < tokens.length and tokens[cumulative_count].is_type? :newline) do
        expr, n = parse_next_expression(tokens, cumulative_count)
        cumulative_count += n
      end
      
      

      if cumulative_count == tokens.length or tokens[cumulative_count].v != "{" then
        
        return [nil, 0]
      end

      
      
      exprs = []
      cumulative_count += 1 # eat the open bracket
      while tokens[cumulative_count].v != "}" do
        
        ast, n = parse_next_expression(tokens, cumulative_count)
        
        exprs << ast
        cumulative_count += n
        if cumulative_count == tokens.length then
          raise BeetSyntaxError, "Expected block close before EOF.  Block open was at line: #{curr_line}."
        end
      end
      
      [{type: :block, expressions: exprs}, cumulative_count + 1 - index]
    end

    def parse_list(tokens, index, first)
      rest, n = parse_next_expression(tokens, index + 1)
      [{type: :list, first: first, rest: rest}, n + 1]
    end

    def parse_keyword(tokens, index)
      case tokens[index].v
      when "def"
        parse_function_definition(tokens, index)
      when "package"
        parse_package_declaration(tokens, index)
      when "include"
        parse_include(tokens, index)
      when "macro"
        parse_macro_def(tokens, index)
      else
        raise BeetSyntaxError, "Unrecognized keyword #{tokens[index].v} at line #{curr_line}."
      end
    end

    def parse_function_definition(tokens, index)
      name = {type: :variable, name: tokens[index + 1].v}
      
      args, n_args = parse_args(tokens, index + 2)
      
      
      bl, n_bl = parse_block(tokens, index + 2 + n_args)
      

      x = [{type: :function_definition, name: name, args: args, body: bl}, 2 + n_args + n_bl]
      
      x
    end

    def parse_package_declaration(tokens, index)
      [{type: :package_declaration, package: tokens[index + 1].v}, 2]
    end

    def parse_include(tokens, index)
      [{type: :include, source: tokens[index + 1].v}, 2]
    end

    def parse_all(tokens)
      ParserState.merge! InitialParserState
      index = 0
      nodes = []
      while index < tokens.length do

        node, n = parse_next_expression(tokens, index)
        
        
        
        nodes << node
        index += n
      end
      nodes
    end

  end
end