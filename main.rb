#!/usr/bin/env ruby
# encoding: utf-8

require 'strscan'  # Para leitura e análise da string de entrada
require 'set'      # Para conjuntos de estados no parser
require 'json'     # Para serializar o resultado em JSON

# ======================================================
# Token e Lexer
# ======================================================

class Token
  attr_reader :type, :value
  def initialize(type, value)
    @type, @value = type, value
  end
end

class Lexer
  TOKEN_SPECS = [
    [:NEWLINE, /\r\n|\r|\n/],
    [:WS,      /[ \t]+/],                         # WS = whitespace (espaços e tabs)
    [:NUMBER,  /\b\d+(?:\.\d+)?\b/],
    [:BOOL,    /\b(?:true|false|null)\b/],
    [:STRING,  /"(?:\\"|[^"])*"|'(?:\\'|[^'])*'/],
    [:BAREWORD,/[\p{L}_][\p{L}\p{N}_-]*/u],
    [:COLON,   /:/],
    [:DOLLAR,  /\$/],
    [:PLUS,    /\+/],
    [:MINUS,   /-/],
    [:STAR,    /\*/],
    [:SLASH,   /\//],
    [:CARET,   /\^/],
    [:LPAREN,  /\(/],
    [:RPAREN,  /\)/],
  ]

  def initialize(input)
    @scanner = StringScanner.new(input)
  end

  def tokenize
    tokens = []
    until @scanner.eos?
      matched = false
      TOKEN_SPECS.each do |type, regex|
        if txt = @scanner.scan(regex)
          matched = true
          tokens << Token.new(type, txt) unless type == :WS
          break
        end
      end
      raise "Lexical error perto de '#{@scanner.peek(10)}'" unless matched
    end
    tokens
  end
end

# ======================================================
# Earley Parser
# ======================================================

Rule = Struct.new(:lhs, :rhs)

class State
  attr_reader :rule, :dot, :start, :children

  def initialize(rule, dot, start, children = [])
    @rule, @dot, @start, @children = rule, dot, start, children
  end

  def next_symbol;     rule.rhs[dot];    end
  def complete?;       dot >= rule.rhs.size; end

  # Avança o ponto de leitura e anexa child (se houver) em children
  def advance(child = nil)
    State.new(rule, dot + 1, start, children + [child])
  end

  def ==(other)
    rule == other.rule && dot == other.dot && start == other.start
  end
  alias eql? ==
  def hash; [rule, dot, start].hash; end
end

class EarleyParser
  def initialize(grammar, tokens)
    @grammar, @tokens = grammar, tokens
    @charts = Array.new(tokens.size + 1) { Set.new }
  end

  def parse
    # Estado inicial customizado: _S' → document
    @charts[0] << State.new(Rule.new("_S'", ["document"]), 0, 0)

    (0..@tokens.size).each do |i|
      loop do
        changed = false
        @charts[i].dup.each do |st|
          if !st.complete?
            sym = st.next_symbol
            changed ||= predict(sym, i)   if nonterm?(sym)
            changed ||= scan(st, i)      if term?(sym)
          else
            changed ||= complete(st, i)
          end
        end
        break unless changed
      end
    end

    final = @charts.last.find { |st|
      st.rule.lhs == "_S'" && st.complete? && st.start == 0
    }
    raise "Parse error: entrada inválida" unless final
    final
  end

  private

  def nonterm?(s); @grammar.any? { |r| r.lhs == s }; end
  def term?(s);    !nonterm?(s);              end

  def predict(sym, idx)
    added = false
    @grammar.select { |r| r.lhs == sym }.each do |r|
      ns = State.new(r, 0, idx)
      unless @charts[idx].include?(ns)
        @charts[idx] << ns
        added = true
      end
    end
    added
  end

  def scan(state, idx)
    return false if idx >= @tokens.size
    sym = state.next_symbol
    tok = @tokens[idx]
    if (sym.is_a?(String) && sym == tok.value) || sym.to_s == tok.type.to_s
      ns = state.advance(tok)
      unless @charts[idx + 1].include?(ns)
        @charts[idx + 1] << ns
        return true
      end
    end
    false
  end

  def complete(state, idx)
    added = false
    @charts[state.start].select { |st0|
      !st0.complete? && st0.next_symbol == state.rule.lhs
    }.each do |st0|
      ns = st0.advance(state)
      unless @charts[idx].include?(ns)
        @charts[idx] << ns
        added = true
      end
    end
    added
  end
end

# ======================================================
# ASTBuilder
# ======================================================

class ASTBuilder
  def initialize(root)
    @root = root
  end

  # Inicia em _S'; build_node já pula esse nível
  def build
    build_node(@root)
  end

  private

  def build_node(child)
    case child
    when Token
      case child.type
      when :NUMBER
        child.value.include?('.') ? child.value.to_f : child.value.to_i
      when :BOOL
        {'true'=>true,'false'=>false,'null'=>nil}[child.value]
      when :STRING
        child.value[1..-2]
      else
        child.value
      end

    when State
      if child.rule.lhs == "_S'"
        return build_node(child.children.first)
      end

      method = "node_#{child.rule.lhs}"
      unless respond_to?(method, true)
        raise "ASTBuilder não sabe lidar com regra '#{child.rule.lhs}'"
      end
      send(method, child)
    end
  end

  def node_document(state)
    # document → entry_list
    build_node(state.children.first)
  end

  def node_entry_list(state)
    # entry_list → entry rest
    first = build_node(state.children[0])
    rest  = node_rest(state.children[1])
    rest.inject(first) { |h,e| h.merge(e) }
  end

  def node_rest(state)
    # rest → NEWLINE entry rest | NEWLINE rest | ε
    return [] if state.children.empty?
    if state.children[1].is_a?(State) && state.children[1].rule.lhs == 'entry'
      [ build_node(state.children[1]) ] + node_rest(state.children[2])
    else
      node_rest(state.children[1])
    end
  end

  def node_entry(state)
    # entry → BAREWORD ':' value
    key = state.children[0].value
    val = build_node(state.children[2])
    { key => val }
  end

  def node_value(state)
    # value → CALC | SCALAR
    build_node(state.children.first)
  end

  def node_SCALAR(state)
    # SCALAR → NUMBER | BOOL | STRING | BAREWORD
    build_node(state.children.first)
  end

  def node_CALC(state)
    # CALC → '$' expr '$' ; avalia sem construir sub-AST
    expr_state = state.children[1]
    result     = Evaluator.evaluate_expr(expr_state)
    warns      = Evaluator.warnings.dup
    puts "AST: #{warns.inspect}" unless warns.empty?
    result
  end
end

# ======================================================
# Evaluator: avalia expr/term/factor recursivamente
# ======================================================

module Evaluator
  @warnings = []
  class << self
    attr_reader :warnings

    def evaluate_expr(state)
      acc = evaluate_term(state.children[0])
      evaluate_expr_tail(acc, state.children[1])
    end

    def evaluate_expr_tail(acc, tail)
      return acc if tail.children.empty?
      op_tok, term_state, next_tail = tail.children
      right = evaluate_term(term_state)
      op_name = op_tok.type == :PLUS ? 'soma' : 'diferenca'
      @warnings << [op_name, acc, right]
      res = (op_name == 'soma' ? acc + right : acc - right)
      evaluate_expr_tail(res, next_tail)
    end

    def evaluate_term(state)
      acc = evaluate_factor(state.children[0])
      evaluate_term_tail(acc, state.children[1])
    end

    def evaluate_term_tail(acc, tail)
      return acc if tail.children.empty?
      op_tok, factor_state, next_tail = tail.children
      right = evaluate_factor(factor_state)
      op_name = op_tok.type == :STAR ? 'multiplicacao' : 'divisao'
      @warnings << [op_name, acc, right]
      res = (op_name == 'multiplicacao' ? acc * right : acc.to_f / right)
      evaluate_term_tail(res, next_tail)
    end

    def evaluate_factor(state)
      acc = evaluate_primary(state.children[0])
      tail = state.children[1]
      return acc if tail.children.empty?
      op_tok, next_factor = tail.children
      right = evaluate_factor(next_factor)
      @warnings << ['potencia', acc, right]
      acc ** right
    end

    def evaluate_primary(state)
      child = state.children[0]
      if child.is_a?(Token) && child.type == :NUMBER
        child.value.include?('.') ? child.value.to_f : child.value.to_i
      else
        evaluate_expr(state.children[1])
      end
    end
  end
end

# ======================================================
# Fluxo principal: monta gramática, tokeniza, faz parse e imprime 
# ======================================================


grammar = [
Rule.new('document',    ['entry_list']),
Rule.new('entry_list',  ['entry','rest']),
Rule.new('rest',        ['NEWLINE','entry','rest']),
Rule.new('rest',        ['NEWLINE','rest']),
Rule.new('rest',        []),
Rule.new('entry',       ['BAREWORD','COLON','value']),
Rule.new('value',       ['CALC']),
Rule.new('value',       ['SCALAR']),
Rule.new('CALC',        ['DOLLAR','expr','DOLLAR']),
Rule.new('SCALAR',      ['NUMBER']),
Rule.new('SCALAR',      ['BOOL']),
Rule.new('SCALAR',      ['STRING']),
Rule.new('SCALAR',      ['BAREWORD']),
Rule.new('expr',        ['term','expr_tail']),
Rule.new('expr_tail',   ['PLUS','term','expr_tail']),
Rule.new('expr_tail',   ['MINUS','term','expr_tail']),
Rule.new('expr_tail',   []),
Rule.new('term',        ['factor','term_tail']),
Rule.new('term_tail',   ['STAR','factor','term_tail']),
Rule.new('term_tail',   ['SLASH','factor','term_tail']),
Rule.new('term_tail',   []),
Rule.new('factor',      ['primary','factor_tail']),
Rule.new('factor_tail', ['CARET','factor']),
Rule.new('factor_tail', []),
Rule.new('primary',     ['NUMBER']),
Rule.new('primary',     ['LPAREN','expr','RPAREN']),
]

input = if ARGV[0] && File.exist?(ARGV[0])
        File.read(ARGV[0])
      else
        <<~YAML
        nome: João
        idade: 30
        ativo: true
        conta: $9/3+21^1*10$
        YAML
      end

tokens = Lexer.new(input).tokenize
result = EarleyParser.new(grammar, tokens).parse
data   = ASTBuilder.new(result).build

puts "\nResultado:"
puts JSON.generate(data, ascii_only: false)

