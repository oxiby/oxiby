# frozen_string_literal: true

require_relative "std/all"

module Closures
  def self.main
    null = ->  {  }
    null.call
    implicit = -> (name) { "Hello, #{name}!" }
    ::Std::Io.print_line(implicit.call("Oxiby"))
    explicit = -> (name) { "Hello, #{name}!" }
    ::Std::Io.print_line(explicit.call("Oxiby"))
  end

  main
end