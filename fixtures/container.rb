# frozen_string_literal: true

require_relative "std/all"

module Container
  class Container
    attr_accessor :__0

    def initialize(__0)
      self.__0 = __0
    end

    def to_s
      "Container(#{__0})"
    end

    def ==(other)
      __0 == other.__0
    end

    def deconstruct
      [__0]
    end
  end

  def self.main
    contained_integer = Container.new(1)
    contained_string = Container.new("Oxiby")
    ::Std::Io.print_line(contained_integer)
    ::Std::Io.print_line(contained_string)
  end

  main
end