# frozen_string_literal: true

require_relative "std/all"

module TupleAccess
  class Two
    attr_accessor :__0, :__1

    def initialize(__0, __1)
      self.__0 = __0
      self.__1 = __1
    end

    def to_s
      "Two(#{__0}, #{__1})"
    end

    def ==(other)
      __0 == other.__0 && __1 == other.__1
    end

    def deconstruct
      [__0, __1]
    end
  end

  def self.main
    two = Two.new("apple", "banana")
    ::Std::Io.print_line(two.__0)
    ::Std::Io.print_line(two.__1)
  end

  main
end