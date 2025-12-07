# frozen_string_literal: true

require_relative "std/all"

module Structs
  class U
    def to_s
      "U"
    end

    def ==(other)
      self.class == other.class
    end

    def self.static
      "Static method on U"
    end

    def instance
      "Instance method on U"
    end
  end

  class T
    attr_accessor :__0

    def initialize(__0)
      self.__0 = __0
    end

    def to_s
      "T(#{__0})"
    end

    def ==(other)
      __0 == other.__0
    end

    def deconstruct
      [__0]
    end

    def self.static
      "Static method on T"
    end

    def instance
      "Instance method on T with value #{__0}"
    end
  end

  class S
    attr_accessor :a

    def initialize(a:)
      self.a = a
    end

    def to_s
      "S { a: #{a} }"
    end

    def ==(other)
      a == other.a
    end
    def deconstruct_keys(_keys)
      { a: }
    end

    def self.static
      "Static method on S"
    end

    def instance
      "Instance method on T with value #{a}"
    end
  end

  def self.main
    ::Std::Io.print_line(U.static)
    u = U.new
    ::Std::Io.print_line(u.instance)
    ::Std::Io.print_line(T.static)
    t = T.new("t")
    ::Std::Io.print_line(t.instance)
    ::Std::Io.print_line(S.static)
    s = S.allocate.tap { |__oxiby_new| __oxiby_new.send(:initialize, a: "a") }
    ::Std::Io.print_line(s.instance)
  end

  main
end