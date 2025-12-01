# frozen_string_literal: true

require_relative "std/all"

module Types
  class S
    attr_accessor :a, :b

    def initialize(a:, b:)
      self.a = a
      self.b = b
    end

    def to_s
      "S { a: #{a}, b: #{b} }"
    end

    def ==(other)
      a == other.a && b == other.b
    end
    def deconstruct_keys(_keys)
      { a:, b: }
    end
  end

  class GenS
    attr_accessor :a, :b

    def initialize(a:, b:)
      self.a = a
      self.b = b
    end

    def to_s
      "GenS { a: #{a}, b: #{b} }"
    end

    def ==(other)
      a == other.a && b == other.b
    end
    def deconstruct_keys(_keys)
      { a:, b: }
    end
  end

  class E
    class U
      def to_s
        "U"
      end

      def ==(other)
        self.class == other.class
      end
    end

    class T
      attr_accessor :__0, :__1

      def initialize(__0, __1)
        self.__0 = __0
        self.__1 = __1
      end

      def to_s
        "T"
      end

      def ==(other)
        __0 == other.__0 && __1 == other.__1
      end

      def deconstruct
        [__0, __1]
      end
    end

    class S
      attr_accessor :c, :d
      def initialize(c:, d:)
        self.c = c
        self.d = d
      end

      def to_s
        "S"
      end

      def ==(other)
        c == other.c && d == other.d
      end

      def deconstruct_keys(_keys)
        { c:, d: }
      end
    end
  end

  class GenE
    class U
      def to_s
        "U"
      end

      def ==(other)
        self.class == other.class
      end
    end

    class T
      attr_accessor :__0, :__1

      def initialize(__0, __1)
        self.__0 = __0
        self.__1 = __1
      end

      def to_s
        "T"
      end

      def ==(other)
        __0 == other.__0 && __1 == other.__1
      end

      def deconstruct
        [__0, __1]
      end
    end

    class S
      attr_accessor :c, :d
      def initialize(c:, d:)
        self.c = c
        self.d = d
      end

      def to_s
        "S"
      end

      def ==(other)
        c == other.c && d == other.d
      end

      def deconstruct_keys(_keys)
        { c:, d: }
      end
    end
  end

  class S
    def self.test
      "test"
    end
  end

  def self.main
    s = S.allocate.tap { |__oxiby_new| __oxiby_new.send(:initialize, a: "a", b: "b") }
    unit = E::U.allocate
    tuple = E::T.allocate.tap { |__oxiby_new| __oxiby_new.send(:initialize, "1", "2") }
    strct = E::S.allocate.tap { |__oxiby_new| __oxiby_new.send(:initialize, c: "c", d: "d") }
    assoc = S.test
    x = case tuple
    in E::U
      "unit"
    in E::T(x, y)
      "tuple: 0=#{x}, 1=#{y}"
    in E::S(c:, d: x)
      "struct: c=#{c}, d=#{x}"
    end
    y = case tuple
    in E::U
      "unit"
    in E::T(x, y)
      "tuple: 0=#{x}, 1=#{y}"
    in E::S(c:, d: x)
      "struct: c=#{c}, d=#{x}"
    end
  end

  main
end