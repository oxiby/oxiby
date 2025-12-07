# frozen_string_literal: true

require_relative "std/all"

module Enums
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
      attr_accessor :__0

      def initialize(__0)
        self.__0 = __0
      end

      def to_s
        "T"
      end

      def ==(other)
        __0 == other.__0
      end

      def deconstruct
        [__0]
      end
    end

    class S
      attr_accessor :a
      def initialize(a:)
        self.a = a
      end

      def to_s
        "S"
      end

      def ==(other)
        a == other.a
      end

      def deconstruct_keys(_keys)
        { a: }
      end
    end
  end

  def self.main
    u = E::U.allocate
    t = E::T.allocate.tap { |__oxiby_new| __oxiby_new.send(:initialize, "T") }
    s = E::S.allocate.tap { |__oxiby_new| __oxiby_new.send(:initialize, a: "S") }
  end

  main
end