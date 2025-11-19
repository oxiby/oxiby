# frozen_string_literal: true

require_relative "std/all"

module Patterns
  class Message
    class Quit
      def to_s
        "Quit"
      end

      def ==(other)
        self.class == other.class
      end
    end

    class Move
      attr_accessor :x, :y
      def initialize(x:, y:)
        self.x = x
        self.y = y
      end

      def to_s
        "Move"
      end

      def ==(other)
        x == other.x && y == other.y
      end

      def deconstruct_keys(_keys)
        { x:, y: }
      end
    end

    class Write
      attr_accessor :__0

      def initialize(__0)
        self.__0 = __0
      end

      def to_s
        "Write"
      end

      def ==(other)
        __0 == other.__0
      end

      def deconstruct
        [__0]
      end
    end

    class ChangeColor
      attr_accessor :__0, :__1, :__2

      def initialize(__0, __1, __2)
        self.__0 = __0
        self.__1 = __1
        self.__2 = __2
      end

      def to_s
        "ChangeColor"
      end

      def ==(other)
        __0 == other.__0 && __1 == other.__1 && __2 == other.__2
      end

      def deconstruct
        [__0, __1, __2]
      end
    end
  end

  def self.message
    message = [Message::Quit.new, Message::Move.new(x: 1, y: 2), Message::Write.new("hello"), Message::ChangeColor.new(4, 5, 6)].sample
    output = case message
    in Message::Quit
      "I quit!"
    in Message::Move(x:, y:)
      "Move #{x} by #{y}."
    in Message::Write(s)
      "Here's a message: #{s}."
    in Message::ChangeColor(r, g, b)
      "Red: #{r}, Green: #{g}, Blue: #{b}."
    end
    ::Std::Io.print_line(output)
  end

  def self.match_tuples
    case ::Std::Tuple::Tuple.new(fields: ["hello", "world"])
    in [greeting, "world"]
      ::Std::Io.print_line("Said #{greeting} to the world.")
    end
  end

  def self.let_tuples
    x, y = ::Std::Tuple::Tuple.new(fields: [1, 2])
    ::Std::Io.print_line("#{x} + #{y} = #{x + y}")
  end

  class Dog
    attr_accessor :name, :breed

    def initialize(name:, breed:)
      self.name = name
      self.breed = breed
    end

    def to_s
      "Dog { name: #{name}, breed: #{breed} }"
    end

    def ==(other)
      name == other.name && breed == other.breed
    end
    def deconstruct_keys(_keys)
      { name:, breed: }
    end
  end

  def self.structs
    dog = Dog.new(name: "Carl", breed: "Pug")
    dog => Dog(name:, breed:)
    ::Std::Io.print_line("#{name} is a #{breed}.")
  end

  def self.main
    message
    match_tuples
    let_tuples
    structs
  end

  main
end