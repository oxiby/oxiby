# frozen_string_literal: true

require_relative "std/all"

module Tour
  require_relative "call"
  def self.predictable
    123
  end

  def self.add(a, b)
    a + b
  end

  def self.identity(a)
    a
  end

  def self.greet(name)
    if name == "Oxiby"
      ::Std::Io.print_line("Hello to myself!")
      return
    end
    ::Std::Io.print_line("Hello, #{name}")
  end

  class Person
    attr_accessor :name, :age, :gender

    def initialize(name:, age:, gender:)
      self.name = name
      self.age = age
      self.gender = gender
    end

    def to_s
      "Person { name: #{name}, age: #{age}, gender: #{gender} }"
    end

    def ==(other)
      name == other.name && age == other.age && gender == other.gender
    end
    def deconstruct_keys(_keys)
      { name:, age:, gender: }
    end
  end

  class Gender
    class Male
      def to_s
        "Male"
      end

      def ==(other)
        self.class == other.class
      end
    end

    class Female
      def to_s
        "Female"
      end

      def ==(other)
        self.class == other.class
      end
    end

    class NonBinary
      attr_accessor :__0

      def initialize(__0)
        self.__0 = __0
      end

      def to_s
        "NonBinary"
      end

      def ==(other)
        __0 == other.__0
      end

      def deconstruct
        [__0]
      end
    end
  end

  def self.main
    x = 5
    x = 3 + x
    x = x * -1
    x = (x)
    x = (x + 7) * 4
    x = ((x + 17) * 4) - 2
    ::Std::Io.print_line(x)
    s = "I'm a string!"
    ::Std::Io.print_line(s)
    ::Std::Io.print_line(predictable)
    ::Std::Io.print_line(identity(456))
    ::Std::Io.print_line(add(5, 10))
    greet("Oxiby")
    if true
      ::Std::Io.print_line("If without an else.")
    end
    if add(1, 2) == 3
      ::Std::Io.print_line("One and two is three!")
    else
      ::Std::Io.print_line("Something is seriously wrong.")
    end
    sum = add(1, 2)
    if sum == 5
      ::Std::Io.print_line("I'm off by two!")
    elsif sum == 4
      ::Std::Io.print_line("I'm off by one!")
    else
      ::Std::Io.print_line("I need to go back to school.")
    end
    if "foo" == "bar"
      ::Std::Io.print_line("nope")
    elsif "foo" == "baz"
      ::Std::Io.print_line("also nope")
    end
    fruits = ["apple", "banana", "carrot", "dragonfruit"]
    (fruits).each do |fruit|
      if fruit == "apple"
        next
      elsif fruit == "carrot"
        break
      end
      ::Std::Io.print_line(fruit)
    end
    alice = Person.new(name: ["Alice", "Henderson"], age: 42, gender: Gender::NonBinary.new("they/them"))
    bob = Person.new(name: ["Bob", "Sanders"], age: 67, gender: Gender::Male.new)
    ::Std::Io.print_line("#{alice.name[0]} #{alice.name[1]} is a #{alice.age} year old #{alice.gender}.")
    ::Std::Io.print_line("#{bob.name[0]} #{bob.name[1]} is a #{bob.age} year old #{bob.gender}.")
  end

  main
end