# frozen_string_literal: true

require_relative "std/all"

module Functions
  def self.null
  end

  def self.greet(name)
    if name == "Oxiby"
      return "Hello to myself!"
    end
    "Hello, #{name}!"
  end

  def self.identity(t)
    t
  end

  class Integer
    def add(other)
      self + other
    end
  end

  def self.add(a, b)
    a + b
  end

  def self.main
    null
    greet("Oxiby")
    identity(true)
    add(1, 2)
  end

  main
end