# frozen_string_literal: true

require_relative "std/all"

module FnParamCombos
  def self.no_params
  end

  def self.one_pos(a)
  end

  def self.two_pos(a, b)
  end

  def self.one_kw(a:)
  end

  def self.two_kw(a:, b:)
  end

  def self.one_each(a, b:)
  end

  def self.two_each(a, b, c:, d:)
  end

  def self.main
    no_params
    one_pos("a")
    two_pos("a", "b")
    one_kw(a: "a")
    two_kw(a: "a", b: "b")
    one_each("a", b: "b")
    two_each("a", "b", c: "c", d: "d")
  end

  main
end