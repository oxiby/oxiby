# frozen_string_literal: true

require_relative "std/all"

module Conditional
  def self.thing
    false
  end

  def self.another_thing
    true
  end

  def self.main
    something = true
    if 3 * 3 == 9
      true
    end
    if something
      "a"
    else
      "b"
    end
    if thing
      "a"
    elsif another_thing
      "b"
    end
    if thing
      "a"
    elsif another_thing
      "b"
    else
      "c"
    end
    outer = true
    if outer
      if 3 * 3 == 9
        true
      end
      if something
        "a"
      else
        "b"
      end
      if thing
        "a"
      elsif another_thing
        "b"
      end
      if thing
        "a"
      elsif another_thing
        "b"
      else
        "c"
      end
    end
  end

  main
end