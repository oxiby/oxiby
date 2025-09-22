# frozen_string_literal: true

require_relative "std/all"

module Lists
  def self.single_index
    ["hello"][0]
  end

  def self.double_index_across_multiple_lines
    ["hello"][0]["bar"]
  end

  def self.two_lists
    [["one"], ["two"]]
  end
end