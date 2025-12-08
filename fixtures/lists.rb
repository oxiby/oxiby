# frozen_string_literal: true

require_relative "std/all"

module Lists
  def self.single_index
    ["hello"][0]
  end

  def self.double_index_across_multiple_lines
    [["hello"]][0][0]
  end

  def self.two_lists
    ::Std::Tuple::Tuple.new(fields: [["one"], ["two"]])
    ::Std::Tuple::Tuple.new(fields: [])
  end
end