# frozen_string_literal: true

require_relative "std/all"

module Call
  def self.id(s)
    s
  end

  def self.main
    id("yo")
  end

  main
end