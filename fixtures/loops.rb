# frozen_string_literal: true

require_relative "std/all"

module Loops
  def self.main
    n = 10
    while n > 0
      ::Std::Io.print_line(n)
      n = n - 1
    end
    loop do
      ::Std::Io.print_line(n)
      n = n + 1
      if n > 9
        break
      end
    end
    (["A", "B", "C"]).each do |item|
      ::Std::Io.print_line(item)
    end
    ({ "A" => 1, "B" => 2, "C" => 3 }).each do |key, value|
      ::Std::Io.print_line("#{key}: #{value}")
    end
  end

  main
end