# frozen_string_literal: true

require_relative "std/all"

module Maps
  def self.main
    dogs = { "Carl" => "Pug", "Koda" => "Terrier" }
    (dogs).each do |pair|
      ::Std::Io.print_line("#{pair[0]} is a #{pair[1]}.")
    end
    (dogs).each do |key, value|
      ::Std::Io.print_line("#{key} is a #{value}.")
    end
  end

  main
end