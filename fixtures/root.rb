# frozen_string_literal: true

require_relative "std/all"

module Root
  require_relative "a"
  require_relative "a/b"
  require_relative "c/d"
  def self.main
    ::Std::Io.print_line(::A.foo)
    ::Std::Io.print_line(::A::B.bar)
    ::Std::Io.print_line(::C::D.baz)
  end

  main
end