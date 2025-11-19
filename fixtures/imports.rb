# frozen_string_literal: true

require_relative "std/all"

module Imports
  require_relative "a"
  require_relative "b"
  require_relative "c"
  require_relative "d"
  require_relative "e"
  require_relative "f/g"
  require_relative "h/i"
  require_relative "j/k"
  require_relative "l/m"
  require_relative "n/o"
  def self.examples
    ::A.f
    ::B::S.f
    ::C::E::V.allocate
    ::D::E::U.new
    ::E::E::S.allocate.tap { |__oxiby_new| __oxiby_new.send(:initialize, f: 1) }
    ::F::G.f2
    ::H::I::S2.f
    ::J::K::E2::V.allocate
    ::L::M::E::U2.new
    ::N::O::E::S.new(1)
  end
end