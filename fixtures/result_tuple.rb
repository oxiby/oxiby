# frozen_string_literal: true

require_relative "std/all"

module ResultTuple
  def self.result_tuple
    ::Std::Result::Result::Ok.new([])
  end

  def self.main
    case result_tuple
    in ::Std::Result::Result::Ok
      ::Std::Io.print_line("ok")
    in ::Std::Result::Result::Err
      print_link("err")
    end
  end

  main
end